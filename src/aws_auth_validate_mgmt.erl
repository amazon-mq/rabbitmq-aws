%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Cowboy REST handler that exposes
%%   PUT /api/aws/auth/validate/:method
%% Implements the rabbit_mgmt_extension behaviour so the route is
%% mounted automatically by the management plugin. Each request flows
%% through a fixed pipeline: feature toggle -> management auth -> user
%% tag gate -> body size cap -> JSON decode -> concurrency semaphore ->
%% registry dispatch -> audit log -> response.
-module(aws_auth_validate_mgmt).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

-export([
    init/2,
    content_types_accepted/2,
    allowed_methods/2,
    resource_exists/2,
    is_authorized/2,
    accept_content/2
]).

-ifdef(TEST).
%% Exposed for unit tests: status_for_category/1 maps a backend error category
%% to an HTTP status, including the catch-all for an unexpected category;
%% max_body_size/0 resolves the configured body-size limit against its bounds;
%% sanitize_log/1 strips control chars from a caller-influenceable audit field;
%% username/1 extracts the authenticated principal (or the `unknown' fallback).
-export([status_for_category/1, max_body_size/0, sanitize_log/1, username/1]).
-endif.

-include_lib("rabbitmq_web_dispatch/include/rabbitmq_web_dispatch_records.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include("aws.hrl").

-define(DEFAULT_MAX_BODY_SIZE, 65_536).
-define(DEFAULT_REQUIRED_USER_TAG, administrator).

dispatcher() -> [{"/aws/auth/validate/:method", ?MODULE, []}].

%% Register the management-console UI extension. The management plugin serves
%% this plugin's priv/www/ automatically (see rabbit_mgmt_dispatcher). Two files
%% load, in order: aws-ejs.js first (the build-time-precompiled EJS templates,
%% populating COMPILED_TEMPLATES -- the UI no longer compiles .ejs at runtime),
%% then aws_auth_validate.js, which adds an admin-gated "Auth Validation" tab
%% that drives PUT /aws/auth/validate/:method and renders those templates.
web_ui() -> [{javascript, [<<"aws-ejs.js">>, <<"aws_auth_validate.js">>]}].

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------

init(Req, _Opts) ->
    {cowboy_rest, rabbit_mgmt_cors:set_headers(Req, ?MODULE), #context{}}.

content_types_accepted(ReqData, Context) ->
    {[{'*', accept_content}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"PUT">>, <<"OPTIONS">>], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {feature_enabled(), ReqData, Context}.

is_authorized(ReqData, Context) ->
    case required_user_tag() of
        administrator ->
            rabbit_mgmt_util:is_authorized_admin(ReqData, Context);
        Tag ->
            case rabbit_mgmt_util:is_authorized(ReqData, Context) of
                {true, ReqData1, Context1} ->
                    authorize_tag(Tag, ReqData1, Context1);
                Other ->
                    Other
            end
    end.

%%--------------------------------------------------------------------
%% Request pipeline
%%--------------------------------------------------------------------

accept_content(Req0, Context) ->
    %% Enforce the feature toggle here, before touching any worker. For a PUT,
    %% cowboy_rest routes resource_exists/2 -> false into the create path
    %% (accept_content), NOT a 404 -- so resource_exists alone does not gate
    %% the endpoint. When the feature is disabled, aws_sup starts no
    %% semaphore worker, so reaching the pipeline below would gen_server:call
    %% a non-existent process and crash (HTTP 500). Short-circuit to 404
    %% instead, matching the documented toggle behaviour.
    case feature_enabled() of
        false ->
            reply_error(404, method_disabled, <<"Validation method disabled">>, Req0, Context);
        true ->
            T0 = erlang:monotonic_time(millisecond),
            SourceIP = peer_ip(Req0),
            User = username(Context),
            Method = cowboy_req:binding(method, Req0),
            with_body(T0, SourceIP, User, Method, Req0, Context)
    end.

with_body(T0, SourceIP, User, Method, Req0, Context) ->
    MaxBytes = max_body_size(),
    case read_body(Req0, MaxBytes) of
        {error, body_too_large, Req1} ->
            audit(Method, SourceIP, User, body_too_large, T0),
            reply_error(400, body_too_large, <<"Request body too large">>, Req1, Context);
        {ok, RawBody, Req1} ->
            case decode_json(RawBody) of
                {error, _} ->
                    audit(Method, SourceIP, User, input_invalid, T0),
                    reply_error(400, input_invalid, <<"Invalid JSON body">>, Req1, Context);
                {ok, BodyMap} when is_map(BodyMap) ->
                    with_semaphore(T0, SourceIP, User, Method, BodyMap, Req1, Context);
                {ok, _NotMap} ->
                    audit(Method, SourceIP, User, input_invalid, T0),
                    reply_error(
                        400, input_invalid, <<"JSON body must be an object">>, Req1, Context
                    )
            end
    end.

with_semaphore(T0, SourceIP, User, Method, BodyMap, Req, Context) ->
    %% acquire/0 is a gen_server:call to the semaphore worker. The worker is
    %% started by aws_sup only at boot when the feature is enabled; if the env
    %% was flipped to true at runtime (no restart) or the supervisor gave up on
    %% the worker, the call exits {noproc, _}. Catch exactly that here -- the
    %% atomic acquire IS the liveness check, so there is no time-of-check vs
    %% time-of-use window -- and map it to the same graceful 503 as a full
    %% semaphore. Any other exit propagates (it is a genuine fault).
    case try_acquire() of
        not_ready ->
            audit(Method, SourceIP, User, capacity_exhausted, T0),
            reply_error(
                503,
                capacity_exhausted,
                <<"Validation service is not ready; broker restart required">>,
                Req,
                Context
            );
        {error, full} ->
            audit(Method, SourceIP, User, capacity_exhausted, T0),
            reply_error(503, capacity_exhausted, <<"Service at capacity">>, Req, Context);
        {ok, Ref} ->
            try
                %% Defense in depth -- no credential leakage: a backend must
                %% always *return* a fixed-category result, but if one ever
                %% raises, the escaping exception would carry BodyMap (and any
                %% future secret it holds) into a Cowboy crash report. Catch
                %% here, discarding the class/reason/stacktrace so no request
                %% term is logged.
                %%
                %% A raise here is OUR fault, not the caller's: a real
                %% unreachable server is *returned* as connection_failed by the
                %% backend (which has its own no-credential-leakage try/catch),
                %% so reaching this clause means an unexpected internal error.
                %% Report it as a 500 rather than a 400 connection_failed, which
                %% would wrongly tell the caller their LDAP server is unreachable.
                Result = aws_auth_validate_registry:dispatch(Method, BodyMap),
                audit(Method, SourceIP, User, result_category(Result), T0),
                respond(Result, Req, Context)
            catch
                _Class:_Reason:_Stack ->
                    audit(Method, SourceIP, User, internal_error, T0),
                    reply_error(
                        500,
                        internal_error,
                        <<"Internal error during validation">>,
                        Req,
                        Context
                    )
            after
                aws_auth_validate_semaphore:release(Ref)
            end
    end.

%% Acquire a semaphore slot, treating an absent worker as `not_ready' rather
%% than letting the noproc exit escape. This is the single point of contact
%% with the worker, so the acquire doubles as the liveness check -- no
%% separate whereis/check beforehand, hence no TOCTOU window. Only a noproc
%% (worker not registered / dead) is converted; every other exit propagates.
try_acquire() ->
    try
        aws_auth_validate_semaphore:acquire()
    catch
        exit:{noproc, _} -> not_ready
    end.

%%--------------------------------------------------------------------
%% Response mapping
%%--------------------------------------------------------------------

respond(ok, Req, Context) ->
    Req1 = cowboy_req:reply(204, #{}, <<>>, Req),
    {stop, Req1, Context};
respond({error, unknown_method}, Req, Context) ->
    reply_error(404, unknown_method, <<"Unknown validation method">>, Req, Context);
respond({error, method_disabled}, Req, Context) ->
    reply_error(404, method_disabled, <<"Validation method disabled">>, Req, Context);
respond({error, Category, Reason}, Req, Context) when is_atom(Category), is_binary(Reason) ->
    Status = status_for_category(Category),
    reply_error(Status, Category, Reason, Req, Context).

%% The domain is any atom, not just aws_auth_validate_backend:error_category().
%% body_too_large is raised by this module (read_body/2) rather than by a
%% backend, and the final clause exists precisely for atoms outside the
%% documented set.
%%
%% Both of those clauses are reachable only through the -ifdef(TEST) export
%% above, and `make dialyze' compiles without TEST, so dialyzer sees a local
%% function and infers its domain from the sole production call site in
%% respond/3 -- which passes only error_category(). That makes the two clauses
%% look unreachable even though the eunit suite calls them directly. The spec
%% below documents the real domain; it cannot silence the warning, because
%% dialyzer prefers the inferred success typing of a local function.
-dialyzer({no_match, status_for_category/1}).
-spec status_for_category(atom()) -> 400 | 422 | 500.
status_for_category(input_invalid) -> 400;
status_for_category(body_too_large) -> 400;
status_for_category(connection_failed) -> 400;
status_for_category(tls_failed) -> 400;
status_for_category(query_invalid) -> 400;
status_for_category(auth_failed) -> 422;
status_for_category(config_conflict) -> 422;
status_for_category(authz_unverified) -> 422;
%% Token-verification refinements of auth_failed; same 422 status, distinct
%% category so callers can branch transient-vs-config without parsing message.
status_for_category(token_expired) -> 422;
status_for_category(token_invalid) -> 422;
%% A category outside the backend behaviour's documented set is our fault, not
%% the caller's, so map it to 500 rather than crashing with a function_clause
%% (which the with_semaphore/6 catch would also surface as a 500).
status_for_category(_) -> 500.

reply_error(Status, Category, Message, Req, Context) ->
    Body = rabbit_json:encode(#{
        error => atom_to_binary(Category, utf8),
        message => Message
    }),
    Headers = #{<<"content-type">> => <<"application/json">>},
    Req1 = cowboy_req:reply(Status, Headers, Body, Req),
    {stop, Req1, Context}.

result_category(ok) -> success;
result_category({error, unknown_method}) -> unknown_method;
result_category({error, method_disabled}) -> method_disabled;
result_category({error, Category, _Reason}) -> Category.

%%--------------------------------------------------------------------
%% Authorization helpers
%%--------------------------------------------------------------------

%% Operator-configured non-administrator tag gating. The default
%% (administrator) is handled in is_authorized/2 via the management
%% plugin's helper, which already handles oauth tokens etc.
%%
%% An OPTIONS preflight reaches is_authorized/2 too, but rabbit_mgmt_util
%% short-circuits OPTIONS to {true, _, Context} WITHOUT authenticating, so
%% Context#context.user is `undefined'. Let that case through (the admin
%% path does the same) so a CORS preflight is not rejected with a spurious
%% 401 -- there is no body to act on, and the actual PUT is still gated.
authorize_tag(_Tag, ReqData, #context{user = undefined} = Context) ->
    {true, ReqData, Context};
authorize_tag(Tag, ReqData, #context{user = #user{tags = Tags}} = Context) ->
    case lists:member(Tag, Tags) of
        true -> {true, ReqData, Context};
        false -> not_authorised(ReqData, Context)
    end;
authorize_tag(_Tag, ReqData, Context) ->
    not_authorised(ReqData, Context).

not_authorised(ReqData, Context) ->
    Body = rabbit_json:encode(#{
        error => <<"insufficient_user_tag">>,
        message => <<"User does not have required tag">>
    }),
    Headers = #{<<"content-type">> => <<"application/json">>},
    Req1 = cowboy_req:reply(401, Headers, Body, ReqData),
    {stop, Req1, Context}.

%%--------------------------------------------------------------------
%% Request helpers
%%--------------------------------------------------------------------

%% The fallback clause is unreachable per cowboy_req:peer/1's own spec, which is
%% why dialyzer flags it, but it is kept deliberately: peer_ip/1 runs BEFORE the
%% try in with_semaphore/6, so a function_clause here would surface as an
%% unhandled 500 on an otherwise valid request. Falling back to 0.0.0.0 keeps
%% the request serviceable and still produces an audit line.
-dialyzer({no_match, peer_ip/1}).
peer_ip(Req) ->
    case cowboy_req:peer(Req) of
        {IP, _Port} -> IP;
        _ -> {0, 0, 0, 0}
    end.

read_body(Req0, MaxBytes) ->
    Opts = #{length => MaxBytes + 1, period => 5_000},
    case cowboy_req:read_body(Req0, Opts) of
        {ok, Body, Req1} when byte_size(Body) =< MaxBytes ->
            {ok, Body, Req1};
        {ok, _Body, Req1} ->
            {error, body_too_large, Req1};
        {more, _Body, Req1} ->
            {error, body_too_large, Req1}
    end.

decode_json(<<>>) ->
    {ok, #{}};
decode_json(Raw) ->
    rabbit_json:try_decode(Raw).

audit(Method, SourceIP, User, ResultCategory, T0) ->
    Duration = erlang:monotonic_time(millisecond) - T0,
    ?AWS_LOG_INFO(
        "auth_validate: method=~ts user=~ts source_ip=~ts result=~ts duration_ms=~B",
        [sanitize_log(Method), sanitize_log(User), format_ip(SourceIP), ResultCategory, Duration]
    ).

%% Strip control characters (CR, LF, TAB, any other byte < 0x20, and DEL) from a
%% value before it is interpolated into the single-line audit record. Method
%% comes from the URL path binding and User from the stored username -- both are
%% caller-influenceable, so an embedded CR/LF could otherwise forge or split an
%% audit line (log injection), defeating the attribution this record exists to
%% provide. Each offending byte is replaced with the Unicode replacement
%% character (U+FFFD) so the record stays on one line and the tampering is
%% visible rather than silently dropped. UTF-8 multi-byte sequences are
%% preserved untouched (every one of their bytes is >= 0x80).
sanitize_log(Bin) when is_binary(Bin) ->
    <<<<(sanitize_byte(B))/binary>> || <<B>> <= Bin>>;
sanitize_log(Other) ->
    Other.

sanitize_byte(B) when B < 16#20; B =:= 16#7F ->
    <<"\x{fffd}"/utf8>>;
sanitize_byte(B) ->
    <<B>>.

%% The authenticated management user's name, for the audit trail. This endpoint
%% is admin-gated and makes outbound connections to caller-chosen targets (an
%% SSRF surface), so the acting principal -- not just the TCP peer IP, which
%% behind the management load balancer is often the LB -- must be attributable.
%% is_authorized/2 populates Context#context.user before accept_content/2 runs;
%% an OPTIONS preflight is let through unauthenticated (user=undefined), but the
%% feature toggle short-circuits OPTIONS before any audit, so a `<<"unknown">>'
%% here only ever reflects an unexpected shape, never a real unaudited PUT. Only
%% the username is logged -- never tags, target host, URL, or DN (to prevent
%% information disclosure and ensure resolved secrets never reach logs).
username(#context{user = #user{username = Name}}) when is_binary(Name) ->
    Name;
username(_) ->
    <<"unknown">>.

%% As with peer_ip/1, the non-tuple clause is unreachable given the inferred
%% argument type but is kept so that a malformed address can never turn an audit
%% log write into a crash. Audit logging must not be able to fail the request it
%% is recording.
-dialyzer({no_match, format_ip/1}).
format_ip(IP) when is_tuple(IP) ->
    case inet:ntoa(IP) of
        {error, _} -> <<"unknown">>;
        Str -> list_to_binary(Str)
    end;
format_ip(_) ->
    <<"unknown">>.

%%--------------------------------------------------------------------
%% Configuration accessors
%%--------------------------------------------------------------------

feature_enabled() ->
    application:get_env(aws, auth_validation_enabled, false) =:= true.

max_body_size() ->
    case application:get_env(aws, auth_validation_max_body_size) of
        {ok, N} when is_integer(N), N > 0, N =< 1_048_576 -> N;
        _ -> ?DEFAULT_MAX_BODY_SIZE
    end.

required_user_tag() ->
    case application:get_env(aws, auth_validation_required_user_tag) of
        {ok, Tag} when is_atom(Tag) -> Tag;
        _ -> ?DEFAULT_REQUIRED_USER_TAG
    end.
