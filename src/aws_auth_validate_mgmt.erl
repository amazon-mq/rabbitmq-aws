%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Cowboy REST handler that exposes
%%   PUT /api/aws/auth/validate/:method
%% Implements the rabbit_mgmt_extension behaviour so the route is
%% mounted automatically by the management plugin. Each request flows
%% through a fixed pipeline: feature toggle -> management auth -> user
%% tag gate -> per-IP rate limit -> body size cap -> JSON decode ->
%% concurrency semaphore -> registry dispatch -> audit log -> response.
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

-include_lib("rabbitmq_web_dispatch/include/rabbitmq_web_dispatch_records.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include("aws.hrl").

-define(DEFAULT_MAX_BODY_SIZE, 65_536).
-define(DEFAULT_REQUIRED_USER_TAG, administrator).

dispatcher() -> [{"/aws/auth/validate/:method", ?MODULE, []}].

web_ui() -> [].

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
    T0 = erlang:monotonic_time(millisecond),
    SourceIP = peer_ip(Req0),
    Method = cowboy_req:binding(method, Req0),
    case aws_auth_validate_rate_limiter:check(SourceIP) of
        {error, rate_limited} ->
            audit(Method, SourceIP, rate_limited, T0),
            reply_error(429, rate_limited, <<"Rate limit exceeded">>, Req0, Context);
        ok ->
            with_body(T0, SourceIP, Method, Req0, Context)
    end.

with_body(T0, SourceIP, Method, Req0, Context) ->
    MaxBytes = max_body_size(),
    case read_body(Req0, MaxBytes) of
        {error, body_too_large, Req1} ->
            audit(Method, SourceIP, input_invalid, T0),
            reply_error(400, body_too_large, <<"Request body too large">>, Req1, Context);
        {ok, RawBody, Req1} ->
            case decode_json(RawBody) of
                {error, _} ->
                    audit(Method, SourceIP, input_invalid, T0),
                    reply_error(400, input_invalid, <<"Invalid JSON body">>, Req1, Context);
                {ok, BodyMap} when is_map(BodyMap) ->
                    with_semaphore(T0, SourceIP, Method, BodyMap, Req1, Context);
                {ok, _NotMap} ->
                    audit(Method, SourceIP, input_invalid, T0),
                    reply_error(400, input_invalid, <<"JSON body must be an object">>, Req1, Context)
            end
    end.

with_semaphore(T0, SourceIP, Method, BodyMap, Req, Context) ->
    case aws_auth_validate_semaphore:acquire() of
        {error, full} ->
            audit(Method, SourceIP, capacity_exhausted, T0),
            reply_error(503, capacity_exhausted, <<"Service at capacity">>, Req, Context);
        {ok, Ref} ->
            try
                Result = aws_auth_validate_registry:dispatch(Method, BodyMap),
                audit(Method, SourceIP, result_category(Result), T0),
                respond(Result, Req, Context)
            after
                aws_auth_validate_semaphore:release(Ref)
            end
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

status_for_category(input_invalid) -> 400;
status_for_category(connection_failed) -> 400;
status_for_category(tls_failed) -> 400;
status_for_category(query_invalid) -> 400;
status_for_category(auth_failed) -> 422;
status_for_category(config_conflict) -> 422;
status_for_category(authz_unverified) -> 422.

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

audit(Method, SourceIP, ResultCategory, T0) ->
    Duration = erlang:monotonic_time(millisecond) - T0,
    ?AWS_LOG_INFO(
        "auth_validate: method=~ts source_ip=~ts result=~ts duration_ms=~B",
        [Method, format_ip(SourceIP), ResultCategory, Duration]
    ).

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
        {ok, N} when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_MAX_BODY_SIZE
    end.

required_user_tag() ->
    case application:get_env(aws, auth_validation_required_user_tag) of
        {ok, Tag} when is_atom(Tag) -> Tag;
        _ -> ?DEFAULT_REQUIRED_USER_TAG
    end.
