%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% LDAP simple-bind validation backend.
%%
%% Opens an ephemeral connection per request, optionally performs
%% start_tls, attempts a simple bind, then unconditionally closes the
%% handle. All outcomes collapse to a fixed set of error categories so
%% the HTTP response cannot be used to extract LDAP-server or
%% network-topology details. Credentials never appear in returned
%% reasons.
%%
%% Beyond authentication, the backend optionally validates two further
%% layers of an LDAP configuration, reusing the same bound connection:
%%
%%   * DN lookup -- when `dn_lookup_base' is supplied, confirms the base
%%     DN exists and is readable by the bound user. `dn_lookup_attribute'
%%     is checked for syntactic validity only: it is NOT used in a live
%%     username->DN search, because the endpoint has no principal to look up
%%     and running one could leak directory contents. CONSEQUENCE: a request
%%     with a correct dn_lookup_base but a wrong dn_lookup_attribute still
%%     returns `ok'. An `ok' here means "the base is reachable", not "this
%%     attribute resolves a user" -- a distinction that must be reflected in
%%     any user-facing documentation so customers do not over-trust the
%%     result.
%%
%%   * Authorization queries -- `queries.{tags,vhost_access,
%%     resource_access,topic_access}' are parsed with the same grammar
%%     the broker uses (aws_auth_validate_ldap_query), and any group/DN
%%     referenced with a fully literal DN (no ${...} runtime placeholder)
%%     is checked for existence and readability. Query terms that need a
%%     runtime principal (e.g. ${username}) are parsed but not evaluated,
%%     because the endpoint has no live login to evaluate them against and
%%     must not leak directory contents.
%%
%% Parse failures map to `query_invalid' (400); a referenced object that
%% cannot be verified maps to `authz_unverified' (422). Both carry fixed
%% constant messages -- no DN, group name, or raw LDAP detail is echoed.
-module(aws_auth_validate_ldap).

-behaviour(aws_auth_validate_backend).

-export([method_name/0, validate/1, allowed_fields/0]).

-ifdef(TEST).
%% Exposed for unit tests: build_ssl_opts/1 translates validated ssl_options
%% to the eldap proplist; build_tls_opts/2 layers the shared verify-mode policy
%% on top (returning the fail-loud {ok,_}|{error,tls_failed,_} contract);
%% peer_allowed/1 is the post-connect SSRF re-check on a peername result;
%% is_denied_ip/1 is the SSRF address classifier for v4 and v6 (delegating to the
%% shared aws_auth_validate_net:classify_ip/2 with LDAP's denylist);
%% search_time_limit_seconds/0 is the post-bind search timeLimit. All are
%% otherwise internal.
-export([
    build_ssl_opts/1,
    build_tls_opts/2,
    is_allowed_server/1,
    peer_allowed/1,
    is_denied_ip/1,
    parse_port/2,
    search_time_limit_seconds/0
]).
-endif.

-include_lib("eldap/include/eldap.hrl").

-define(DEFAULT_TIMEOUT_MS, 5_000).

%% Default LDAP port when the request omits `port'. Mirrors the broker's
%% rabbit_auth_backend_ldap default (application env `port', 389 -- the broker
%% does NOT bump to 636 for TLS), so a config that omits port validates the same
%% way the live broker would connect rather than being rejected input_invalid.
-define(DEFAULT_LDAP_PORT, 389).

%% Accepted query names within the `queries' object. Mirrors the broker's
%% auth_ldap.queries.* config keys.
-define(QUERY_NAMES, [
    <<"tags">>,
    <<"vhost_access">>,
    <<"resource_access">>,
    <<"topic_access">>
]).

%% Accepted keys within the `ssl_options' object. A request carrying any
%% other key is rejected (rather than silently dropped) so a mis-typed
%% option cannot make validation pass green without testing the option the
%% customer intended.
-define(SSL_OPTION_KEYS, [
    <<"cacertfile_arn">>,
    <<"verify">>,
    <<"depth">>,
    <<"versions">>,
    <<"server_name_indication">>,
    <<"hostname_verification">>
]).

%% Accepted values for `hostname_verification'. Mirrors the broker's
%% auth_ldap.ssl_options.hostname_verification enum ({wildcard, none}); the
%% broker's default (key unset) is strict OTP matching.
-define(SSL_HOSTNAME_VERIFICATION_VALUES, [<<"wildcard">>, <<"none">>]).

%% Accepted values for the `verify' ssl option. Mirrors the ssl app's two
%% modes; anything else is a mis-typed value and rejected up front rather
%% than silently dropped and re-defaulted.
-define(SSL_VERIFY_VALUES, [<<"verify_peer">>, <<"verify_none">>]).

%% Accepted values for the `versions' ssl option. Mirrors the TLS versions
%% the ssl app understands.
-define(SSL_VERSION_VALUES, [
    <<"tlsv1.3">>,
    <<"tlsv1.2">>,
    <<"tlsv1.1">>,
    <<"tlsv1">>
]).

-define(REASON_BAD_SERVERS, <<"servers must be a non-empty list of non-empty strings">>).
-define(REASON_BLOCKED_SERVER,
    <<"one or more server addresses resolve to a blocked network range">>
).
-define(REASON_BAD_PORT, <<"port must be an integer in 1..65535">>).
-define(REASON_BAD_USER_DN, <<"user_dn must be a non-empty string">>).
-define(REASON_BAD_PASSWORD_ARN, <<"password_arn must be a non-empty string">>).
-define(REASON_BAD_SSL_FLAG, <<"use_ssl must be a boolean">>).
-define(REASON_BAD_STARTTLS_FLAG, <<"use_starttls must be a boolean">>).
-define(REASON_BAD_SSL_OPTIONS, <<"ssl_options must be an object">>).
-define(REASON_UNKNOWN_SSL_OPTION, <<
    "ssl_options contains an unknown key; allowed keys are cacertfile_arn, "
    "verify, depth, versions, server_name_indication, hostname_verification"
>>).
-define(REASON_BAD_SSL_VERIFY, <<"ssl_options.verify must be verify_peer or verify_none">>).
-define(REASON_BAD_SSL_DEPTH, <<"ssl_options.depth must be a non-negative integer">>).
-define(REASON_BAD_SSL_VERSIONS, <<"ssl_options.versions must be a list of known TLS versions">>).
-define(REASON_BAD_SSL_SNI, <<"ssl_options.server_name_indication must be a string">>).
-define(REASON_BAD_SSL_HOSTNAME_VERIFICATION,
    <<"ssl_options.hostname_verification must be wildcard or none">>
).
-define(REASON_BAD_SSL_CACERT_ARN, <<"ssl_options.cacertfile_arn must be a non-empty string">>).
%% ARN-RESOLUTION failures no longer have a per-field macro here: the failing
%% fields are collected across both ARNs and rendered by
%% aws_auth_validate_ssl:arn_resolve_reason/1 so one response can name them all.
%% The PEM macro below is for CONTENT failures, where the ARN did resolve.
-define(REASON_CACERT_PEM_INVALID,
    <<"ssl_options.cacertfile_arn did not resolve to a valid PEM certificate">>
).
-define(REASON_TLS_BOTH, <<"use_ssl and use_starttls are mutually exclusive">>).
-define(REASON_CONNECTION, <<"could not connect to LDAP server">>).
-define(REASON_TLS_HANDSHAKE, <<"TLS handshake failed">>).
-define(REASON_AUTH, <<"LDAP simple bind rejected the supplied credentials">>).
-define(REASON_ASSUME_ROLE, <<"failed to assume the configured role">>).
-define(REASON_NO_ASSUME_ROLE, <<
    "auth validation requires an assume_role to be configured; "
    "set aws.arns.assume_role_arn"
>>).
-define(REASON_BAD_DN_LOOKUP_BASE, <<"dn_lookup_base must be a non-empty string">>).
-define(REASON_BAD_DN_LOOKUP_ATTR, <<"dn_lookup_attribute must be a non-empty string">>).
-define(REASON_BAD_USERNAME, <<"username must be a non-empty string">>).
-define(REASON_BAD_QUERIES, <<"queries must be an object of query strings">>).
-define(REASON_BAD_QUERY_VALUE, <<"each query must be a non-empty string">>).
-define(REASON_QUERY_PARSE, <<"one or more authorization queries are not valid">>).
-define(REASON_DN_LOOKUP_BASE_UNVERIFIED,
    <<"dn_lookup_base does not exist or is not readable by the bind user">>
).
-define(REASON_AUTHZ_UNVERIFIED,
    <<"a DN referenced by an authorization query could not be verified">>
).
-define(REASON_NOT_MEMBER,
    <<"the supplied username is not authorized by one or more authorization queries">>
).
-define(REASON_USERNAME_UNRESOLVED,
    <<"the supplied username could not be resolved to a single directory entry">>
).

method_name() ->
    <<"ldap">>.

allowed_fields() ->
    [
        <<"servers">>,
        <<"port">>,
        <<"user_dn">>,
        <<"password_arn">>,
        <<"use_ssl">>,
        <<"use_starttls">>,
        <<"ssl_options">>,
        <<"dn_lookup_base">>,
        <<"dn_lookup_attribute">>,
        <<"username">>,
        <<"queries">>
    ].

-spec validate(map()) -> aws_auth_validate_backend:result().
validate(Body) when is_map(Body) ->
    %% Order matters for security: every purely-local check (type/shape
    %% validation, query grammar, config conflicts) runs before we resolve
    %% the password ARN. Malformed input is rejected without fetching the
    %% customer's secret, and the secret is resolved only once we are about
    %% to actually connect.
    case parse_input(Body) of
        {error, _, _} = Err ->
            Err;
        {ok, Params} ->
            case check_config_conflicts(Params) of
                {error, _, _} = Err ->
                    Err;
                ok ->
                    case resolve_request_state(Params) of
                        {error, _, _} = Err ->
                            Err;
                        {ok, Params0} ->
                            case resolve_arn_material(Body, Params0) of
                                {error, _, _} = Err -> Err;
                                {ok, Params2} -> do_ldap_validate(Params2)
                            end
                    end
            end
    end.

%% Build the per-request aws_lib state used for every ARN fetch in this request.
%% Runs in the network phase, after all pure input validation has passed, so a
%% malformed request never triggers an STS AssumeRole call.
%%
%% A configured `aws.arns.assume_role_arn' is MANDATORY. When set, we assume
%% that role into the request's aws_lib state -- the SAME role the plugin
%% already assumes at boot to resolve every configured ARN
%% (aws_arn_config:maybe_assume_role/1) -- and resolve password_arn/cacertfile_arn
%% under it. This is operator config, not caller input, so it raises no
%% confused-deputy concern.
%%
%% When NO role is configured we do NOT fall back to a default aws_lib state:
%% that would resolve ARNs with the broker's ambient (EC2 instance) credentials,
%% which on Amazon MQ can be far more privileged than the role a customer would
%% attach to their own secret/bucket, so a validate request could resolve ARNs
%% the caller's intended role never could -- a least-privilege pitfall (the
%% resolved secret is never returned to the caller today, but the capability
%% would be a hazard for future development).
%% We therefore never use the instance role: with no assume_role configured we
%% refuse the request with config_conflict before any secret fetch or outbound
%% connection.
%%
%% The state is request-local (aws_lib threads credentials per call -- there is
%% no global singleton to clobber), so the assumed role's credentials are
%% visible only to this request's ARN resolutions and are discarded when the
%% request ends.
resolve_request_state(Params) ->
    case configured_assume_role_arn() of
        none ->
            {error, config_conflict, ?REASON_NO_ASSUME_ROLE};
        RoleArn ->
            case aws_iam:assume_role(RoleArn, aws_lib:new()) of
                {ok, State} -> {ok, Params#{aws_state => State}};
                {error, _} -> {error, input_invalid, ?REASON_ASSUME_ROLE}
            end
    end.

%% The operator-configured boot-time assume role (shared helper).
configured_assume_role_arn() ->
    aws_auth_validate_ssl:configured_assume_role_arn().

%%--------------------------------------------------------------------
%% Input parsing
%%--------------------------------------------------------------------

%% Pure, network-free validation steps. Password ARN resolution is handled
%% separately (resolve_password/2) after these all pass.
parse_input(Body) ->
    Steps = [
        fun parse_servers/2,
        fun parse_port/2,
        fun parse_user_dn/2,
        fun parse_use_ssl/2,
        fun parse_use_starttls/2,
        fun parse_ssl_options/2,
        fun parse_dn_lookup_base/2,
        fun parse_dn_lookup_attribute/2,
        fun parse_username/2,
        fun parse_queries/2
    ],
    parse_input(Steps, Body, #{timeout => connection_timeout_ms()}).

parse_input([], _Body, Acc) ->
    {ok, Acc};
parse_input([Step | Rest], Body, Acc0) ->
    case Step(Body, Acc0) of
        {ok, Acc1} -> parse_input(Rest, Body, Acc1);
        {error, _, _} = Err -> Err
    end.

parse_servers(Body, Acc) ->
    case maps:get(<<"servers">>, Body, undefined) of
        Servers when is_list(Servers), Servers =/= [] ->
            case lists:all(fun is_nonempty_binary/1, Servers) of
                true ->
                    ServerStrs = [binary_to_list(S) || S <- Servers],
                    case lists:all(fun is_allowed_server/1, ServerStrs) of
                        true -> {ok, Acc#{servers => ServerStrs}};
                        false -> {error, input_invalid, ?REASON_BLOCKED_SERVER}
                    end;
                false ->
                    {error, input_invalid, ?REASON_BAD_SERVERS}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_SERVERS}
    end.

parse_port(Body, Acc) ->
    case maps:get(<<"port">>, Body, undefined) of
        %% Omitted port defaults to the broker's default (389), so a config the
        %% live broker would accept is not rejected here for parity's sake.
        undefined ->
            {ok, Acc#{port => ?DEFAULT_LDAP_PORT}};
        Port when is_integer(Port), Port >= 1, Port =< 65535 ->
            {ok, Acc#{port => Port}};
        _ ->
            {error, input_invalid, ?REASON_BAD_PORT}
    end.

parse_user_dn(Body, Acc) ->
    case maps:get(<<"user_dn">>, Body, undefined) of
        UserDn when is_binary(UserDn), byte_size(UserDn) > 0 ->
            {ok, Acc#{user_dn => binary_to_list(UserDn)}};
        _ ->
            {error, input_invalid, ?REASON_BAD_USER_DN}
    end.

%% Resolve the bind password from its ARN. Runs only after all pure input
%% validation has passed, so a malformed request never triggers a secret
%% fetch. The resolved password is added to the params map and never logged
%% or returned. Validated for shape here (rather than in parse_input) so the
%% network call stays out of the pure pipeline.
%% An ARN-resolution failure returns the field-tagged {arn_failed, Fields} form
%% so resolve_arn_material/2 can aggregate it with the cacert ARN's outcome and
%% name every failing field in one response. A SHAPE failure is unrelated to
%% resolution and still short-circuits with its own reason.
resolve_password(Body, #{aws_state := State} = Params) ->
    case maps:get(<<"password_arn">>, Body, undefined) of
        Arn when is_binary(Arn), byte_size(Arn) > 0 ->
            case resolve_arn(Arn, State) of
                {ok, Password} -> {ok, Params#{password => Password}};
                {error, _} -> {arn_failed, [<<"password_arn">>]}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_PASSWORD_ARN}
    end.

%% Resolve both ARN-backed inputs -- the bind password and (when TLS is in use)
%% the CA bundle -- attempting BOTH so a request with two broken ARNs names both
%% fields instead of sending the operator round the loop twice.
%%
%% Only resolution failures aggregate. A shape error or a PEM-content error is
%% reported as-is: in the content case the ARN did resolve, so naming it would
%% misdescribe the failure.
resolve_arn_material(Body, Params) ->
    case resolve_password(Body, Params) of
        {error, _, _} = ShapeErr ->
            %% A SHAPE error (password_arn missing/empty) is pure-input invalid,
            %% so stop here rather than aggregating. Continuing would fetch the
            %% cacert ARN for a request we have already rejected, breaking the
            %% ARN-first ordering invariant that a malformed request triggers no
            %% secret fetch.
            ShapeErr;
        PasswordRes ->
            %% Resolve the cacert ARN against the incoming Params; the password
            %% branch's map is merged in below. Runs even when the password ARN
            %% failed to RESOLVE, so both failing fields can be named at once.
            aggregate_arn_results(PasswordRes, resolve_cacert(Params))
    end.

aggregate_arn_results(PasswordRes, CacertRes) ->
    case arn_failed_fields(PasswordRes) ++ arn_failed_fields(CacertRes) of
        [_ | _] = Failed ->
            {error, input_invalid, aws_auth_validate_ssl:arn_resolve_reason(Failed)};
        [] ->
            %% PasswordRes cannot be an error here: resolve_arn_material/2
            %% short-circuits the only shape error before this point, leaving
            %% {ok,_} once its ARN did not fail to resolve. Only the cacert
            %% branch can still carry a CONTENT error (an invalid PEM).
            case {PasswordRes, CacertRes} of
                {_, {error, _, _} = Err} ->
                    Err;
                {{ok, WithPassword}, {ok, WithCacert}} ->
                    %% Both succeeded: keep the password from one branch and the
                    %% decoded cacerts (under ssl_options) from the other.
                    {ok, maps:merge(WithPassword, maps:with([ssl_options], WithCacert))}
            end
    end.

arn_failed_fields({arn_failed, Fields}) -> Fields;
arn_failed_fields(_Other) -> [].

%% Resolve the CA-cert ARN (when ssl_options.cacertfile_arn is set) in the
%% network phase, alongside the password ARN and after all pure validation.
%% A resolve OR a PEM-decode failure is reported as input_invalid -- mirroring
%% resolve_password/2 -- rather than silently leaving the connection without a
%% trust anchor. The latter would let `verify' fall back to verify_none (see
%% build_tls_opts/2 and the shared apply_verify_default/2) and report a TLS
%% config as valid that the operator believes is certificate-verified -- a
%% silent security degradation. The
%% decoded certs are stored under the atom `cacerts' key in ssl_options so
%% build_ssl_opts/1 consumes them directly instead of re-resolving the ARN
%% (and re-clobbering the region) at connect time.
%%
%% Only resolved when TLS is actually in use (use_ssl or use_starttls): the
%% cert is consumed solely by build_ssl_opts/1, which only runs on the TLS
%% paths, so fetching it for a plaintext request would be a pointless secret
%% fetch and could reject an otherwise-valid plaintext config.
resolve_cacert(#{use_ssl := false, use_starttls := false} = Params) ->
    {ok, Params};
resolve_cacert(#{ssl_options := SslOpts, aws_state := State} = Params) ->
    case maps:get(<<"cacertfile_arn">>, SslOpts, undefined) of
        undefined ->
            {ok, Params};
        Arn when is_binary(Arn) ->
            case resolve_arn(Arn, State) of
                {ok, PemData} ->
                    %% A malformed PEM can raise during decode; an empty/
                    %% certless PEM returns `skip'. Both mean the ARN does not
                    %% hold a usable CA cert -- report input_invalid rather than
                    %% degrading the trust anchor.
                    try aws_auth_validate_ssl:decode_pem_cacerts(PemData) of
                        skip -> {error, input_invalid, ?REASON_CACERT_PEM_INVALID};
                        Certs -> {ok, Params#{ssl_options => SslOpts#{cacerts => Certs}}}
                    catch
                        _:_ -> {error, input_invalid, ?REASON_CACERT_PEM_INVALID}
                    end;
                {error, _} ->
                    {arn_failed, [<<"ssl_options.cacertfile_arn">>]}
            end
    end.

parse_use_ssl(Body, Acc) ->
    parse_boolean(<<"use_ssl">>, Body, Acc, use_ssl, ?REASON_BAD_SSL_FLAG).

parse_use_starttls(Body, Acc) ->
    parse_boolean(<<"use_starttls">>, Body, Acc, use_starttls, ?REASON_BAD_STARTTLS_FLAG).

parse_boolean(Key, Body, Acc, AccKey, Reason) ->
    case maps:get(Key, Body, undefined) of
        undefined -> {ok, Acc#{AccKey => false}};
        Bool when is_boolean(Bool) -> {ok, Acc#{AccKey => Bool}};
        _ -> {error, input_invalid, Reason}
    end.

parse_ssl_options(Body, Acc) ->
    case maps:get(<<"ssl_options">>, Body, undefined) of
        undefined ->
            {ok, Acc#{ssl_options => #{}}};
        Map when is_map(Map) ->
            %% Validate both keys AND values here, in the pure phase. Rejecting
            %% only unknown keys is not enough: a known key with a mis-typed
            %% value (e.g. "verify":"verfy_none") would otherwise survive to
            %% build_ssl_opts, be silently dropped by its catch, and re-default
            %% to verify_peer -- so validation would test options that differ
            %% from what was submitted, the exact silent-drop failure we are
            %% trying to prevent.
            case [K || K <- maps:keys(Map), not lists:member(K, ?SSL_OPTION_KEYS)] of
                [] -> validate_ssl_values(maps:to_list(Map), Acc, Map);
                [_ | _] -> {error, input_invalid, ?REASON_UNKNOWN_SSL_OPTION}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_SSL_OPTIONS}
    end.

%% Validate each known ssl_options value for shape/domain in the pure phase.
%% On success the original map is stored unchanged (build_ssl_opts translates
%% it later); on the first bad value the whole request is rejected.
validate_ssl_values([], Acc, Map) ->
    {ok, Acc#{ssl_options => Map}};
validate_ssl_values([{Key, Value} | Rest], Acc, Map) ->
    case valid_ssl_value(Key, Value) of
        ok -> validate_ssl_values(Rest, Acc, Map);
        {error, _, _} = Err -> Err
    end.

valid_ssl_value(<<"verify">>, V) ->
    case lists:member(V, ?SSL_VERIFY_VALUES) of
        true -> ok;
        false -> {error, input_invalid, ?REASON_BAD_SSL_VERIFY}
    end;
valid_ssl_value(<<"depth">>, V) when is_integer(V), V >= 0 ->
    ok;
valid_ssl_value(<<"depth">>, _) ->
    {error, input_invalid, ?REASON_BAD_SSL_DEPTH};
valid_ssl_value(<<"versions">>, V) when is_list(V), V =/= [] ->
    case lists:all(fun(Ver) -> lists:member(Ver, ?SSL_VERSION_VALUES) end, V) of
        true -> ok;
        false -> {error, input_invalid, ?REASON_BAD_SSL_VERSIONS}
    end;
valid_ssl_value(<<"versions">>, _) ->
    {error, input_invalid, ?REASON_BAD_SSL_VERSIONS};
valid_ssl_value(<<"server_name_indication">>, V) ->
    case is_nonempty_binary(V) of
        true -> ok;
        false -> {error, input_invalid, ?REASON_BAD_SSL_SNI}
    end;
valid_ssl_value(<<"hostname_verification">>, V) ->
    case lists:member(V, ?SSL_HOSTNAME_VERIFICATION_VALUES) of
        true -> ok;
        false -> {error, input_invalid, ?REASON_BAD_SSL_HOSTNAME_VERIFICATION}
    end;
valid_ssl_value(<<"cacertfile_arn">>, V) ->
    case is_nonempty_binary(V) of
        true -> ok;
        false -> {error, input_invalid, ?REASON_BAD_SSL_CACERT_ARN}
    end.

%% DN lookup fields are optional. When `dn_lookup_base' is absent we store
%% `none' and skip the readability check entirely. `dn_lookup_attribute' is
%% likewise optional and validated for shape only.
parse_dn_lookup_base(Body, Acc) ->
    case maps:get(<<"dn_lookup_base">>, Body, undefined) of
        undefined ->
            {ok, Acc#{dn_lookup_base => none}};
        Base when is_binary(Base), byte_size(Base) > 0 ->
            {ok, Acc#{dn_lookup_base => binary_to_list(Base)}};
        _ ->
            {error, input_invalid, ?REASON_BAD_DN_LOOKUP_BASE}
    end.

parse_dn_lookup_attribute(Body, Acc) ->
    case maps:get(<<"dn_lookup_attribute">>, Body, undefined) of
        undefined ->
            {ok, Acc#{dn_lookup_attribute => none}};
        Attr when is_binary(Attr), byte_size(Attr) > 0 ->
            {ok, Acc#{dn_lookup_attribute => binary_to_list(Attr)}};
        _ ->
            {error, input_invalid, ?REASON_BAD_DN_LOOKUP_ATTR}
    end.

%% Optional principal to evaluate authorization queries against. When absent we
%% store `none' and the authz check stays in its literal-DN reachability mode
%% (exactly today's behavior). When present, the bound service connection
%% resolves it to a user DN and the queries' ${username}/${user_dn} placeholders
%% are filled so membership is actually evaluated. The username is an identifier,
%% not a credential -- no principal password is accepted (see resolve_user_dn/2).
parse_username(Body, Acc) ->
    case maps:get(<<"username">>, Body, undefined) of
        undefined ->
            {ok, Acc#{username => none}};
        Username when is_binary(Username), byte_size(Username) > 0 ->
            {ok, Acc#{username => binary_to_list(Username)}};
        _ ->
            {error, input_invalid, ?REASON_BAD_USERNAME}
    end.

%% `queries' is an optional object mapping query names (tags, vhost_access,
%% resource_access, topic_access) to query-DSL strings. Each value is parsed
%% with the broker-compatible grammar; unknown query names are ignored (the
%% registry-level field filter governs the top-level surface, not nested
%% keys). A parse failure short-circuits to query_invalid.
parse_queries(Body, Acc) ->
    case maps:get(<<"queries">>, Body, undefined) of
        undefined ->
            {ok, Acc#{queries => []}};
        Map when is_map(Map) ->
            parse_query_map(maps:to_list(Map), Acc, []);
        _ ->
            {error, input_invalid, ?REASON_BAD_QUERIES}
    end.

parse_query_map([], Acc, Parsed) ->
    {ok, Acc#{queries => lists:reverse(Parsed)}};
parse_query_map([{Name, Value} | Rest], Acc, Parsed) ->
    case lists:member(Name, ?QUERY_NAMES) of
        false ->
            %% Ignore query names we don't recognise rather than failing the
            %% whole request; keeps the accepted surface aligned with the
            %% broker's four query types without leaking which were ignored.
            parse_query_map(Rest, Acc, Parsed);
        true ->
            case Value of
                V when is_binary(V), byte_size(V) > 0 ->
                    case aws_auth_validate_ldap_query:parse(V) of
                        {ok, Query} ->
                            parse_query_map(Rest, Acc, [{Name, Query} | Parsed]);
                        {error, _} ->
                            {error, query_invalid, ?REASON_QUERY_PARSE}
                    end;
                _ ->
                    {error, input_invalid, ?REASON_BAD_QUERY_VALUE}
            end
    end.

is_nonempty_binary(B) -> is_binary(B) andalso byte_size(B) > 0.

%% Server address validation: resolve the hostname and reject addresses in
%% private, loopback, link-local, or metadata IP ranges. This prevents SSRF
%% via the validation endpoint while still allowing legitimate LDAP servers.
%%
%% This is the FIRST of two SSRF checks (defence in depth). It resolves Server
%% and rejects a blocked IP before any connection is attempted, so a malformed
%% or obviously-internal target never opens a socket. On its own it is subject
%% to a DNS-rebinding TOCTOU -- eldap:open/2 re-resolves the hostname, so the
%% peer it connects to could differ from the IP vetted here. That window is
%% closed by the SECOND check, verify_connected_peer/1, which re-validates the
%% actual connected socket's peer (see do_ldap_bind/1). We keep this pre-connect
%% check too: it rejects bad input cheaply (no socket, clearer input_invalid
%% category) and avoids dialing out for the common case.
%%
%% The test-only auth_validation_allow_private_networks flag relaxes ONLY
%% loopback (see is_denied_ip/1), not the whole denylist -- it must not grant the
%% LDAP backend a broader test-mode bypass than the http/oauth backends get from
%% aws_auth_validate_net (which relaxes loopback alone). IMDS, link-local,
%% RFC1918, CGNAT, and reserved space stay denied even with the flag on, so a
%% test env cannot accidentally prove a path to instance metadata that the
%% stricter guard would block.
is_allowed_server(Server) ->
    check_server_ip(Server).

check_server_ip(Server) ->
    case inet:getaddr(Server, inet) of
        {ok, IP} ->
            not is_denied_ip(IP);
        {error, _} ->
            %% Also try IPv6
            case inet:getaddr(Server, inet6) of
                {ok, IP6} -> not is_denied_ip(IP6);
                {error, _} -> false
            end
    end.

%% Classify a v4 or v6 address against LDAP's range policy via the shared
%% aws_auth_validate_net:classify_ip/2 (which dispatches on tuple shape), passing
%% LDAP's own denylist. Sharing the classifier (not just the CIDR-matching leaf)
%% means the v6->embedded-v4 unwrap and the loopback-only test relaxation are
%% defined in ONE place: LDAP cannot drift from http/oauth on how a v6-encoded v4
%% address (e.g. ::ffff:127.0.0.1) is unwrapped before the relaxation applies.
%% LDAP keeps its own broader list.
is_denied_ip(IP) ->
    aws_auth_validate_net:classify_ip(IP, ldap_cidr_policy()) =:= deny.

%% LDAP's SSRF range policy, as a classify_ip/2 CIDR policy. This is deliberately
%% BROADER than the http/oauth infra-only denylist in aws_auth_validate_net: LDAP
%% denies ALL RFC1918/CGNAT/reserved space, not just broker infra. Sharing the
%% classifier while keeping a distinct list means a future range or segment-math
%% fix cannot silently apply to only one backend. Mirrors #153, one layer up.
%%   100.64.0.0/10 (RFC 6598) is carrier-grade NAT shared address space; it can
%%     route to provider/internal infrastructure, so it is denied too.
%%   240.0.0.0/4 is reserved/Class E, including 255.255.255.255 limited broadcast.
%%   fc00::/7 includes the IPv6 IMDS address fd00:ec2::254; fe80::/10 spans
%%     fe80..febf. v4-carrying v6 notations are unwrapped by classify_ip/2.
ldap_cidr_policy() ->
    #{
        denied_v4 => [
            {{127, 0, 0, 0}, 8},
            {{10, 0, 0, 0}, 8},
            {{172, 16, 0, 0}, 12},
            {{192, 168, 0, 0}, 16},
            {{169, 254, 0, 0}, 16},
            {{100, 64, 0, 0}, 10},
            {{0, 0, 0, 0}, 8},
            {{240, 0, 0, 0}, 4}
        ],
        denied_v6 => [
            {{0, 0, 0, 0, 0, 0, 0, 1}, 128},
            {{0, 0, 0, 0, 0, 0, 0, 0}, 128},
            {{16#fc00, 0, 0, 0, 0, 0, 0, 0}, 7},
            {{16#fe80, 0, 0, 0, 0, 0, 0, 0}, 10}
        ]
    }.

%%--------------------------------------------------------------------
%% Config conflict
%%--------------------------------------------------------------------

check_config_conflicts(#{use_ssl := true, use_starttls := true}) ->
    {error, config_conflict, ?REASON_TLS_BOTH};
check_config_conflicts(_) ->
    ok.

%%--------------------------------------------------------------------
%% LDAP execution
%%--------------------------------------------------------------------

%% SECURITY: no credential leakage. The resolved bind password is passed to
%% eldap:simple_bind/3 as a direct argument. If anything in the connect/bind/
%% post-bind section *raises* (rather than returning {error, _}), the
%% exception's stacktrace would carry the live argument terms -- including this
%% function's Params map, which holds the plaintext password -- into a Cowboy
%% crash report. To guarantee the password can never reach a log or crash dump,
%% the entire body is destructured into a separate worker (do_ldap_bind/1) whose
%% only job is to return a fixed-category result, and any escaping exception is
%% caught here and collapsed to the fixed connection_failed category. We
%% deliberately discard the caught class/reason/stacktrace (binding them to
%% throwaway names that are never logged or returned) so no fragment of the bind
%% arguments survives.
%% Note: a raise in a post-bind probe (e.g. eldap:search/2 in object_exists/2)
%% is therefore reported as connection_failed rather than its more specific
%% category. This is a deliberate, safe degradation -- those probes already map
%% their non-{ok,_} returns to false, so only a genuine exception reaches here.
do_ldap_validate(#{password := _} = Params) ->
    try
        do_ldap_bind(Params)
    catch
        _Class:_Reason:_Stack ->
            {error, connection_failed, ?REASON_CONNECTION}
    end.

do_ldap_bind(#{use_ssl := UseSsl, use_starttls := UseStartTls, ssl_options := SslOpts} = Params) ->
    %% Build the eldap ssl option list once, up front, so the verify-mode policy
    %% (aws_auth_validate_ssl:apply_verify_default/2) runs before we open the
    %% connection: an EXPLICIT verify_peer with no trust anchor fails loud here as
    %% tls_failed -- with an actionable reason -- instead of surfacing as an opaque
    %% unknown_ca handshake error. This matches the http/oauth backends, which
    %% already delegate to the same shared helper.
    case build_tls_opts(UseSsl orelse UseStartTls, SslOpts) of
        {error, _, _} = Err ->
            Err;
        {ok, TlsOpts} ->
            do_ldap_connect(Params, TlsOpts)
    end.

do_ldap_connect(
    #{
        servers := Servers,
        port := Port,
        user_dn := UserDn,
        password := Password,
        use_ssl := UseSsl,
        use_starttls := UseStartTls,
        timeout := Timeout
    } = Params,
    TlsOpts
) ->
    OpenOpts = [{port, Port}, {timeout, Timeout}] ++ ssl_open_opts(UseSsl, TlsOpts),
    case eldap:open(Servers, OpenOpts) of
        {error, _Reason} ->
            {error, connection_failed, ?REASON_CONNECTION};
        {ok, Handle} ->
            try
                %% SSRF defense: close the DNS-rebinding window. parse_servers/2's
                %% is_allowed_server/1 vetted a resolved IP, but eldap re-resolved
                %% the hostname for this connection, so the peer we are actually
                %% attached to may differ (DNS rebinding). Re-check the *real*
                %% connected peer -- the exact socket every later operation
                %% (start_tls, bind, search) runs over -- before sending anything.
                %% A blocked peer collapses to connection_failed (indistinguishable
                %% from an unreachable host, so it leaks no recon signal).
                case verify_connected_peer(Handle) of
                    blocked ->
                        {error, connection_failed, ?REASON_CONNECTION};
                    ok ->
                        case maybe_start_tls(Handle, UseStartTls, TlsOpts, Timeout) of
                            {error, _Reason} ->
                                {error, tls_failed, ?REASON_TLS_HANDSHAKE};
                            ok ->
                                case eldap:simple_bind(Handle, UserDn, Password) of
                                    ok -> post_bind_checks(Handle, Params);
                                    {error, _Reason} -> {error, auth_failed, ?REASON_AUTH}
                                end
                        end
                end
            after
                catch eldap:close(Handle)
            end
    end.

%% Re-validate the IP eldap actually connected to, closing the DNS-rebinding
%% TOCTOU between is_allowed_server/1 (pre-connect, on a resolved IP) and this
%% live socket. Reads the peer from the open handle rather than re-resolving the
%% hostname, so what we check is exactly what we will talk to. Always runs; the
%% test-only loopback relaxation lives in is_denied_ip/1 (see is_allowed_server/1),
%% so a local-slapd suite reaches 127.0.0.1/::1 while IMDS/RFC1918/etc stay denied
%% even under the flag. Fails closed: if the peer cannot be determined, treat it
%% as blocked.
verify_connected_peer(Handle) ->
    check_peer_ip(Handle).

%% eldap:info/1 is specced as returning connection_info() unconditionally, so
%% dialyzer considers the fallback clause unreachable. The spec is optimistic:
%% info/1 returns recv/1's result, and recv/1 answers
%% {error, {internal_error, Reason}} if the eldap process exits first. The clause
%% is therefore reachable in practice, and it must stay because this function
%% fails closed -- dropping it would turn a dead connection into a
%% function_clause instead of `blocked'.
-dialyzer({no_match, check_peer_ip/1}).
check_peer_ip(Handle) ->
    case eldap:info(Handle) of
        #{socket := Sock, socket_type := ssl} -> peer_allowed(ssl:peername(Sock));
        #{socket := Sock, socket_type := tcp} -> peer_allowed(inet:peername(Sock));
        _ -> blocked
    end.

peer_allowed({ok, {IP, _Port}}) when tuple_size(IP) =:= 4; tuple_size(IP) =:= 8 ->
    case is_denied_ip(IP) of
        true -> blocked;
        false -> ok
    end;
peer_allowed(_) ->
    blocked.

%% After a successful bind, run the optional DN-lookup, principal resolution,
%% and authorization checks in sequence on the same bound handle. The first
%% failure wins; if none are configured (or all pass) the request succeeds with
%% `ok'. When a username was supplied we resolve it to a user DN here (between
%% the dn_lookup-base reachability check and the query evaluation) so the authz
%% queries can be evaluated for that principal rather than only reachability-
%% checked.
post_bind_checks(Handle, Params) ->
    case check_dn_lookup(Handle, Params) of
        ok ->
            case resolve_principal_dn(Handle, Params) of
                {ok, UserDn} ->
                    check_authz_queries(Handle, Params#{resolved_user_dn => UserDn});
                {error, _, _} = Err ->
                    Err
            end;
        {error, _, _} = Err ->
            Err
    end.

%% Resolve the request-supplied username to a single user DN, using the bound
%% service connection -- the same equalityMatch(dn_lookup_attribute, username)
%% search rabbit_auth_backend_ldap:dn_lookup/2 runs. This is a read over the
%% admin's own credentials; no principal password is involved. We read the
%% matched entry's object_name (its DN), not an attribute value. Unlike the
%% broker -- which silently falls back to an escaped user_dn pattern on 0/many
%% matches -- the endpoint treats anything other than exactly one match as
%% authz_unverified, since at validation time an unresolvable username is
%% precisely the misconfiguration the admin wants surfaced.
%%
%% `none' means no username was supplied (or no dn_lookup config to resolve it
%% with): we return `unknown', and query evaluation falls back to the literal-DN
%% reachability path -- exactly today's behavior.
resolve_principal_dn(_Handle, #{username := none}) ->
    {ok, unknown};
resolve_principal_dn(_Handle, #{dn_lookup_base := none}) ->
    {ok, unknown};
resolve_principal_dn(_Handle, #{dn_lookup_attribute := none}) ->
    {ok, unknown};
resolve_principal_dn(Handle, #{
    username := Username,
    dn_lookup_base := Base,
    dn_lookup_attribute := Attr
}) ->
    case
        eldap:search(Handle, [
            {base, Base},
            {filter, eldap:equalityMatch(Attr, Username)},
            {attributes, ["distinguishedName"]},
            {timeout, search_time_limit_seconds()}
        ])
    of
        {ok, #eldap_search_result{entries = [#eldap_entry{object_name = Dn}]}} ->
            {ok, Dn};
        _ ->
            {error, authz_unverified, ?REASON_USERNAME_UNRESOLVED}
    end.

%%--------------------------------------------------------------------
%% DN lookup validation
%%--------------------------------------------------------------------

%% When a dn_lookup_base is supplied, confirm it exists and is readable by
%% the bound user. We never run an actual username->DN search here (there is
%% no principal to look up and doing so could leak directory contents);
%% confirming the base is reachable is the side-effect-free signal a
%% customer needs to know their dn_lookup_base is correct.
check_dn_lookup(_Handle, #{dn_lookup_base := none}) ->
    ok;
check_dn_lookup(Handle, #{dn_lookup_base := Base}) ->
    case object_exists(Handle, Base) of
        true -> ok;
        _ -> {error, authz_unverified, ?REASON_DN_LOOKUP_BASE_UNVERIFIED}
    end.

%%--------------------------------------------------------------------
%% Authorization query validation
%%--------------------------------------------------------------------

%% Two modes, selected by whether a principal was resolved:
%%
%%   * No principal (resolved_user_dn = unknown) -- today's behavior. For each
%%     parsed query, extract the fully-literal DNs (no ${...} placeholder) and
%%     confirm each exists and is readable. Queries that reference only
%%     runtime-filled DNs contribute no checks (grammar-validated at parse time).
%%
%%   * With a principal -- fill each query's ${username}/${user_dn}/${ad_*}
%%     placeholders for that user, then *evaluate* it against the bound handle
%%     (membership, existence, attribute comparisons), mirroring how
%%     rabbit_auth_backend_ldap evaluates the same query. A query that evaluates
%%     false is the real misconfiguration signal (the user is not authorized);
%%     a node still carrying an unfillable per-operation placeholder
%%     (${vhost}/${resource}/...) is grammar-checked-only and degrades rather
%%     than failing.
check_authz_queries(Handle, #{queries := Queries, resolved_user_dn := unknown}) ->
    LiteralDns = lists:usort(
        lists:flatmap(
            fun({_Name, Query}) -> aws_auth_validate_ldap_query:literal_dns(Query) end,
            Queries
        )
    ),
    check_dns(Handle, LiteralDns);
check_authz_queries(Handle, #{queries := Queries, resolved_user_dn := UserDn} = Params) ->
    Username = maps:get(username, Params),
    Args =
        [{username, Username}, {user_dn, UserDn}] ++
            aws_auth_validate_ldap_query:ad_args(Username),
    eval_queries(Handle, Queries, Args).

check_dns(_Handle, []) ->
    ok;
check_dns(Handle, [DN | Rest]) ->
    case object_exists(Handle, DN) of
        true -> check_dns(Handle, Rest);
        _ -> {error, authz_unverified, ?REASON_AUTHZ_UNVERIFIED}
    end.

%% Evaluate each query for the resolved principal. The first query that is
%% conclusively false (the user is not authorized by it) wins and maps to
%% authz_unverified; a query that cannot be evaluated (only unfillable
%% per-operation placeholders) is skipped. All-pass/all-degrade -> ok.
eval_queries(_Handle, [], _Args) ->
    ok;
eval_queries(Handle, [{_Name, Query} | Rest], Args) ->
    Filled = aws_auth_validate_ldap_query:fill(Query, Args),
    UserDn = proplists:get_value(user_dn, Args),
    case eval_query(Handle, Filled, UserDn) of
        skip -> eval_queries(Handle, Rest, Args);
        true -> eval_queries(Handle, Rest, Args);
        false -> {error, authz_unverified, ?REASON_NOT_MEMBER}
    end.

%% Evaluate one (already principal-filled) query against the bound handle,
%% mirroring rabbit_auth_backend_ldap:evaluate0/4 minus the connection/creds
%% machinery (the bind identity is fixed = the service account; the broker's
%% as_user path is unreachable since no principal password exists). Returns
%% `true' | `false' | `skip', where `skip' means the term could not be
%% evaluated at validation time (an unfillable ${vhost}/${resource}/... DN, or a
%% sub-result that itself degraded) and must not be treated as a failure.
%% Composition guards against non-boolean (skipped) sub-results so `not'/`and'/
%% `or' never badarg the way the broker's raw boolean ops could. UserDn is
%% threaded through composition so nested membership terms can still evaluate.
eval_query(_Handle, {constant, Bool}, _UserDn) when is_boolean(Bool) ->
    Bool;
eval_query(Handle, {exists, DN}, _UserDn) ->
    eval_dn_probe(DN, fun(Dn) -> object_exists(Handle, Dn) end);
eval_query(Handle, {in_group, DN}, UserDn) ->
    eval_query(Handle, {in_group, DN, "member"}, UserDn);
eval_query(Handle, {in_group, DN, Desc}, UserDn) ->
    eval_membership(Handle, DN, Desc, UserDn);
eval_query(Handle, {in_group_nested, DN}, UserDn) ->
    eval_query(Handle, {in_group_nested, DN, "member"}, UserDn);
eval_query(Handle, {in_group_nested, DN, Desc}, UserDn) ->
    %% Nested membership needs a group-search base the endpoint does not model
    %% (the broker's group_lookup_base). Rather than search a wrong base, fall
    %% back to direct membership at the named group -- a safe subset that still
    %% catches the common "is the user in THIS group" case; deeper nesting
    %% degrades to that single hop.
    eval_membership(Handle, DN, Desc, UserDn);
eval_query(Handle, {in_group_nested, DN, Desc, _Scope}, UserDn) ->
    eval_membership(Handle, DN, Desc, UserDn);
eval_query(Handle, {'not', Sub}, UserDn) ->
    %% Negate only a conclusive sub-result; a skipped sub-result stays skipped.
    case eval_query(Handle, Sub, UserDn) of
        skip -> skip;
        Bool -> not Bool
    end;
eval_query(Handle, {'and', Subs}, UserDn) when is_list(Subs) ->
    eval_and(Handle, Subs, UserDn);
eval_query(Handle, {'or', Subs}, UserDn) when is_list(Subs) ->
    eval_or(Handle, Subs, UserDn);
eval_query(_Handle, _Other, _UserDn) ->
    %% equals/match/for and any residual shape need per-operation context
    %% (${vhost}/${resource}/...) or attribute reads the endpoint does not model
    %% for validation; degrade rather than guess.
    skip.

%% Conjunction over true/false/skip: any conclusive false short-circuits to
%% false; otherwise the result is true only if every conjunct is true, else skip
%% (at least one conjunct degraded).
eval_and(_Handle, [], _UserDn) ->
    true;
eval_and(Handle, [Sub | Rest], UserDn) ->
    case eval_query(Handle, Sub, UserDn) of
        false -> false;
        skip -> eval_and_skip(Handle, Rest, UserDn);
        true -> eval_and(Handle, Rest, UserDn)
    end.

%% After a conjunct degraded, a later conclusive false still makes the whole
%% `and' false; otherwise it can only be skip (cannot prove all-true).
eval_and_skip(_Handle, [], _UserDn) ->
    skip;
eval_and_skip(Handle, [Sub | Rest], UserDn) ->
    case eval_query(Handle, Sub, UserDn) of
        false -> false;
        _ -> eval_and_skip(Handle, Rest, UserDn)
    end.

%% Disjunction over true/false/skip: any conclusive true short-circuits to true;
%% otherwise false only if every disjunct is false, else skip.
eval_or(_Handle, [], _UserDn) ->
    false;
eval_or(Handle, [Sub | Rest], UserDn) ->
    case eval_query(Handle, Sub, UserDn) of
        true -> true;
        skip -> eval_or_skip(Handle, Rest, UserDn);
        false -> eval_or(Handle, Rest, UserDn)
    end.

eval_or_skip(_Handle, [], _UserDn) ->
    skip;
eval_or_skip(Handle, [Sub | Rest], UserDn) ->
    case eval_query(Handle, Sub, UserDn) of
        true -> true;
        _ -> eval_or_skip(Handle, Rest, UserDn)
    end.

%% Resolve a (filled) DN to a probe result, degrading when a placeholder
%% survived (per-operation context we cannot supply).
eval_dn_probe(DN, Probe) ->
    case aws_auth_validate_ldap_query:is_evaluable(DN) of
        true -> Probe(dn_str(DN));
        false -> skip
    end.

%% Membership probe mirroring rabbit_auth_backend_ldap in_group/3: a base-scoped
%% search at the group DN with an equalityMatch on the membership attribute
%% (default "member") against the resolved user DN. A match means the user is a
%% member. Distinct from object_exists/2, which only proves the group EXISTS;
%% reusing that here would wrongly pass a real-but-non-member group. Any
%% non-{ok,_} collapses to false (never raises -- so a resolved secret cannot
%% reach a crash report).
eval_membership(_Handle, _DN, _Desc, undefined) ->
    %% No resolved principal in scope for this sub-result; degrade.
    skip;
eval_membership(Handle, DN, Desc, UserDn) ->
    eval_dn_probe(DN, fun(GroupDn) -> member_exists(Handle, GroupDn, Desc, UserDn) end).

member_exists(Handle, GroupDn, Desc, UserDn) ->
    case
        eldap:search(Handle, [
            {base, GroupDn},
            {filter, eldap:equalityMatch(Desc, UserDn)},
            {attributes, ["objectClass"]},
            {scope, eldap:baseObject()},
            {timeout, search_time_limit_seconds()}
        ])
    of
        {ok, #eldap_search_result{entries = Entries}} ->
            length(Entries) > 0;
        _ ->
            false
    end.

dn_str({string, Pattern}) -> dn_str(Pattern);
dn_str(DN) when is_binary(DN) -> binary_to_list(DN);
dn_str(DN) when is_list(DN) -> DN.

%% Base-scoped existence probe. Mirrors rabbit_auth_backend_ldap:object_exists/3
%% (base object, present(objectClass) filter) so a DN this returns true for is
%% one the broker's queries could also resolve. Any error -- not found,
%% referral, permission denied -- collapses to `false'; the caller maps that
%% to the single fixed authz_unverified category, leaking no LDAP detail.
object_exists(Handle, DN) ->
    case
        eldap:search(Handle, [
            {base, DN},
            {filter, eldap:present("objectClass")},
            {attributes, ["objectClass"]},
            {scope, eldap:baseObject()},
            {timeout, search_time_limit_seconds()}
        ])
    of
        {ok, #eldap_search_result{entries = Entries}} ->
            length(Entries) > 0;
        _ ->
            false
    end.

ssl_open_opts(true, TlsOpts) ->
    [{ssl, true}, {sslopts, TlsOpts}];
ssl_open_opts(false, _TlsOpts) ->
    [{ssl, false}].

maybe_start_tls(_Handle, false, _TlsOpts, _Timeout) ->
    ok;
maybe_start_tls(Handle, true, TlsOpts, Timeout) ->
    eldap:start_tls(Handle, TlsOpts, Timeout).

%% Translate the JSON ssl_options map into an Erlang ssl options proplist
%% suitable for eldap. Both the key surface AND every value are validated up
%% front in parse_ssl_options/2 (verify/depth/versions/server_name_indication
%% domains, cacertfile_arn shape), so a value reaching here is already known
%% good: the verify/depth/versions/sni translators below are total over the
%% validated domain and cannot fail on a mis-typed option -- that was already
%% rejected with input_invalid in the pure phase.
%%
%% This produces the raw translation only; the verify-mode default (and its
%% fail-loud no-anchor policy) is applied by build_tls_opts/2 via the shared
%% aws_auth_validate_ssl:apply_verify_default/3. Kept separate so the unit tests
%% can pin the translation independently of the trust-anchor policy.
%%
%% `hostname_verification' is intentionally NOT translated here: it is a control
%% input for the shared helper's hostname-match mode, not a direct ssl option, so
%% build_tls_opts/2 reads it and passes the resolved mode alongside.
%%
%% No `catch' here. The previous `(catch Fun(Value))' swallowed every error,
%% which the value-validation in parse_ssl_options/2 has now made unnecessary
%% and which hid bugs (the discouraged `catch' keyword).
%%
%% The CA cert is no longer resolved here: resolve_cacert/1 fetches and decodes
%% cacertfile_arn during the network phase and stores the decoded certs under
%% the atom `cacerts' key, so a resolve/decode failure is reported loud as
%% input_invalid (never a silent verify_none downgrade) and the ARN is resolved
%% exactly once. By the time we reach here the certs are already in hand; the
%% `cacerts' pair below just threads them through unchanged.
build_ssl_opts(Map) when is_map(Map) ->
    Pairs = [
        {cacerts, cacerts, fun(Certs) -> Certs end},
        {verify, <<"verify">>, fun aws_auth_validate_ssl:to_verify/1},
        {depth, <<"depth">>, fun aws_auth_validate_ssl:to_integer/1},
        {versions, <<"versions">>, fun aws_auth_validate_ssl:to_versions/1},
        {server_name_indication, <<"server_name_indication">>, fun aws_auth_validate_ssl:to_list/1}
    ],
    lists:foldl(
        fun({SslKey, JsonKey, Fun}, Acc) ->
            case maps:get(JsonKey, Map, undefined) of
                undefined -> Acc;
                Value -> add_ssl_opt(SslKey, Fun, Value, Acc)
            end
        end,
        [],
        Pairs
    ).

add_ssl_opt(SslKey, Fun, Value, Acc) ->
    case Fun(Value) of
        skip -> Acc;
        Translated -> [{SslKey, Translated} | Acc]
    end.

%% Build the final eldap ssl option list, applying the shared verify-mode policy.
%% TlsInUse is (use_ssl orelse use_starttls); on a plaintext request there is no
%% TLS to shape, so return an empty list unused by ssl_open_opts(false, _).
%%
%% The verify-mode default is delegated to the shared
%% aws_auth_validate_ssl:apply_verify_default/3, so the no-anchor policy matches
%% the http/oauth backends:
%%   * an EXPLICIT verify_peer with no trust anchor FAILS loud as tls_failed (never
%%     silently downgraded -- the false positive this endpoint exists to prevent),
%%   * a DEFAULTED verify_peer with no anchor downgrades to verify_none (broker
%%     parity -- a plain reachability probe still works without a CA store),
%%   * an explicit verify_none is untouched.
%% The hostname-check mode is passed explicitly (NOT the http/oauth wildcard
%% default): LDAP mirrors rabbit_auth_backend_ldap, which applies the RFC 6125
%% https match fun ONLY when auth_ldap.ssl_options.hostname_verification =
%% wildcard, and otherwise uses OTP strict matching. Defaulting to wildcard here
%% would pass a wildcard LDAPS cert the live broker rejects.
%% VerifyExplicit is whether the caller set `verify' themselves; it governs the
%% fail-vs-downgrade branch for the no-anchor case.
-spec build_tls_opts(boolean(), map()) ->
    {ok, list()} | {error, aws_auth_validate_backend:error_category(), binary()}.
build_tls_opts(false, _SslOpts) ->
    {ok, []};
build_tls_opts(true, SslOpts) ->
    Translated = build_ssl_opts(SslOpts),
    VerifyExplicit = maps:is_key(<<"verify">>, SslOpts),
    %% Shared with http/oauth: unset/`none' = strict, `wildcard' opts in.
    HostnameCheck = aws_auth_validate_ssl:hostname_check_mode(SslOpts),
    aws_auth_validate_ssl:apply_verify_default(Translated, VerifyExplicit, HostnameCheck).

%%--------------------------------------------------------------------

%% Resolve an ARN using the request's threaded aws_state() (shared helper). The
%% state is built once per request by resolve_request_state/1 under the
%% operator-configured assume_role. The resolved secret is neither logged nor
%% returned (no credential leakage); this runs only after the pure validation
%% pipeline (zero side effects -- no secret fetch for a malformed request).
resolve_arn(Arn, State) when is_binary(Arn) ->
    aws_auth_validate_ssl:resolve_arn(Arn, State).

connection_timeout_ms() ->
    aws_auth_validate_ssl:connection_timeout_ms(#{default => ?DEFAULT_TIMEOUT_MS, max => 60_000}).

%% Server-side LDAP search time limit, in whole seconds, for the post-bind
%% probes (dn_lookup, principal resolution, membership/existence). This is the
%% protocol-level timeLimit, which is server-COOPERATIVE: a compliant server
%% bounds its own search time, but a server that stalls at the TCP layer ignores
%% it -- only the eldap open/2 socket recv timeout bounds that case. It is a
%% best-effort budget, not a hard DoS cap. Derived from the same request-timeout
%% config (ms -> s, rounded up, floored at 1s) so it tracks the operator's
%% configured budget.
search_time_limit_seconds() ->
    max(1, (connection_timeout_ms() + 999) div 1000).
