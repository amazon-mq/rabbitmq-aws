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

-include_lib("eldap/include/eldap.hrl").

-define(DEFAULT_TIMEOUT_MS, 5_000).

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
    <<"server_name_indication">>
]).

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
-define(REASON_BAD_PORT, <<"port must be an integer in 1..65535">>).
-define(REASON_BAD_USER_DN, <<"user_dn must be a non-empty string">>).
-define(REASON_BAD_PASSWORD_ARN, <<"password_arn must be a non-empty string">>).
-define(REASON_BAD_SSL_FLAG, <<"use_ssl must be a boolean">>).
-define(REASON_BAD_STARTTLS_FLAG, <<"use_starttls must be a boolean">>).
-define(REASON_BAD_SSL_OPTIONS, <<"ssl_options must be an object">>).
-define(REASON_UNKNOWN_SSL_OPTION, <<
    "ssl_options contains an unknown key; allowed keys are cacertfile_arn, "
    "verify, depth, versions, server_name_indication"
>>).
-define(REASON_BAD_SSL_VERIFY, <<"ssl_options.verify must be verify_peer or verify_none">>).
-define(REASON_BAD_SSL_DEPTH, <<"ssl_options.depth must be a non-negative integer">>).
-define(REASON_BAD_SSL_VERSIONS, <<"ssl_options.versions must be a list of known TLS versions">>).
-define(REASON_BAD_SSL_SNI, <<"ssl_options.server_name_indication must be a string">>).
-define(REASON_BAD_SSL_CACERT_ARN, <<"ssl_options.cacertfile_arn must be a non-empty string">>).
-define(REASON_TLS_BOTH, <<"use_ssl and use_starttls are mutually exclusive">>).
-define(REASON_CONNECTION, <<"could not connect to LDAP server">>).
-define(REASON_TLS_HANDSHAKE, <<"TLS handshake failed">>).
-define(REASON_AUTH, <<"LDAP simple bind rejected the supplied credentials">>).
-define(REASON_ARN_RESOLVE, <<"failed to resolve ARN">>).
-define(REASON_BAD_DN_LOOKUP_BASE, <<"dn_lookup_base must be a non-empty string">>).
-define(REASON_BAD_DN_LOOKUP_ATTR, <<"dn_lookup_attribute must be a non-empty string">>).
-define(REASON_BAD_QUERIES, <<"queries must be an object of query strings">>).
-define(REASON_BAD_QUERY_VALUE, <<"each query must be a non-empty string">>).
-define(REASON_QUERY_PARSE, <<"one or more authorization queries are not valid">>).
-define(REASON_DN_LOOKUP_BASE_UNVERIFIED,
    <<"dn_lookup_base does not exist or is not readable by the bind user">>
).
-define(REASON_AUTHZ_UNVERIFIED,
    <<"a DN referenced by an authorization query could not be verified">>
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
                    case resolve_password(Body, Params) of
                        {error, _, _} = Err -> Err;
                        {ok, Params1} -> do_ldap_validate(Params1)
                    end
            end
    end.

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
                true -> {ok, Acc#{servers => [binary_to_list(S) || S <- Servers]}};
                false -> {error, input_invalid, ?REASON_BAD_SERVERS}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_SERVERS}
    end.

parse_port(Body, Acc) ->
    case maps:get(<<"port">>, Body, undefined) of
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
resolve_password(Body, Params) ->
    case maps:get(<<"password_arn">>, Body, undefined) of
        Arn when is_binary(Arn), byte_size(Arn) > 0 ->
            case resolve_arn(Arn) of
                {ok, Password} -> {ok, Params#{password => Password}};
                {error, _} -> {error, input_invalid, ?REASON_ARN_RESOLVE}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_PASSWORD_ARN}
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

%% SECURITY (R6): the resolved bind password is passed to eldap:simple_bind/3
%% as a direct argument. If anything in the connect/bind/post-bind section
%% *raises* (rather than returning {error, _}), the exception's stacktrace
%% would carry the live argument terms -- including this function's Params map,
%% which holds the plaintext password -- into a Cowboy crash report. To
%% guarantee the password can never reach a log or crash dump, the entire body
%% is destructured into a separate worker (do_ldap_bind/1) whose only job is to
%% return a fixed-category result, and any escaping exception is caught here and
%% collapsed to the fixed connection_failed category. We deliberately discard
%% the caught class/reason/stacktrace (binding them to throwaway names that are
%% never logged or returned) so no fragment of the bind arguments survives.
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

do_ldap_bind(
    #{
        servers := Servers,
        port := Port,
        user_dn := UserDn,
        password := Password,
        use_ssl := UseSsl,
        use_starttls := UseStartTls,
        ssl_options := SslOpts,
        timeout := Timeout
    } = Params
) ->
    OpenOpts = [{port, Port}, {timeout, Timeout}] ++ ssl_open_opts(UseSsl, SslOpts),
    case eldap:open(Servers, OpenOpts) of
        {error, _Reason} ->
            {error, connection_failed, ?REASON_CONNECTION};
        {ok, Handle} ->
            try
                case maybe_start_tls(Handle, UseStartTls, SslOpts, Timeout) of
                    {error, _Reason} ->
                        {error, tls_failed, ?REASON_TLS_HANDSHAKE};
                    ok ->
                        case eldap:simple_bind(Handle, UserDn, Password) of
                            ok -> post_bind_checks(Handle, Params);
                            {error, _Reason} -> {error, auth_failed, ?REASON_AUTH}
                        end
                end
            after
                catch eldap:close(Handle)
            end
    end.

%% After a successful bind, run the optional DN-lookup and authorization
%% checks in sequence on the same bound handle. The first failure wins; if
%% none are configured (or all pass) the request succeeds with `ok'.
post_bind_checks(Handle, Params) ->
    case check_dn_lookup(Handle, Params) of
        ok -> check_authz_queries(Handle, Params);
        {error, _, _} = Err -> Err
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

%% For each parsed query, extract the fully-literal DNs (those with no
%% ${...} runtime placeholder) and confirm each exists and is readable by
%% the bound user. Queries that reference only runtime-filled DNs contribute
%% no checks -- they were already validated for grammar at parse time.
check_authz_queries(Handle, #{queries := Queries}) ->
    LiteralDns = lists:usort(
        lists:flatmap(
            fun({_Name, Query}) -> aws_auth_validate_ldap_query:literal_dns(Query) end,
            Queries
        )
    ),
    check_dns(Handle, LiteralDns).

check_dns(_Handle, []) ->
    ok;
check_dns(Handle, [DN | Rest]) ->
    case object_exists(Handle, DN) of
        true -> check_dns(Handle, Rest);
        _ -> {error, authz_unverified, ?REASON_AUTHZ_UNVERIFIED}
    end.

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
            {scope, eldap:baseObject()}
        ])
    of
        {ok, #eldap_search_result{entries = Entries}} ->
            length(Entries) > 0;
        _ ->
            false
    end.

ssl_open_opts(true, SslOpts) ->
    [{ssl, true}, {sslopts, build_ssl_opts(SslOpts)}];
ssl_open_opts(false, _SslOpts) ->
    [{ssl, false}].

maybe_start_tls(_Handle, false, _SslOpts, _Timeout) ->
    ok;
maybe_start_tls(Handle, true, SslOpts, Timeout) ->
    eldap:start_tls(Handle, build_ssl_opts(SslOpts), Timeout).

%% Translate the JSON ssl_options map into an Erlang ssl options
%% proplist suitable for eldap. The accepted key surface is validated up
%% front in parse_ssl_options/2, so any key reaching here is known.
build_ssl_opts(Map) when is_map(Map) ->
    Pairs = [
        {cacerts, <<"cacertfile_arn">>, fun resolve_and_decode_pem_cacerts/1},
        {verify, <<"verify">>, fun to_atom/1},
        {depth, <<"depth">>, fun to_integer/1},
        {versions, <<"versions">>, fun to_versions/1},
        {server_name_indication, <<"server_name_indication">>, fun to_list/1}
    ],
    Translated = lists:foldl(
        fun({SslKey, JsonKey, Fun}, Acc) ->
            case maps:get(JsonKey, Map, undefined) of
                undefined ->
                    Acc;
                Value ->
                    case (catch Fun(Value)) of
                        {'EXIT', _} -> Acc;
                        skip -> Acc;
                        Translated -> [{SslKey, Translated} | Acc]
                    end
            end
        end,
        [],
        Pairs
    ),
    apply_verify_default(Translated).

%% OTP's ssl defaults `verify' to verify_none, which silently accepts any
%% certificate -- insecure, and a poor thing to validate a config against.
%% When the caller did not specify `verify', prefer verify_peer -- BUT only
%% when a trust anchor is actually available to verify against (the caller's
%% cacertfile_arn, or the validator host's OS trust store). Forcing
%% verify_peer with no CA would make the handshake fail with unknown_ca and
%% report tls_failed/connection_failed for a config the real broker (default
%% verify_none) would accept -- a host-environment artifact, not a customer
%% config error, breaking decision parity. With no trust source we therefore
%% leave `verify' unset (broker-parity verify_none). An explicit `verify'
%% from the caller is always left untouched (verify_none stays opt-in).
apply_verify_default(Opts) ->
    case lists:keymember(verify, 1, Opts) of
        true ->
            Opts;
        false ->
            case trust_source(Opts) of
                {ok, Opts1} -> [{verify, verify_peer} | Opts1];
                none -> Opts
            end
    end.

%% Returns {ok, OptsWithCacerts} when a trust anchor is available (either the
%% caller already supplied cacerts, or the OS trust store is non-empty, in
%% which case it is added), or `none' when nothing can verify the peer.
trust_source(Opts) ->
    case lists:keymember(cacerts, 1, Opts) of
        true ->
            {ok, Opts};
        false ->
            case os_cacerts() of
                [] -> none;
                Certs -> {ok, [{cacerts, Certs} | Opts]}
            end
    end.

os_cacerts() ->
    try
        public_key:cacerts_get()
    catch
        _:_ -> []
    end.

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

to_atom(B) when is_binary(B) -> binary_to_existing_atom(B, utf8);
to_atom(A) when is_atom(A) -> A.

to_integer(I) when is_integer(I) -> I.

to_versions(L) when is_list(L) ->
    [to_atom(V) || V <- L].

decode_pem_cacerts(B) when is_binary(B) ->
    case public_key:pem_decode(B) of
        [] -> skip;
        Entries -> [public_key:pem_entry_decode(E) || E <- Entries]
    end.

resolve_and_decode_pem_cacerts(Arn) when is_binary(Arn) ->
    case resolve_arn(Arn) of
        {ok, PemData} -> decode_pem_cacerts(PemData);
        {error, _} -> skip
    end.

%%--------------------------------------------------------------------

%% ARN resolution mutates the shared rabbitmq_aws region/credential
%% singleton (aws_sms/aws_acm_pca call rabbitmq_aws:set_region/1), so
%% serialize it across concurrent validation requests to prevent region
%% clobbering between a set_region and its HTTP call. See
%% aws_auth_validate_arn_lock.
resolve_arn(Arn) when is_binary(Arn) ->
    aws_auth_validate_arn_lock:with_lock(fun() ->
        aws_arn_util:resolve_arn(binary_to_list(Arn))
    end).

connection_timeout_ms() ->
    case application:get_env(aws, auth_validation_connection_timeout_ms) of
        {ok, Ms} when is_integer(Ms), Ms > 0 -> Ms;
        _ -> ?DEFAULT_TIMEOUT_MS
    end.
