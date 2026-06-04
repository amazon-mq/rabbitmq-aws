%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% LDAP simple-bind validation backend.
%%
%% Opens an ephemeral connection per request, optionally performs
%% start_tls, attempts a simple bind, then unconditionally closes the
%% handle. All outcomes collapse to one of five fixed error categories
%% so the HTTP response cannot be used to extract LDAP-server or
%% network-topology details. Credentials never appear in returned
%% reasons.
-module(aws_auth_validate_ldap).

-behaviour(aws_auth_validate_backend).

-export([method_name/0, validate/1, allowed_fields/0]).

-define(DEFAULT_TIMEOUT_MS, 5_000).

-define(REASON_BAD_SERVERS, <<"servers must be a non-empty list of non-empty strings">>).
-define(REASON_BAD_PORT, <<"port must be an integer in 1..65535">>).
-define(REASON_BAD_USER_DN, <<"user_dn must be a non-empty string">>).
-define(REASON_BAD_PASSWORD_ARN, <<"password_arn must be a non-empty string">>).
-define(REASON_BAD_SSL_FLAG, <<"use_ssl must be a boolean">>).
-define(REASON_BAD_STARTTLS_FLAG, <<"use_starttls must be a boolean">>).
-define(REASON_BAD_SSL_OPTIONS, <<"ssl_options must be an object">>).
-define(REASON_TLS_BOTH, <<"use_ssl and use_starttls are mutually exclusive">>).
-define(REASON_CONNECTION, <<"could not connect to LDAP server">>).
-define(REASON_TLS_HANDSHAKE, <<"TLS handshake failed">>).
-define(REASON_AUTH, <<"LDAP simple bind rejected the supplied credentials">>).
-define(REASON_ARN_RESOLVE, <<"failed to resolve ARN">>).

method_name() ->
    <<"ldap-simple-bind">>.

allowed_fields() ->
    [
        <<"servers">>,
        <<"port">>,
        <<"user_dn">>,
        <<"password_arn">>,
        <<"use_ssl">>,
        <<"use_starttls">>,
        <<"ssl_options">>
    ].

-spec validate(map()) -> aws_auth_validate_backend:result().
validate(Body) when is_map(Body) ->
    case parse_input(Body) of
        {error, _, _} = Err ->
            Err;
        {ok, Params} ->
            case check_config_conflicts(Params) of
                {error, _, _} = Err -> Err;
                ok -> do_ldap_validate(Params)
            end
    end.

%%--------------------------------------------------------------------
%% Input parsing
%%--------------------------------------------------------------------

parse_input(Body) ->
    Steps = [
        fun parse_servers/2,
        fun parse_port/2,
        fun parse_user_dn/2,
        fun parse_password/2,
        fun parse_use_ssl/2,
        fun parse_use_starttls/2,
        fun parse_ssl_options/2
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

parse_password(Body, Acc) ->
    case maps:get(<<"password_arn">>, Body, undefined) of
        Arn when is_binary(Arn), byte_size(Arn) > 0 ->
            case resolve_arn(Arn) of
                {ok, Password} -> {ok, Acc#{password => Password}};
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
            {ok, Acc#{ssl_options => Map}};
        _ ->
            {error, input_invalid, ?REASON_BAD_SSL_OPTIONS}
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

do_ldap_validate(#{
    servers := Servers,
    port := Port,
    user_dn := UserDn,
    password := Password,
    use_ssl := UseSsl,
    use_starttls := UseStartTls,
    ssl_options := SslOpts,
    timeout := Timeout
}) ->
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
                            ok -> ok;
                            {error, _Reason} -> {error, auth_failed, ?REASON_AUTH}
                        end
                end
            after
                catch eldap:close(Handle)
            end
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
%% proplist suitable for eldap. Unknown keys are ignored to keep the
%% allowed surface narrow.
build_ssl_opts(Map) when is_map(Map) ->
    Pairs = [
        {cacerts, <<"cacertfile_arn">>, fun resolve_and_decode_pem_cacerts/1},
        {verify, <<"verify">>, fun to_atom/1},
        {depth, <<"depth">>, fun to_integer/1},
        {versions, <<"versions">>, fun to_versions/1},
        {server_name_indication, <<"server_name_indication">>, fun to_list/1}
    ],
    lists:foldl(
        fun({SslKey, JsonKey, Fun}, Acc) ->
            case maps:get(JsonKey, Map, undefined) of
                undefined -> Acc;
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
    ).

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
        Entries ->
            [public_key:pem_entry_decode(E) || E <- Entries]
    end.

resolve_and_decode_pem_cacerts(Arn) when is_binary(Arn) ->
    case resolve_arn(Arn) of
        {ok, PemData} -> decode_pem_cacerts(PemData);
        {error, _} -> skip
    end.

%%--------------------------------------------------------------------

resolve_arn(Arn) when is_binary(Arn) ->
    aws_arn_util:resolve_arn(binary_to_list(Arn)).

connection_timeout_ms() ->
    case application:get_env(aws, auth_validation_connection_timeout_ms) of
        {ok, Ms} when is_integer(Ms), Ms > 0 -> Ms;
        _ -> ?DEFAULT_TIMEOUT_MS
    end.
