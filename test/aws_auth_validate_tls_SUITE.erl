%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Integration tests for PUT /api/aws/auth/validate/tls against a real broker.
%%
%% The full request pipeline runs end to end (mgmt auth, opt-in gate, registry
%% dispatch, the tls backend, ARN resolution + cert checks, and the handler's
%% category-to-status mapping). Only aws_iam:assume_role and
%% aws_arn_util:resolve_arn are mocked on the broker node; everything else is
%% production code.
%%
%% No external dependency -- the tls method makes no outbound connection, so a
%% broker node plus openssl is all this needs.
-module(aws_auth_validate_tls_SUITE).

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    tls_disabled_by_default_returns_404/1,
    tls_valid_ca_returns_204/1,
    tls_expired_ca_returns_400_tls_failed/1,
    tls_malformed_pem_returns_400_input_invalid/1,
    tls_no_certs_returns_400_input_invalid/1,
    tls_missing_cacert_arn_returns_400/1,
    tls_no_assume_role_returns_422_config_conflict/1,
    tls_response_no_ca_material/1,
    tls_chain_valid_returns_204/1,
    tls_chain_invalid_returns_422/1,
    tls_cert_login_conflict_returns_422/1
]).

%% Invoked on the broker node via rpc to install/remove the AWS-boundary mocks.
-export([mock_resolve/1, unmock_resolve/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbitmq_ct_helpers/include/rabbit_mgmt_test.hrl").
-include("aws.hrl").

-define(API, "/aws/auth/validate/tls").
-define(ROLE_ARN, "arn:aws:iam::123456789012:role/validation").
-define(CACERT_ARN, <<"arn:aws:s3:::test-ca/ca.pem">>).

all() ->
    [
        {group, tls_method}
    ].

groups() ->
    [
        {tls_method, [], [
            %% tls is opt-in: with the feature on but the method not enabled it
            %% must 404 (enabling ldap/the feature toggle does not bring it live).
            tls_disabled_by_default_returns_404,
            %% The method enabled + a valid, in-window CA -> 204.
            tls_valid_ca_returns_204,
            %% An expired CA in the resolved bundle -> 400 tls_failed.
            tls_expired_ca_returns_400_tls_failed,
            %% A cert-framed but malformed-base64 PEM -> 400 input_invalid (the
            %% decode raises internally; the pipeline must not 500).
            tls_malformed_pem_returns_400_input_invalid,
            %% A PEM with no certificate entries -> 400 input_invalid.
            tls_no_certs_returns_400_input_invalid,
            %% ssl_options without cacertfile_arn -> 400 input_invalid (pure).
            tls_missing_cacert_arn_returns_400,
            %% cacertfile_arn referenced but no assume_role configured -> 422.
            tls_no_assume_role_returns_422_config_conflict,
            %% resolved CA material must not appear in the response.
            tls_response_no_ca_material,
            %% Client cert chain validates against the CA bundle -> 204.
            tls_chain_valid_returns_204,
            %% Client cert not chaining to the CA -> 422 auth_failed.
            tls_chain_invalid_returns_422,
            %% cert_login without client_cert -> 422 config_conflict.
            tls_cert_login_conflict_returns_422
        ]}
    ].

%%--------------------------------------------------------------------
%% Suite / group setup
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = inets:start(),
    rabbit_ct_helpers:log_environment(),
    %% Generate the CA fixtures once for the whole suite (on the CT node); the
    %% PEM binaries are threaded to the broker-node mock via rpc per testcase.
    ValidCaPem = gen_ca_pem(Config, "valid", "-days 2"),
    ExpiredCaPem = gen_expired_ca_pem(Config),
    %% Generate a leaf cert signed by the same valid CA (for chain validation cases).
    ClientLeafPem = gen_leaf_cert_pem(Config, "client-leaf", "valid"),
    %% Generate an unrelated leaf cert (signed by a different CA) for the invalid case.
    _ = gen_ca_pem(Config, "other", "-days 2"),
    UnrelatedLeafPem = gen_leaf_cert_pem(Config, "unrelated-leaf", "other"),
    [
        {valid_ca_pem, ValidCaPem},
        {expired_ca_pem, ExpiredCaPem},
        {client_leaf_pem, ClientLeafPem},
        {unrelated_leaf_pem, UnrelatedLeafPem}
        | Config
    ].

end_per_suite(Config) ->
    inets:stop(),
    Config.

init_per_group(tls_method, Config) ->
    %% Feature on; the tls METHOD is left at its opt-in default (disabled) so the
    %% disabled-by-default case sees a 404. Cases that need it enable it in
    %% init_per_testcase. A boot-time assume_role is configured for the whole
    %% group (the resolving cases need it); the config_conflict case unsets it.
    setup_broker(Config, [
        {auth_validation_enabled, true},
        {auth_validation_max_concurrent, 1},
        {auth_validation_max_body_size, 65536},
        {arn_config, [{assume_role_arn, ?ROLE_ARN}]}
    ]).

end_per_group(_Group, Config) ->
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_broker_helpers:teardown_steps()
    ).

%%--------------------------------------------------------------------
%% Per-testcase wiring
%%--------------------------------------------------------------------

%% The disabled-by-default case must NOT enable the method and needs no mock.
init_per_testcase(tls_disabled_by_default_returns_404 = TC, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TC);
%% The missing-cacert and no-assume-role cases fail before any ARN is resolved,
%% so they need the method enabled but no resolve mock.
init_per_testcase(tls_missing_cacert_arn_returns_400 = TC, Config) ->
    enable_tls_method(Config),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(tls_no_assume_role_returns_422_config_conflict = TC, Config) ->
    enable_tls_method(Config),
    %% Remove the configured assume_role so a referenced cacertfile_arn is
    %% refused with config_conflict rather than resolved under the instance role.
    rabbit_ct_broker_helpers:rpc(Config, 0, application, unset_env, [aws, arn_config]),
    rabbit_ct_helpers:testcase_started(Config, TC);
%% The cert_login conflict case fails at input validation (pure); no mock needed.
init_per_testcase(tls_cert_login_conflict_returns_422 = TC, Config) ->
    enable_tls_method(Config),
    rabbit_ct_helpers:testcase_started(Config, TC);
%% Every other case enables the method AND installs a resolve mock returning the
%% appropriate fixture PEM.
init_per_testcase(TC, Config) ->
    enable_tls_method(Config),
    Pem = pem_for_case(TC, Config),
    ok = rabbit_ct_broker_helpers:rpc(Config, 0, ?MODULE, mock_resolve, [Pem]),
    rabbit_ct_helpers:testcase_started(Config, TC).

end_per_testcase(tls_disabled_by_default_returns_404 = TC, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(tls_missing_cacert_arn_returns_400 = TC, Config) ->
    disable_methods(Config),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(tls_cert_login_conflict_returns_422 = TC, Config) ->
    disable_methods(Config),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(tls_no_assume_role_returns_422_config_conflict = TC, Config) ->
    disable_methods(Config),
    %% Restore the group-level assume_role for the remaining cases.
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, set_env, [aws, arn_config, [{assume_role_arn, ?ROLE_ARN}]]
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(TC, Config) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0, ?MODULE, unmock_resolve, []),
    disable_methods(Config),
    rabbit_ct_helpers:testcase_finished(Config, TC).

%% The PEM the resolve mock should return for each resolving case.
pem_for_case(tls_expired_ca_returns_400_tls_failed, Config) ->
    ?config(expired_ca_pem, Config);
pem_for_case(tls_malformed_pem_returns_400_input_invalid, _Config) ->
    %% Cert framing with a non-base64 body: public_key:pem_decode/1 raises on
    %% this, which the backend must catch and report as input_invalid.
    <<"-----BEGIN CERTIFICATE-----\nnot base64 %%%\n-----END CERTIFICATE-----\n">>;
pem_for_case(tls_no_certs_returns_400_input_invalid, _Config) ->
    %% Well-formed PEM with no certificate entries: public_key:pem_decode/1
    %% decodes the body but yields zero certificates (the `skip' branch), as
    %% opposed to the malformed-base64 case above, which raises.
    <<"-----BEGIN PRIVATE KEY-----\naGVsbG8=\n-----END PRIVATE KEY-----\n">>;
pem_for_case(tls_chain_valid_returns_204, Config) ->
    ?config(valid_ca_pem, Config);
pem_for_case(tls_chain_invalid_returns_422, Config) ->
    ?config(valid_ca_pem, Config);
pem_for_case(_TC, Config) ->
    %% Default (valid CA) for tls_valid_ca_returns_204 / tls_response_no_ca_material.
    ?config(valid_ca_pem, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

tls_disabled_by_default_returns_404(Config) ->
    {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, valid_body()),
    ?assertEqual(404, Code).

tls_valid_ca_returns_204(Config) ->
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?API, valid_body()),
    ?assertEqual(204, Code),
    ?assertEqual(<<>>, iolist_to_binary(ResBody)).

tls_expired_ca_returns_400_tls_failed(Config) ->
    case ?config(expired_ca_pem, Config) of
        skip ->
            {skip, "openssl lacks -not_before/-not_after; expired fixture unavailable"};
        _ ->
            {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, valid_body()),
            ?assertEqual(400, Code),
            ?assertMatch(#{<<"error">> := <<"tls_failed">>}, decode(ResBody))
    end.

tls_malformed_pem_returns_400_input_invalid(Config) ->
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, valid_body()),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"input_invalid">>}, decode(ResBody)).

tls_no_certs_returns_400_input_invalid(Config) ->
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, valid_body()),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"input_invalid">>}, decode(ResBody)).

tls_missing_cacert_arn_returns_400(Config) ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_peer">>}
    },
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"input_invalid">>}, decode(ResBody)).

tls_no_assume_role_returns_422_config_conflict(Config) ->
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, valid_body()),
    ?assertEqual(422, Code),
    ?assertMatch(#{<<"error">> := <<"config_conflict">>}, decode(ResBody)).

%% The resolved CA PEM must not appear in the response, even on success.
tls_response_no_ca_material(Config) ->
    Pem = ?config(valid_ca_pem, Config),
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, valid_body()),
    ?assertEqual(204, Code),
    Body = iolist_to_binary(ResBody),
    Needle = pem_body_slice(Pem),
    ?assertEqual(nomatch, binary:match(Body, Needle)).

%% A client certificate that chains to the resolved CA bundle passes (204).
%% This exercises Layer 1 (chain validation) without cert_login (chain-only mode).
tls_chain_valid_returns_204(Config) ->
    ClientPem = ?config(client_leaf_pem, Config),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"client_cert">> => ClientPem
    },
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(204, Code),
    ?assertEqual(<<>>, iolist_to_binary(ResBody)).

%% A client certificate signed by a different CA does not chain -> 422 auth_failed.
tls_chain_invalid_returns_422(Config) ->
    UnrelatedPem = ?config(unrelated_leaf_pem, Config),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"client_cert">> => UnrelatedPem
    },
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(422, Code),
    ?assertMatch(#{<<"error">> := <<"auth_failed">>}, decode(ResBody)).

%% cert_login without client_cert is a cross-field conflict -> 422.
tls_cert_login_conflict_returns_422(Config) ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"cert_login">> => #{<<"from">> => <<"common_name">>}
    },
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(422, Code),
    ?assertMatch(#{<<"error">> := <<"config_conflict">>}, decode(ResBody)).

%%--------------------------------------------------------------------
%% Broker-node mocks (invoked via rpc)
%%--------------------------------------------------------------------

%% Runs on the broker node. assume_role passes through; resolve_arn returns the
%% fixture PEM. meck is loaded by setup_meck/1 in setup_broker/2.
mock_resolve(Pem) ->
    ok = meck:new(aws_iam, [passthrough, no_link]),
    ok = meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
    ok = meck:new(aws_arn_util, [passthrough, no_link]),
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, Pem, State} end),
    ok.

unmock_resolve() ->
    catch meck:unload(aws_arn_util),
    catch meck:unload(aws_iam),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

enable_tls_method(Config) ->
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, set_env, [aws, auth_validation_enabled_methods, [{<<"tls">>, true}]]
    ).

disable_methods(Config) ->
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, unset_env, [aws, auth_validation_enabled_methods]
    ).

valid_body() ->
    #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        }
    }.

setup_broker(Config0, ExtraEnv) ->
    Config1 = rabbit_ct_helpers:set_config(Config0, [
        {rmq_nodename_suffix, ?MODULE}
    ]),
    Config2 = rabbit_ct_helpers:merge_app_env(Config1, {aws, ExtraEnv}),
    Config3 = rabbit_ct_helpers:run_setup_steps(
        Config2,
        rabbit_ct_broker_helpers:setup_steps()
    ),
    case Config3 of
        {skip, _} = Skip ->
            Skip;
        _ ->
            ok = rabbit_ct_broker_helpers:setup_meck(Config3),
            Config3
    end.

put_request(Config, Path, BodyMap) ->
    Body = binary_to_list(rabbit_json:encode(BodyMap)),
    rabbit_mgmt_test_util:req(
        Config,
        0,
        put,
        Path,
        [
            rabbit_mgmt_test_util:auth_header("guest", "guest"),
            {"content-type", "application/json"}
        ],
        Body
    ).

decode(ResBody) ->
    case rabbit_json:try_decode(iolist_to_binary(ResBody)) of
        {ok, Map} -> Map;
        _ -> #{}
    end.

%% A distinctive interior slice of a PEM body, used as a no-leak check needle.
pem_body_slice(Pem) ->
    Lines = binary:split(Pem, <<"\n">>, [global]),
    %% Pick a base64 body line (not a BEGIN/END marker, non-trivial length).
    case [L || L <- Lines, byte_size(L) >= 16, binary:match(L, <<"-----">>) =:= nomatch] of
        [Line | _] -> Line;
        [] -> Pem
    end.

%%--------------------------------------------------------------------
%% Certificate fixtures (openssl)
%%--------------------------------------------------------------------

%% Generate a self-signed CA PEM with the given validity flag (e.g. "-days 2").
gen_ca_pem(Config, Name, ValidityFlag) ->
    PrivDir = ?config(priv_dir, Config),
    Key = filename:join(PrivDir, Name ++ "-ca-key.pem"),
    Cert = filename:join(PrivDir, Name ++ "-ca-cert.pem"),
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
            "~s -subj /CN=AwsAuthValidateTls~sCA 2>/dev/null",
            [Key, Cert, ValidityFlag, Name]
        )
    ),
    _ = os:cmd(Cmd),
    true = filelib:is_regular(Cert),
    {ok, Pem} = file:read_file(Cert),
    Pem.

%% Generate a CA whose validity window is entirely in the past. Returns `skip'
%% if this openssl lacks -not_before/-not_after (the CT case then skips).
gen_expired_ca_pem(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Key = filename:join(PrivDir, "expired-ca-key.pem"),
    Cert = filename:join(PrivDir, "expired-ca-cert.pem"),
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
            "-not_before 20200101000000Z -not_after 20200102000000Z "
            "-subj /CN=AwsAuthValidateTlsExpiredCA 2>&1",
            [Key, Cert]
        )
    ),
    Out = os:cmd(Cmd),
    case filelib:is_regular(Cert) andalso not has_error(Out) of
        true ->
            {ok, Pem} = file:read_file(Cert),
            Pem;
        false ->
            skip
    end.

%% Generate a leaf cert signed by the named CA fixture. Returns a cert-only PEM
%% (no private key material).
gen_leaf_cert_pem(Config, Name, CaName) ->
    PrivDir = ?config(priv_dir, Config),
    CaKey = filename:join(PrivDir, CaName ++ "-ca-key.pem"),
    CaCert = filename:join(PrivDir, CaName ++ "-ca-cert.pem"),
    LeafKey = filename:join(PrivDir, Name ++ "-key.pem"),
    LeafCsr = filename:join(PrivDir, Name ++ ".csr"),
    LeafCert = filename:join(PrivDir, Name ++ "-cert.pem"),
    %% Generate leaf key + CSR.
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
                "-subj /CN=~s 2>/dev/null",
                [LeafKey, LeafCsr, Name]
            )
        )
    ),
    %% Sign leaf with the named CA.
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
                "-out ~ts -days 2 2>/dev/null",
                [LeafCsr, CaCert, CaKey, LeafCert]
            )
        )
    ),
    true = filelib:is_regular(LeafCert),
    {ok, Pem} = file:read_file(LeafCert),
    Pem.

has_error(Out) ->
    string:find(Out, "error") =/= nomatch orelse
        string:find(Out, "usage") =/= nomatch orelse
        string:find(Out, "unknown option") =/= nomatch.
