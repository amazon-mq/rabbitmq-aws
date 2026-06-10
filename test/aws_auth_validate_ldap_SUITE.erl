%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Integration tests for the LDAP validation backend against a real
%% slapd(8) directory. These exercise the parts of aws_auth_validate_ldap
%% that the unit tests cannot: the actual eldap open -> (start_tls) ->
%% simple_bind -> post-bind search path.
%%
%% slapd is provided by the init-slapd.sh harness borrowed from
%% rabbitmq_auth_backend_ldap (a TEST_DEP). On Linux/FreeBSD the script
%% launches slapd locally; on macOS it expects an slapd already listening
%% on the chosen port (e.g. an OpenLDAP container). If no server can be
%% reached the whole suite is skipped rather than failing, so a developer
%% machine without slapd still gets a green (skipped) run.
%%
%% The directory is seeded directly over eldap as the rootdn, so the suite
%% is self-contained and does not depend on the backend dep's seed module.
%%
%% password_arn resolution is mocked with meck: aws_arn_util:resolve_arn/1
%% is a separately-tested AWS concern, so here it simply maps a fake ARN to
%% a known password. The real eldap bind still runs against real slapd.
-module(aws_auth_validate_ldap_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("eldap/include/eldap.hrl").

%% Directory layout we seed (mirrors the backend dep's base DN so the
%% R12 parity comparison is against a realistic tree).
-define(BASE_DN, "dc=rabbitmq,dc=com").
-define(ADMIN_DN, "cn=admin,dc=rabbitmq,dc=com").
-define(ADMIN_PW, "admin").
-define(PEOPLE_OU, "ou=people,dc=rabbitmq,dc=com").
-define(GROUPS_OU, "ou=groups,dc=rabbitmq,dc=com").

%% The bind ("service account") user the validation request authenticates as.
-define(BIND_DN, "cn=svc,ou=people,dc=rabbitmq,dc=com").
-define(BIND_PW, "svc-password").
%% A second user, used as a group member / DN-lookup target.
-define(ALICE_DN, "cn=alice,ou=people,dc=rabbitmq,dc=com").
-define(ALICE_PW, "alice-password").
%% A group that really exists (for authz query reachability checks).
-define(ADMINS_GROUP_DN, "cn=admins,ou=groups,dc=rabbitmq,dc=com").
%% A group that does NOT exist (for the negative authz case).
-define(MISSING_GROUP_DN, "cn=does-not-exist,ou=groups,dc=rabbitmq,dc=com").

%% Fake ARNs the meck'd resolver understands.
-define(BIND_PW_ARN, <<"arn:aws:secretsmanager:us-east-1:111111111111:secret:svc">>).
-define(WRONG_PW_ARN, <<"arn:aws:secretsmanager:us-east-1:111111111111:secret:wrong">>).
-define(UNRESOLVABLE_ARN, <<"arn:aws:secretsmanager:us-east-1:111111111111:secret:nope">>).

all() ->
    [
        {group, ldap}
    ].

groups() ->
    [
        {ldap, [], [
            bind_success_returns_ok,
            wrong_password_returns_auth_failed,
            wrong_dn_returns_auth_failed,
            unresolvable_arn_returns_input_invalid,
            unreachable_server_returns_connection_failed,
            config_conflict_returns_error,
            dn_lookup_base_exists_returns_ok,
            dn_lookup_base_missing_returns_authz_unverified,
            authz_query_existing_group_returns_ok,
            authz_query_missing_group_returns_authz_unverified,
            authz_query_runtime_placeholder_returns_ok,
            parity_with_backend_bind
        ]}
    ].

%%--------------------------------------------------------------------
%% Suite setup / teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    %% eldap is an OTP app; make sure it (and ssl/crypto for completeness)
    %% are available to this test node.
    {ok, _} = application:ensure_all_started(eldap),
    case start_slapd(Config) of
        {skip, _} = Skip ->
            Skip;
        Config1 ->
            try
                ok = seed(?config(ldap_port, Config1)),
                Config1
            catch
                Class:Reason:St ->
                    ct:pal("LDAP seed failed: ~p:~p~n~p", [Class, Reason, St]),
                    stop_slapd(Config1),
                    {skip, "Failed to seed slapd directory"}
            end
    end.

end_per_suite(Config) ->
    case ?config(ldap_port, Config) of
        undefined -> ok;
        Port ->
            catch delete_seed(Port),
            stop_slapd(Config)
    end,
    Config.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(TC, Config) ->
    %% Mock ARN resolution: real eldap bind, fake secret fetch.
    ok = meck:new(aws_arn_util, [passthrough, no_link]),
    ok = meck:expect(aws_arn_util, resolve_arn, fun mock_resolve_arn/1),
    rabbit_ct_helpers:testcase_started(Config, TC).

end_per_testcase(TC, Config) ->
    catch meck:unload(aws_arn_util),
    rabbit_ct_helpers:testcase_finished(Config, TC).

%% Resolve only the ARNs this suite knows about; everything else fails to
%% resolve (which the backend maps to input_invalid).
mock_resolve_arn(Arn) when is_list(Arn) ->
    mock_resolve_arn(list_to_binary(Arn));
mock_resolve_arn(?BIND_PW_ARN) -> {ok, list_to_binary(?BIND_PW)};
mock_resolve_arn(?WRONG_PW_ARN) -> {ok, <<"definitely-wrong">>};
mock_resolve_arn(_Other) -> {error, not_found}.

%%--------------------------------------------------------------------
%% Functional tests
%%--------------------------------------------------------------------

bind_success_returns_ok(Config) ->
    ?assertEqual(ok, validate(Config, base_body(Config))).

wrong_password_returns_auth_failed(Config) ->
    Body = (base_body(Config))#{<<"password_arn">> => ?WRONG_PW_ARN},
    ?assertMatch({error, auth_failed, _}, validate(Config, Body)).

wrong_dn_returns_auth_failed(Config) ->
    Body = (base_body(Config))#{
        <<"user_dn">> => <<"cn=nonexistent,ou=people,dc=rabbitmq,dc=com">>
    },
    ?assertMatch({error, auth_failed, _}, validate(Config, Body)).

unresolvable_arn_returns_input_invalid(Config) ->
    Body = (base_body(Config))#{<<"password_arn">> => ?UNRESOLVABLE_ARN},
    ?assertMatch({error, input_invalid, _}, validate(Config, Body)).

unreachable_server_returns_connection_failed(Config) ->
    %% Port 1 on localhost: nothing listens there, so eldap:open fails fast.
    Body = (base_body(Config))#{
        <<"servers">> => [<<"127.0.0.1">>],
        <<"port">> => 1
    },
    ?assertMatch({error, connection_failed, _}, validate(Config, Body)).

config_conflict_returns_error(Config) ->
    Body = (base_body(Config))#{
        <<"use_ssl">> => true,
        <<"use_starttls">> => true
    },
    ?assertMatch({error, config_conflict, _}, validate(Config, Body)).

dn_lookup_base_exists_returns_ok(Config) ->
    Body = (base_body(Config))#{
        <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
        <<"dn_lookup_attribute">> => <<"cn">>
    },
    ?assertEqual(ok, validate(Config, Body)).

dn_lookup_base_missing_returns_authz_unverified(Config) ->
    Body = (base_body(Config))#{
        <<"dn_lookup_base">> => <<"ou=nope,dc=rabbitmq,dc=com">>,
        <<"dn_lookup_attribute">> => <<"cn">>
    },
    ?assertMatch({error, authz_unverified, _}, validate(Config, Body)).

authz_query_existing_group_returns_ok(Config) ->
    Query = list_to_binary(
        "{in_group, \"" ++ ?ADMINS_GROUP_DN ++ "\"}"
    ),
    Body = (base_body(Config))#{
        <<"queries">> => #{<<"vhost_access">> => Query}
    },
    ?assertEqual(ok, validate(Config, Body)).

authz_query_missing_group_returns_authz_unverified(Config) ->
    Query = list_to_binary(
        "{in_group, \"" ++ ?MISSING_GROUP_DN ++ "\"}"
    ),
    Body = (base_body(Config))#{
        <<"queries">> => #{<<"vhost_access">> => Query}
    },
    ?assertMatch({error, authz_unverified, _}, validate(Config, Body)).

%% A query whose only DN contains a ${...} placeholder has no literal DN to
%% check, so it passes (grammar-validated, not directory-checked).
authz_query_runtime_placeholder_returns_ok(Config) ->
    Query = <<"{in_group, \"cn=${username},ou=groups,dc=rabbitmq,dc=com\"}">>,
    Body = (base_body(Config))#{
        <<"queries">> => #{<<"tags">> => Query}
    },
    ?assertEqual(ok, validate(Config, Body)).

%%--------------------------------------------------------------------
%% R12: parity with the broker's bind path
%%--------------------------------------------------------------------

%% For the same (server, port, DN, password), the endpoint's validate
%% result and a direct eldap simple_bind (the same primitive
%% rabbit_auth_backend_ldap uses) must agree on success/failure. We compare
%% against eldap directly rather than booting the full LDAP backend, which
%% keeps the test hermetic while still pinning the bind-level decision.
parity_with_backend_bind(Config) ->
    Port = ?config(ldap_port, Config),
    Cases = [
        {?BIND_DN, ?BIND_PW, expect_ok},
        {?BIND_DN, "definitely-wrong", expect_fail},
        {"cn=nonexistent,ou=people,dc=rabbitmq,dc=com", ?BIND_PW, expect_fail},
        {?ALICE_DN, ?ALICE_PW, expect_ok}
    ],
    [parity_case(Config, Port, C) || C <- Cases],
    ok.

parity_case(Config, Port, {Dn, Pw, _Expect}) ->
    %% Endpoint path (with the ARN mock returning Pw).
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_) -> {ok, list_to_binary(Pw)} end),
    Body = (base_body(Config))#{
        <<"user_dn">> => list_to_binary(Dn),
        <<"password_arn">> => ?BIND_PW_ARN
    },
    EndpointOk =
        case validate(Config, Body) of
            ok -> true;
            {error, auth_failed, _} -> false;
            Other -> ct:fail({unexpected_endpoint_result, Dn, Other})
        end,
    %% Direct eldap bind (the broker's primitive).
    BackendOk = direct_bind(Port, Dn, Pw),
    ?assertEqual(
        BackendOk,
        EndpointOk,
        lists:flatten(io_lib:format("bind parity mismatch for ~ts", [Dn]))
    ).

direct_bind(Port, Dn, Pw) ->
    {ok, H} = eldap:open(["localhost"], [{port, Port}]),
    Result = eldap:simple_bind(H, Dn, Pw),
    catch eldap:close(H),
    Result =:= ok.

%%--------------------------------------------------------------------
%% Request bodies
%%--------------------------------------------------------------------

%% Minimal valid request that binds as the service account over plaintext.
%% port is an integer (the backend's parse_port/2 requires is_integer/1).
base_body(Config) ->
    #{
        <<"servers">> => [<<"localhost">>],
        <<"port">> => ?config(ldap_port, Config),
        <<"user_dn">> => list_to_binary(?BIND_DN),
        <<"password_arn">> => ?BIND_PW_ARN
    }.

%% Drive the backend exactly as the registry would: validate/1 on a body
%% already filtered to allowed_fields. We don't go through HTTP here; the
%% HTTP pipeline is covered by aws_auth_validate_mgmt_SUITE.
validate(_Config, Body) ->
    aws_auth_validate_ldap:validate(Body).

%%--------------------------------------------------------------------
%% slapd lifecycle (borrowed harness)
%%--------------------------------------------------------------------

start_slapd(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    TcpPort = 25389,
    SlapdDir = filename:join([PrivDir, "openldap"]),
    InitSlapd = filename:join([DataDir, "init-slapd.sh"]),
    Cmd = [InitSlapd, SlapdDir, {"~b", [TcpPort]}],
    case rabbit_ct_helpers:exec(Cmd) of
        {ok, Stdout} ->
            case re:run(Stdout, "^SLAPD_PID=([0-9]+)$",
                        [{capture, all_but_first, list}, multiline]) of
                {match, [SlapdPid]} ->
                    ct:pal("slapd PID ~ts listening on ~b", [SlapdPid, TcpPort]),
                    rabbit_ct_helpers:set_config(
                        Config, [{slapd_pid, SlapdPid}, {ldap_port, TcpPort}]
                    );
                nomatch ->
                    {skip, "Could not parse slapd PID from init-slapd.sh"}
            end;
        Error ->
            ct:pal("init-slapd.sh failed: ~p", [Error]),
            _ = rabbit_ct_helpers:exec(["pkill", "-INT", "slapd"]),
            {skip, "Failed to initialize slapd(8) - is an LDAP server available?"}
    end.

stop_slapd(Config) ->
    case ?config(slapd_pid, Config) of
        undefined -> ok;
        "0" -> ok;  %% macOS: externally-managed server, leave it running
        Pid -> _ = rabbit_ct_helpers:exec(["kill", "-INT", Pid]), ok
    end.

%%--------------------------------------------------------------------
%% Directory seeding (over eldap, as rootdn)
%%--------------------------------------------------------------------

seed(Port) ->
    H = admin_connect(Port),
    try
        ok = add(H, ?BASE_DN, [
            {"objectClass", ["dcObject", "organization"]},
            {"dc", ["rabbitmq"]},
            {"o", ["Test"]}
        ]),
        ok = add(H, ?PEOPLE_OU, ou_attrs("people")),
        ok = add(H, ?GROUPS_OU, ou_attrs("groups")),
        ok = add(H, ?BIND_DN, person_attrs("svc", ?BIND_PW)),
        ok = add(H, ?ALICE_DN, person_attrs("alice", ?ALICE_PW)),
        ok = add(H, ?ADMINS_GROUP_DN, [
            {"objectClass", ["groupOfNames"]},
            {"cn", ["admins"]},
            {"member", [?BIND_DN, ?ALICE_DN]}
        ]),
        ok
    after
        catch eldap:close(H)
    end.

delete_seed(Port) ->
    H = admin_connect(Port),
    try
        [del(H, DN) || DN <- [
            ?ADMINS_GROUP_DN,
            ?ALICE_DN,
            ?BIND_DN,
            ?GROUPS_OU,
            ?PEOPLE_OU,
            ?BASE_DN
        ]],
        ok
    after
        catch eldap:close(H)
    end.

admin_connect(Port) ->
    {ok, H} = eldap:open(["localhost"], [{port, Port}]),
    ok = eldap:simple_bind(H, ?ADMIN_DN, ?ADMIN_PW),
    H.

ou_attrs(Name) ->
    [{"objectClass", ["top", "organizationalUnit"]}, {"ou", [Name]}].

person_attrs(Cn, Pw) ->
    [
        {"objectClass", ["person"]},
        {"cn", [Cn]},
        {"sn", [Cn]},
        {"userPassword", [Pw]}
    ].

add(H, DN, Attrs) ->
    case eldap:add(H, DN, Attrs) of
        ok -> ok;
        {error, entryAlreadyExists} -> ok;
        Other -> Other
    end.

del(H, DN) ->
    case eldap:delete(H, DN) of
        ok -> ok;
        {error, noSuchObject} -> ok;
        Other -> Other
    end.
