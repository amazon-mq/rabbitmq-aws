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
%% reached the suite fails rather than skips: this is an integration suite,
%% so a green run must mean the bind path actually executed. Running it
%% therefore requires slapd to be available.
%%
%% The directory is seeded directly over eldap as the rootdn, so the suite
%% is self-contained and does not depend on the backend dep's seed module.
%%
%% password_arn resolution is mocked with meck: aws_arn_util:resolve_arn/2
%% is a separately-tested AWS concern, so here it simply maps a fake ARN to
%% a known password (threading the passed aws_lib:aws_state() back unchanged).
%% The real eldap bind still runs against real slapd.
-module(aws_auth_validate_ldap_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Directory layout we seed (mirrors the backend dep's base DN so the
%% decision-parity comparison is against a realistic tree).
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
%% A group that really exists (for authz query reachability checks). Both svc
%% and alice are members.
-define(ADMINS_GROUP_DN, "cn=admins,ou=groups,dc=rabbitmq,dc=com").
%% A group that exists but does NOT include alice (only svc), for the negative
%% membership case.
-define(OTHERS_GROUP_DN, "cn=others,ou=groups,dc=rabbitmq,dc=com").
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
            username_in_group_returns_ok,
            username_in_group_placeholder_dn_returns_ok,
            username_not_in_group_returns_authz_unverified,
            username_unresolvable_returns_authz_unverified,
            username_runtime_placeholder_degrades_returns_ok,
            username_membership_parity_with_backend,
            parity_with_backend_bind
        ]}
    ].

%%--------------------------------------------------------------------
%% Suite setup / teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    %% run_setup_steps/1 installs the long_running_testsuite_monitor that
    %% init_per_testcase's rabbit_ct_helpers:testcase_started/2 sends to.
    %% Without it that send targets an `undefined' monitor, raises badarg on
    %% every case, auto-skips them all, and makes `make ct' exit non-zero.
    %% (It also subsumes the log_environment/0 call this used to make.)
    Config = rabbit_ct_helpers:run_setup_steps(Config0),
    %% eldap is an OTP app; make sure it (and ssl/crypto for completeness)
    %% are available to this test node.
    {ok, _} = application:ensure_all_started(eldap),
    case start_slapd(Config) of
        {skip, Reason} ->
            %% No slapd reachable. This is an integration suite: a green run
            %% must mean the bind path actually executed, so a missing server
            %% is a hard failure, not a skip. (Running it requires slapd --
            %% apt-installed on CI, an OpenLDAP container on macOS; see the
            %% module header.) Failing here keeps "green" meaning the same
            %% thing locally and on CI.
            ct:fail("slapd unreachable, cannot run LDAP suite: ~ts", [Reason]);
        Config1 ->
            %% slapd is up. Seed the directory -- but eldap:open/1 uses
            %% spawn_link, so a connection process that dies mid-seed sends a
            %% LINKED EXIT SIGNAL to this process. Without trap_exit that
            %% signal kills init_per_suite directly (it is not an exception,
            %% so the try/catch below would NOT catch it). Trap exits so a
            %% linked eldap death becomes a drainable message instead, and any
            %% seed exception surfaces through the catch below as a deliberate
            %% ct:fail -- a clean suite failure rather than an opaque crash.
            Prev = process_flag(trap_exit, true),
            try
                ok = seed(?config(ldap_port, Config1)),
                Config1
            catch
                Class:Reason:St ->
                    ct:pal("LDAP seed failed: ~p:~p~n~p", [Class, Reason, St]),
                    stop_slapd(Config1),
                    ct:fail("Failed to seed slapd directory")
            after
                %% Drain any linked-exit messages the seeding left behind so a
                %% late one cannot surface during a later testcase, then
                %% restore the original trap_exit setting.
                flush_exits(),
                process_flag(trap_exit, Prev)
            end
    end.

%% Drain pending {'EXIT', _, _} messages (from linked eldap connection
%% processes) so they don't leak into subsequent test execution.
flush_exits() ->
    receive
        {'EXIT', _, _} -> flush_exits()
    after 0 -> ok
    end.

end_per_suite(Config) ->
    case ?config(ldap_port, Config) of
        undefined ->
            ok;
        Port ->
            catch delete_seed(Port),
            stop_slapd(Config)
    end,
    %% Stop the long_running_testsuite_monitor that run_setup_steps/1 started.
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_group(_Group, Config) ->
    %% Allow private/loopback addresses for testing against local slapd.
    application:set_env(aws, auth_validation_allow_private_networks, true),
    %% Auth validation requires a configured assume_role
    %% (aws.arns.assume_role_arn); the endpoint refuses with config_conflict when
    %% none is set, and never falls back to the broker's ambient credentials.
    %% Configure one so the request reaches the bind path; aws_iam:assume_role is
    %% mocked per testcase so no real STS call is made.
    application:set_env(aws, arn_config, [
        {assume_role_arn, "arn:aws:iam::123456789012:role/validation"}
    ]),
    Config.

end_per_group(_Group, Config) ->
    application:unset_env(aws, auth_validation_allow_private_networks),
    application:unset_env(aws, arn_config),
    Config.

init_per_testcase(TC, Config) ->
    %% Mock ARN resolution: real eldap bind, fake secret fetch. resolve_arn/2
    %% now takes the threaded aws_lib:aws_state() and returns it in the success
    %% 3-tuple; the mock threads the passed State straight back. ARN resolution
    %% runs inline now (the old aws_auth_validate_arn_lock serialization of the
    %% rabbitmq_aws region singleton is gone -- there is no shared singleton
    %% under state threading), so there is nothing else to mock.
    ok = meck:new(aws_arn_util, [passthrough, no_link]),
    ok = meck:expect(aws_arn_util, resolve_arn, fun mock_resolve_arn/2),
    %% The configured assume_role (see init_per_group) is assumed before ARN
    %% resolution; stub it to thread the passed state straight back so no real
    %% STS call is made and the mocked resolve_arn/2 sees an unchanged state.
    ok = meck:new(aws_iam, [passthrough, no_link]),
    ok = meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
    rabbit_ct_helpers:testcase_started(Config, TC).

end_per_testcase(TC, Config) ->
    catch meck:unload(aws_iam),
    catch meck:unload(aws_arn_util),
    rabbit_ct_helpers:testcase_finished(Config, TC).

%% Resolve only the ARNs this suite knows about; everything else fails to
%% resolve (which the backend maps to input_invalid). The passed
%% aws_lib:aws_state() is threaded back unchanged in the success 3-tuple.
mock_resolve_arn(Arn, State) when is_list(Arn) ->
    mock_resolve_arn(list_to_binary(Arn), State);
mock_resolve_arn(?BIND_PW_ARN, State) ->
    {ok, list_to_binary(?BIND_PW), State};
mock_resolve_arn(?WRONG_PW_ARN, State) ->
    {ok, <<"definitely-wrong">>, State};
mock_resolve_arn(_Other, State) ->
    {error, not_found, State}.

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
%% check, so it passes (grammar-validated, not directory-checked). NOTE: this
%% case supplies NO username, so it exercises the unchanged literal-DN path --
%% a backward-compatibility guard for the new username feature below.
authz_query_runtime_placeholder_returns_ok(Config) ->
    Query = <<"{in_group, \"cn=${username},ou=groups,dc=rabbitmq,dc=com\"}">>,
    Body = (base_body(Config))#{
        <<"queries">> => #{<<"tags">> => Query}
    },
    ?assertEqual(ok, validate(Config, Body)).

%%--------------------------------------------------------------------
%% Authorization query EVALUATION (username supplied)
%%--------------------------------------------------------------------
%% These supply a `username', so the endpoint resolves it to a DN over the
%% bound service handle and evaluates membership -- not just reachability.

%% alice IS a member of cn=admins (member=[svc,alice]); a literal in_group on
%% that group, evaluated for username=alice, returns ok.
username_in_group_returns_ok(Config) ->
    Query = list_to_binary("{in_group, \"" ++ ?ADMINS_GROUP_DN ++ "\"}"),
    Body = (base_body(Config))#{
        <<"username">> => <<"alice">>,
        <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
        <<"dn_lookup_attribute">> => <<"cn">>,
        <<"queries">> => #{<<"vhost_access">> => Query}
    },
    ?assertEqual(ok, validate(Config, Body)).

%% A ${username} placeholder DN that the broker would fill: with username=alice
%% it resolves to cn=alice,ou=people and is a real entry, so membership against
%% admins (filled DN) is evaluated and returns ok. Proves the fill + resolve +
%% equalityMatch(member, userDN) path, not mere reachability.
username_in_group_placeholder_dn_returns_ok(Config) ->
    Query = <<"{in_group, \"cn=admins,ou=groups,dc=rabbitmq,dc=com\"}">>,
    Body = (base_body(Config))#{
        <<"username">> => <<"alice">>,
        <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
        <<"dn_lookup_attribute">> => <<"cn">>,
        <<"queries">> => #{<<"tags">> => Query}
    },
    ?assertEqual(ok, validate(Config, Body)).

%% alice is NOT a member of cn=others (member=[svc] only); evaluating membership
%% for username=alice returns authz_unverified even though the group exists.
username_not_in_group_returns_authz_unverified(Config) ->
    Query = list_to_binary("{in_group, \"" ++ ?OTHERS_GROUP_DN ++ "\"}"),
    Body = (base_body(Config))#{
        <<"username">> => <<"alice">>,
        <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
        <<"dn_lookup_attribute">> => <<"cn">>,
        <<"queries">> => #{<<"vhost_access">> => Query}
    },
    ?assertMatch({error, authz_unverified, _}, validate(Config, Body)).

%% A username with no matching directory entry cannot be resolved to a single
%% DN, so the endpoint returns authz_unverified (deliberate divergence from the
%% broker's silent escaped-DN fallback: surface the misconfiguration).
username_unresolvable_returns_authz_unverified(Config) ->
    Query = list_to_binary("{in_group, \"" ++ ?ADMINS_GROUP_DN ++ "\"}"),
    Body = (base_body(Config))#{
        <<"username">> => <<"ghost">>,
        <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
        <<"dn_lookup_attribute">> => <<"cn">>,
        <<"queries">> => #{<<"vhost_access">> => Query}
    },
    ?assertMatch({error, authz_unverified, _}, validate(Config, Body)).

%% A DN keyed on per-operation context (${vhost}) cannot be filled even with a
%% username, so that query node degrades (grammar-checked only) and the request
%% still succeeds -- graceful degradation, not a hard failure.
username_runtime_placeholder_degrades_returns_ok(Config) ->
    Query = <<"{in_group, \"cn=admins,ou=${vhost},dc=rabbitmq,dc=com\"}">>,
    Body = (base_body(Config))#{
        <<"username">> => <<"alice">>,
        <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
        <<"dn_lookup_attribute">> => <<"cn">>,
        <<"queries">> => #{<<"vhost_access">> => Query}
    },
    ?assertEqual(ok, validate(Config, Body)).

%% Membership decision parity: the endpoint's in_group decision for a resolved
%% principal must match a direct eldap equalityMatch(member, userDN) search --
%% the same primitive rabbit_auth_backend_ldap:in_group/3 runs. Check both the
%% positive (admins) and negative (others) group for alice.
username_membership_parity_with_backend(Config) ->
    Port = ?config(ldap_port, Config),
    AliceDn = ?ALICE_DN,
    Cases = [
        {?ADMINS_GROUP_DN, true},
        {?OTHERS_GROUP_DN, false}
    ],
    [
        begin
            Query = list_to_binary("{in_group, \"" ++ GroupDn ++ "\"}"),
            Body = (base_body(Config))#{
                <<"username">> => <<"alice">>,
                <<"dn_lookup_base">> => list_to_binary(?PEOPLE_OU),
                <<"dn_lookup_attribute">> => <<"cn">>,
                <<"queries">> => #{<<"vhost_access">> => Query}
            },
            EndpointOk =
                case validate(Config, Body) of
                    ok -> true;
                    {error, authz_unverified, _} -> false
                end,
            BackendOk = direct_member(Port, GroupDn, AliceDn),
            ?assertEqual(
                Expected,
                EndpointOk,
                lists:flatten(io_lib:format("endpoint membership for ~ts", [GroupDn]))
            ),
            ?assertEqual(
                BackendOk,
                EndpointOk,
                lists:flatten(io_lib:format("membership parity for ~ts", [GroupDn]))
            )
        end
     || {GroupDn, Expected} <- Cases
    ],
    ok.

%% Direct base-scoped equalityMatch(member, UserDN) at the group DN -- the
%% broker's in_group primitive -- bound as the rootdn.
direct_member(Port, GroupDn, UserDn) ->
    {ok, H} = eldap:open(["localhost"], [{port, Port}]),
    ok = eldap:simple_bind(H, ?ADMIN_DN, ?ADMIN_PW),
    Result =
        case
            eldap:search(H, [
                {base, GroupDn},
                {filter, eldap:equalityMatch("member", UserDn)},
                {attributes, ["objectClass"]},
                {scope, eldap:baseObject()}
            ])
        of
            {ok, {eldap_search_result, Entries, _Ref, _Ctrls}} -> length(Entries) > 0;
            {ok, {eldap_search_result, Entries, _Ref}} -> length(Entries) > 0;
            _ -> false
        end,
    catch eldap:close(H),
    Result.

%%--------------------------------------------------------------------
%% Decision parity with the broker's bind path
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
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
        {ok, list_to_binary(Pw), State}
    end),
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
            case
                re:run(
                    Stdout,
                    "^SLAPD_PID=([0-9]+)$",
                    [{capture, all_but_first, list}, multiline]
                )
            of
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
        undefined ->
            ok;
        %% macOS: externally-managed server, leave it running
        "0" ->
            ok;
        Pid ->
            _ = rabbit_ct_helpers:exec(["kill", "-INT", Pid]),
            ok
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
        %% A group alice is NOT a member of (only svc), for the negative
        %% membership case.
        ok = add(H, ?OTHERS_GROUP_DN, [
            {"objectClass", ["groupOfNames"]},
            {"cn", ["others"]},
            {"member", [?BIND_DN]}
        ]),
        ok
    after
        catch eldap:close(H)
    end.

delete_seed(Port) ->
    H = admin_connect(Port),
    try
        [
            del(H, DN)
         || DN <- [
                ?OTHERS_GROUP_DN,
                ?ADMINS_GROUP_DN,
                ?ALICE_DN,
                ?BIND_DN,
                ?GROUPS_OU,
                ?PEOPLE_OU,
                ?BASE_DN
            ]
        ],
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
