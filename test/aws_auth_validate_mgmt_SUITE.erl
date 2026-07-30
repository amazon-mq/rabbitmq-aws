%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% HTTP-level tests for the auth validation endpoint
%%   PUT /api/aws/auth/validate/:method
%% These exercise the request pipeline (auth, body parsing, dispatch,
%% feature toggle) without requiring a real LDAP server. The actual
%% bind/connect/TLS paths are covered in aws_auth_validate_ldap_SUITE.
-module(aws_auth_validate_mgmt_SUITE).

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
    feature_disabled_returns_404/1,
    unknown_method_returns_404/1,
    bad_json_returns_400/1,
    body_too_large_returns_400/1,
    config_conflict_returns_422/1,
    missing_servers_returns_400/1,
    capacity_exhausted_returns_503/1,
    enabled_without_worker_returns_503/1,
    method_disabled_returns_404/1,
    backend_raise_returns_500_without_leak/1,
    custom_tag_insufficient_returns_401/1,
    custom_tag_unauthenticated_put_returns_401/1,
    custom_tag_options_preflight_allowed/1,
    options_returns_allowed_methods/1,
    get_returns_405/1,
    password_not_in_response/1,
    success_returns_204/1,
    oauth_disabled_by_default_returns_404/1,
    oauth_success_returns_204/1,
    oauth_response_no_secret/1,
    oauth_authz_portability_tripwire/1,
    oauth_authz_grants_returns_204/1,
    oauth_authz_denies_returns_422/1
]).

%% Invoked on the broker node via rpc to hold a semaphore slot.
-export([hold_slot/1]).

%% Invoked on the broker node via rpc to stub/unstub the registry dispatch.
-export([mock_dispatch_ok/0, mock_dispatch_raise/1, unmock_dispatch/0]).

%% Invoked on the broker node via rpc for the authz-through-the-pipeline cases:
%% probe for the oauth2 backend and stub ONLY the network seam (DNS
%% resolve-and-pin + the JWKS httpc GET) so the REAL backend + registry run
%% end-to-end through the Cowboy handler. Token + JWKS minting is done via the
%% shared aws_auth_validate_oauth_test_helpers module (rpc'd directly).
-export([
    oauth2_backend_available/0,
    oauth2_backend_loaded/0,
    oauth2_evaluator_compiled_in/0,
    mock_oauth_network/1,
    unmock_oauth_network/0
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbitmq_ct_helpers/include/rabbit_mgmt_test.hrl").
-include("aws.hrl").

-define(API, "/aws/auth/validate/ldap").
-define(OAUTH_API, "/aws/auth/validate/oauth").

%% A unique secret-shaped sentinel that a backend raise carries in scope. If the
%% handler's crash-dump guard ever let it escape, it would surface in the HTTP
%% response body; backend_raise_returns_500_without_leak asserts it does not.
-define(LEAK_SENTINEL, <<"S3cr3t-Sentinel-Handler-Leak-DO-NOT-LEAK">>).

all() ->
    [
        {group, feature_disabled},
        {group, feature_enabled},
        {group, feature_enabled_custom_tag},
        {group, oauth_method}
    ].

groups() ->
    [
        {feature_disabled, [], [
            feature_disabled_returns_404
        ]},
        {feature_enabled, [], [
            unknown_method_returns_404,
            bad_json_returns_400,
            body_too_large_returns_400,
            config_conflict_returns_422,
            missing_servers_returns_400,
            capacity_exhausted_returns_503,
            enabled_without_worker_returns_503,
            method_disabled_returns_404,
            backend_raise_returns_500_without_leak,
            options_returns_allowed_methods,
            get_returns_405,
            password_not_in_response,
            success_returns_204
        ]},
        %% A non-administrator required_user_tag exercises the authorize_tag/3
        %% branch: a user lacking the tag gets 401, while an OPTIONS preflight
        %% (unauthenticated, user=undefined) must still be allowed through.
        {feature_enabled_custom_tag, [], [
            custom_tag_insufficient_returns_401,
            custom_tag_unauthenticated_put_returns_401,
            custom_tag_options_preflight_allowed
        ]},
        %% OAuth method tests: opt-in behaviour (disabled by default -> 404),
        %% success/no-secret when enabled with mocked dispatch, and the optional
        %% authorization-evaluation layer driven end-to-end through the real
        %% backend + registry (dispatch NOT mocked; only the JWKS network seam is).
        {oauth_method, [], [
            oauth_disabled_by_default_returns_404,
            oauth_success_returns_204,
            oauth_response_no_secret,
            oauth_authz_portability_tripwire,
            oauth_authz_grants_returns_204,
            oauth_authz_denies_returns_422
        ]}
    ].

init_per_suite(Config) ->
    ok = inets:start(),
    rabbit_ct_helpers:log_environment(),
    Config.

end_per_suite(Config) ->
    inets:stop(),
    Config.

init_per_group(feature_disabled, Config) ->
    setup_broker(Config, [{auth_validation_enabled, false}]);
init_per_group(feature_enabled, Config) ->
    setup_broker(Config, [
        {auth_validation_enabled, true},
        %% ldap is opt-in like every other method: the master toggle alone does
        %% not bring it online, so this group must enable it explicitly or every
        %% ldap case below would get a 404 (method_disabled).
        {auth_validation_enabled_methods, [{<<"ldap">>, true}]},
        {auth_validation_max_concurrent, 1},
        {auth_validation_max_body_size, 1024},
        %% base_body/0 uses a loopback server (127.0.0.1); without this the
        %% SSRF filter (is_allowed_server/1) rejects it with input_invalid in
        %% parse_servers BEFORE later pipeline stages run, so cases that assert
        %% a later outcome (e.g. config_conflict_returns_422) would wrongly see
        %% a 400. These tests never open a real connection, so allowing private
        %% ranges here is safe and exercises the intended code path.
        {auth_validation_allow_private_networks, true}
    ]);
init_per_group(feature_enabled_custom_tag, Config) ->
    setup_broker(Config, [
        {auth_validation_enabled, true},
        %% ldap is opt-in; enable it explicitly (see the feature_enabled note).
        {auth_validation_enabled_methods, [{<<"ldap">>, true}]},
        {auth_validation_max_concurrent, 1},
        {auth_validation_max_body_size, 1024},
        {auth_validation_allow_private_networks, true},
        %% guest has the administrator tag but not monitoring, so the
        %% authorize_tag/3 membership check fails -> 401.
        {auth_validation_required_user_tag, monitoring}
    ]);
init_per_group(oauth_method, Config) ->
    %% OAuth is opt-in; enable validation but do NOT add oauth to
    %% enabled_methods so the default (disabled) applies for the
    %% disabled-by-default case. Individual testcases that need the method
    %% enabled will toggle it in init_per_testcase.
    setup_broker(Config, [
        {auth_validation_enabled, true},
        {auth_validation_max_concurrent, 1},
        {auth_validation_max_body_size, 1024}
    ]).

end_per_group(_Group, Config) ->
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_broker_helpers:teardown_steps()
    ).

init_per_testcase(method_disabled_returns_404 = TC, Config) ->
    rabbit_ct_broker_helpers:rpc(
        Config,
        0,
        application,
        set_env,
        [aws, auth_validation_enabled_methods, [{<<"ldap">>, false}]]
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(success_returns_204 = TC, Config) ->
    %% This suite has no live LDAP server, so make the backend dispatch return
    %% the success result (`ok') directly. meck runs ON THE BROKER NODE so the
    %% handler's call to aws_auth_validate_registry:dispatch/2 sees the stub.
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, mock_dispatch_ok, []
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(backend_raise_returns_500_without_leak = TC, Config) ->
    %% Drive the registry dispatch to RAISE with a secret-bearing term, so the
    %% handler's with_semaphore/6 defence-in-depth catch (the crash-dump leakage
    %% guard) is exercised through the real Cowboy pipeline. The mock runs ON THE
    %% BROKER NODE so the handler sees it.
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, mock_dispatch_raise, [?LEAK_SENTINEL]
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(oauth_success_returns_204 = TC, Config) ->
    %% Enable the oauth method and mock dispatch to return ok.
    rabbit_ct_broker_helpers:rpc(
        Config,
        0,
        application,
        set_env,
        [aws, auth_validation_enabled_methods, [{<<"oauth">>, true}]]
    ),
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, mock_dispatch_ok, []
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(oauth_response_no_secret = TC, Config) ->
    %% Enable the oauth method and mock dispatch to return ok.
    rabbit_ct_broker_helpers:rpc(
        Config,
        0,
        application,
        set_env,
        [aws, auth_validation_enabled_methods, [{<<"oauth">>, true}]]
    ),
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, mock_dispatch_ok, []
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(TC, Config) when
    TC =:= oauth_authz_grants_returns_204; TC =:= oauth_authz_denies_returns_422
->
    %% These drive the REAL oauth backend + registry end-to-end through the
    %% Cowboy handler (dispatch is NOT mocked), so we only stub the network seam
    %% (DNS resolve-and-pin + the JWKS httpc GET). The optional authz layer runs
    %% the broker's own scope-decision functions, so it needs
    %% rabbitmq_auth_backend_oauth2 loadable on the broker; skip gracefully if it
    %% is not (mirrors the eunit maybe_skip_authz and the LDAP suite's slapd skip).
    %% The available-but-should-not-be portability regression is caught separately
    %% by oauth_authz_portability_tripwire, which fails from a test-case body so it
    %% turns CI red regardless of the exit_status option (a ct:fail here, in
    %% init_per_testcase, would only auto-skip).
    case rabbit_ct_broker_helpers:rpc(Config, 0, ?MODULE, oauth2_backend_available, []) of
        false ->
            {skip,
                "OAuth authz evaluation unavailable on this broker node "
                "(pre-floor series or backend not loaded)"};
        true ->
            rabbit_ct_broker_helpers:rpc(
                Config,
                0,
                application,
                set_env,
                [aws, auth_validation_enabled_methods, [{<<"oauth">>, true}]]
            ),
            %% A signed RS256 token plus the authz config exceeds the group's
            %% 1024-byte cap; give the body headroom for these two cases only.
            rabbit_ct_broker_helpers:rpc(
                Config, 0, application, set_env, [aws, auth_validation_max_body_size, 8192]
            ),
            {Token, JwksJson} = rabbit_ct_broker_helpers:rpc(
                Config, 0, aws_auth_validate_oauth_test_helpers, oauth_authz_mint, []
            ),
            ok = rabbit_ct_broker_helpers:rpc(
                Config, 0, ?MODULE, mock_oauth_network, [JwksJson]
            ),
            Config1 = rabbit_ct_helpers:set_config(Config, [{oauth_token, Token}]),
            rabbit_ct_helpers:testcase_started(Config1, TC)
    end;
init_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TC).

end_per_testcase(method_disabled_returns_404 = TC, Config) ->
    %% Restore the group's ldap-enabled setting rather than unset it: ldap is now
    %% opt-in, so unsetting would leave it DISABLED and spuriously 404 the later
    %% ldap cases in this group (password_not_in_response, success_returns_204).
    rabbit_ct_broker_helpers:rpc(
        Config,
        0,
        application,
        set_env,
        [aws, auth_validation_enabled_methods, [{<<"ldap">>, true}]]
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(success_returns_204 = TC, Config) ->
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, unmock_dispatch, []
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(backend_raise_returns_500_without_leak = TC, Config) ->
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, unmock_dispatch, []
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(oauth_success_returns_204 = TC, Config) ->
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, unmock_dispatch, []
    ),
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, unset_env, [aws, auth_validation_enabled_methods]
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(oauth_response_no_secret = TC, Config) ->
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, unmock_dispatch, []
    ),
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, unset_env, [aws, auth_validation_enabled_methods]
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(TC, Config) when
    TC =:= oauth_authz_grants_returns_204; TC =:= oauth_authz_denies_returns_422
->
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, unmock_oauth_network, []
    ),
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, unset_env, [aws, auth_validation_enabled_methods]
    ),
    %% Restore the group's body-size cap so later cases are unaffected.
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, set_env, [aws, auth_validation_max_body_size, 1024]
    ),
    rabbit_ct_helpers:testcase_finished(Config, TC);
end_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, TC).

%%--------------------------------------------------------------------
%% Feature-disabled group
%%--------------------------------------------------------------------

feature_disabled_returns_404(Config) ->
    %% with feature disabled, resource_exists/2 -> false -> 404.
    Body = base_body(),
    {ok, {{_, Code, _}, _Headers, _ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(404, Code).

%%--------------------------------------------------------------------
%% Feature-enabled group
%%--------------------------------------------------------------------

unknown_method_returns_404(Config) ->
    Body = base_body(),
    Path = "/aws/auth/validate/no-such-method",
    {ok, {{_, Code, _}, _, _}} = put_request(Config, Path, Body),
    ?assertEqual(404, Code).

bad_json_returns_400(Config) ->
    rabbit_mgmt_test_util:http_put_raw(Config, ?API, "{not valid json", 400).

body_too_large_returns_400(Config) ->
    Big = binary:copy(<<"x">>, 4096),
    Body = (base_body())#{<<"user_dn">> => Big},
    {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, Body),
    ?assertEqual(400, Code).

config_conflict_returns_422(Config) ->
    Body = (base_body())#{<<"use_ssl">> => true, <<"use_starttls">> => true},
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(422, Code),
    ?assertMatch(#{<<"error">> := <<"config_conflict">>}, decode(ResBody)).

missing_servers_returns_400(Config) ->
    Body = maps:remove(<<"servers">>, base_body()),
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(400, Code),
    ?assertMatch(#{<<"error">> := <<"input_invalid">>}, decode(ResBody)).

capacity_exhausted_returns_503(Config) ->
    %% Hold the only semaphore slot for the duration of the HTTP request.
    %%
    %% A plain rpc(..., aws_auth_validate_semaphore, acquire, []) does NOT
    %% work: erpc runs acquire/0 in a transient worker process that exits as
    %% soon as the call returns, and the semaphore monitors its holder and
    %% auto-releases the slot on that worker's DOWN -- so the slot would be
    %% free again before our request arrives. Instead spawn a long-lived
    %% holder process ON THE BROKER NODE (via ?MODULE:hold_slot/1, which puts
    %% this suite on the broker's code path) that acquires and then blocks
    %% until told to release.
    Holder = rabbit_ct_broker_helpers:rpc(Config, 0, ?MODULE, hold_slot, [self()]),
    ok = wait_for_current(Config, 1, 50),
    try
        Body = base_body(),
        {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, Body),
        ?assertEqual(503, Code)
    after
        Holder ! release,
        _ = wait_for_current(Config, 0, 50)
    end.

method_disabled_returns_404(Config) ->
    Body = base_body(),
    {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, Body),
    ?assertEqual(404, Code).

%% No credential leakage (crash-dump path): the handler's with_semaphore/6 wraps
%% the backend dispatch in a try/catch whose sole purpose is to keep an escaping
%% exception -- which would carry the request BodyMap (and any resolved secret)
%% into a Cowboy crash report -- from ever surfacing. Nothing exercised that catch
%% before: the success/error paths all RETURN a value, so the class:reason:stack
%% discard was asserted nowhere.
%%
%% Here we force dispatch to RAISE with a secret-bearing term (the worst case for
%% a crash-report leak) and drive a real PUT through the handler. We assert:
%%   1. the caller gets the fixed 500 internal_error response (the catch fired --
%%      the request did not 500 with a stacktrace or hang), and
%%   2. neither the sentinel secret nor the raised reason leaks into the response
%%      body -- it is a fixed category + message only.
%% The submitted-password no-leak on the response is covered by
%% password_not_in_response; this is specifically the RAISE path through the
%% handler boundary that was previously unproven.
backend_raise_returns_500_without_leak(Config) ->
    Body = (base_body())#{<<"password">> => ?LEAK_SENTINEL},
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(500, Code),
    Decoded = decode(ResBody),
    ?assertMatch(#{<<"error">> := <<"internal_error">>}, Decoded),
    %% The fixed message is the only detail; no raised reason / stacktrace.
    ?assertMatch(
        #{<<"message">> := <<"Internal error during validation">>}, Decoded
    ),
    %% The secret the raise carried in scope must not appear anywhere in the body.
    RawResBody = iolist_to_binary(ResBody),
    ?assertEqual(nomatch, binary:match(RawResBody, ?LEAK_SENTINEL)).

%% Feature reads enabled but the semaphore worker is not running (the
%% runtime-enable-without-restart / supervisor-gave-up case). The handler
%% must return a graceful 503, not an opaque 500 from a noproc gen_server
%% call. We simulate the missing worker by terminating the supervised child,
%% then restore it so later cases are unaffected.
enabled_without_worker_returns_503(Config) ->
    ok = rabbit_ct_broker_helpers:rpc(
        Config, 0, supervisor, terminate_child, [aws_sup, aws_auth_validate_semaphore]
    ),
    try
        Body = base_body(),
        {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
        ?assertEqual(503, Code),
        ?assertMatch(#{<<"error">> := <<"capacity_exhausted">>}, decode(ResBody))
    after
        {ok, _} = rabbit_ct_broker_helpers:rpc(
            Config, 0, supervisor, restart_child, [aws_sup, aws_auth_validate_semaphore]
        )
    end.

%%--------------------------------------------------------------------
%% Feature-enabled-custom-tag group (required_user_tag = monitoring)
%%--------------------------------------------------------------------

%% guest authenticates but lacks the monitoring tag, so authorize_tag/3's
%% membership check fails and the PUT is rejected with 401.
custom_tag_insufficient_returns_401(Config) ->
    Body = base_body(),
    {ok, {{_, Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(401, Code),
    ?assertMatch(#{<<"error">> := <<"insufficient_user_tag">>}, decode(ResBody)).

%% Authorization gate confirmatory test: the authorize_tag/3 `user = undefined'
%% clause (aws_auth_validate_mgmt.erl) lets a request through WITHOUT a user, on
%% the documented assumption that only an unauthenticated OPTIONS preflight can
%% reach it (rabbit_mgmt_util short-circuits OPTIONS to {true, _, undefined}
%% before authenticating). The safety of the whole custom-tag branch rests on that
%% clause being UNREACHABLE by a state-changing PUT. This asserts the invariant:
%% an UNAUTHENTICATED PUT (no auth header) is stopped with 401 by is_authorized/2
%% BEFORE authorize_tag/3 is reached -- it must never dispatch. If a future
%% refactor let an anonymous PUT reach the user=undefined pass-through, this
%% would see a 2xx/4xx-from-dispatch instead of the 401 and fail (CWE-862/863).
custom_tag_unauthenticated_put_returns_401(Config) ->
    Body = binary_to_list(rabbit_json:encode(base_body())),
    %% No auth_header/2 credential -- an anonymous request.
    {ok, {{_, Code, _}, _Headers, _ResBody}} = rabbit_mgmt_test_util:req(
        Config,
        0,
        put,
        ?API,
        [{"content-type", "application/json"}],
        Body
    ),
    ?assertEqual(401, Code).

%% An OPTIONS preflight is unauthenticated (user=undefined) but must still be
%% allowed through on the custom-tag branch -- regression test for the bug
%% where authorize_tag/3 fell through to 401 on the preflight.
custom_tag_options_preflight_allowed(Config) ->
    {ok, {{_, Code, _}, Headers, _}} = rabbit_mgmt_test_util:req(
        Config,
        0,
        options,
        ?API,
        [rabbit_mgmt_test_util:auth_header("guest", "guest")]
    ),
    ?assertEqual(200, Code),
    Allow = string:to_upper(proplists:get_value("allow", Headers, "")),
    ?assert(string:str(Allow, "PUT") > 0),
    ?assert(string:str(Allow, "OPTIONS") > 0).

options_returns_allowed_methods(Config) ->
    {ok, {{_, Code, _}, Headers, _}} = rabbit_mgmt_test_util:req(
        Config,
        0,
        options,
        ?API,
        [rabbit_mgmt_test_util:auth_header("guest", "guest")]
    ),
    ?assertEqual(200, Code),
    Allow = string:to_upper(proplists:get_value("allow", Headers, "")),
    ?assert(string:str(Allow, "PUT") > 0),
    ?assert(string:str(Allow, "OPTIONS") > 0).

get_returns_405(Config) ->
    {ok, {{_, Code, _}, _, _}} = rabbit_mgmt_test_util:req(
        Config,
        0,
        get,
        ?API,
        [rabbit_mgmt_test_util:auth_header("guest", "guest")]
    ),
    ?assertEqual(405, Code).

password_not_in_response(Config) ->
    %% Property 6: response body must never contain the submitted password.
    Password = <<"super-secret-pAssw0rd!">>,
    Body = (base_body())#{
        <<"password">> => Password,
        <<"servers">> => [<<"127.0.0.1">>],
        <<"port">> => 1
    },
    {ok, {{_, _Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(ResBody), Password)).

%% A successful validation (backend returns `ok') must surface as a 204 with an
%% empty body. The other cases only exercise the error responses; the 204 path
%% (respond(ok, ...)) was otherwise reached only via direct validate/1 calls in
%% the LDAP integration suite, never through the HTTP handler. The backend is
%% stubbed (see init_per_testcase) because this suite has no live LDAP server.
success_returns_204(Config) ->
    Body = base_body(),
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(204, Code),
    ?assertEqual(<<>>, iolist_to_binary(ResBody)).

%%--------------------------------------------------------------------
%% OAuth method group
%%--------------------------------------------------------------------

%% OAuth is an opt-in method (in ?OPT_IN_METHODS). Without explicit enablement
%% it defaults to disabled, so any PUT to /aws/auth/validate/oauth returns 404.
oauth_disabled_by_default_returns_404(Config) ->
    Body = oauth_body(),
    {ok, {{_, Code, _}, _, _}} = put_request(Config, ?OAUTH_API, Body),
    ?assertEqual(404, Code).

%% With the method enabled and dispatch mocked, the handler returns 204.
oauth_success_returns_204(Config) ->
    Body = oauth_body(),
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?OAUTH_API, Body),
    ?assertEqual(204, Code),
    ?assertEqual(<<>>, iolist_to_binary(ResBody)).

%% The 204 response must carry no secret material. We submit a body that
%% contains a plausible secret field and verify it does not appear in the
%% response.
oauth_response_no_secret(Config) ->
    Secret = <<"oauth-client-secret-DO-NOT-LEAK-1234">>,
    Body = (oauth_body())#{<<"client_secret">> => Secret},
    {ok, {{_, _Code, _}, _, ResBody}} = put_request(Config, ?OAUTH_API, Body),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(ResBody), Secret)).

%% Portability tripwire for the build guard. When the evaluator was compiled in
%% (the Makefile saw the arity-4 scope API in oauth2.hrl) and the oauth2 backend
%% is loaded, available/0 MUST be true; if it is false, the build guard admitted
%% a broker series whose backend lacks the arity-4 API this layer calls -- a
%% portability regression. Every matrix arm exercises exactly one outcome: on
%% v4.2.x/v4.3.x/main the evaluator is compiled in, the backend is loaded and
%% available/0 is true, so this passes; on a pre-floor series (e.g. v3.13.7) the
%% evaluator is not compiled in, so this skips like the two cases below.
%%
%% This fails from a TEST-CASE BODY, not init_per_testcase, on purpose: a
%% ct:fail in an init clause auto-skips the case, which turns the run red only
%% via Common Test's default exit_status (auto-skip -> non-zero). Under
%% -exit_status ignore_config -- a common way to tolerate infra-dependent skips
%% (this suite skips without slapd, without the backend, ...) -- that auto-skip
%% would pass green and silence the regression. A failure from a test-case body
%% increments the Failed count, which ct_run maps to a non-zero exit before the
%% exit_status branch is consulted, so this tripwire stays red unconditionally.
oauth_authz_portability_tripwire(Config) ->
    Available = rabbit_ct_broker_helpers:rpc(Config, 0, ?MODULE, oauth2_backend_available, []),
    EvaluatorCompiledIn = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, oauth2_evaluator_compiled_in, []
    ),
    BackendLoaded = rabbit_ct_broker_helpers:rpc(
        Config, 0, ?MODULE, oauth2_backend_loaded, []
    ),
    case Available of
        true ->
            ok;
        false ->
            %% Gate the hard failure on evaluator_compiled_in, not backend_loaded
            %% alone: rabbitmq_auth_backend_oauth2 is in TEST_DEPS and so loadable
            %% even on a pre-floor series where the evaluator was correctly not
            %% compiled in -- that is a legitimate skip, not a regression.
            case EvaluatorCompiledIn andalso BackendLoaded of
                true ->
                    ct:fail(
                        "rabbitmq_auth_backend_oauth2 is loaded on the broker node and "
                        "the authz evaluator was compiled in, but "
                        "aws_auth_validate_oauth_authz:available/0 is false: the backend "
                        "does not export the arity-4 scope API this layer requires. The "
                        "build guard admitted an unsupported broker series."
                    );
                false ->
                    {skip,
                        "OAuth authz evaluation unavailable on this broker node "
                        "(pre-floor series or backend not loaded)"}
            end
    end.

%% Optional authorization-evaluation layer, driven END-TO-END through the full
%% Cowboy pipeline: auth gate -> body-size cap -> JSON decode -> semaphore ->
%% registry field-filter -> REAL oauth backend -> token verification -> authz
%% evaluation via the broker's own scope-decision functions. Unlike
%% oauth_success_returns_204, dispatch is NOT mocked here; only the JWKS network
%% seam (DNS resolve-and-pin + httpc GET) is stubbed, so the authz decision is
%% genuinely computed. The minted token grants `read' on any vhost/resource.
%%
%% GRANT: request `read' -> the broker's resource_access says yes -> 204. This
%% proves the authz_check field survives the registry allowlist filter and the
%% grant path returns success through the handler (not just via dispatch/2).
oauth_authz_grants_returns_204(Config) ->
    Token = ?config(oauth_token, Config),
    Body = oauth_authz_body(Token, <<"read">>),
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?OAUTH_API, Body),
    ?assertEqual(204, Code),
    ?assertEqual(<<>>, iolist_to_binary(ResBody)).

%% DENY: the SAME verified token (grants `read' only), but request `configure'.
%% The broker's scope match fails -> authz_unverified -> 422. Same token, only
%% the requested permission differs, so a 422 here (vs 204 above) proves the
%% authz config is genuinely consumed and not a no-op. The response carries the
%% fixed category only; it must not leak the token.
oauth_authz_denies_returns_422(Config) ->
    Token = ?config(oauth_token, Config),
    Body = oauth_authz_body(Token, <<"configure">>),
    {ok, {{_, Code, _}, _Headers, ResBody}} = put_request(Config, ?OAUTH_API, Body),
    ?assertEqual(422, Code),
    ?assertMatch(#{<<"error">> := <<"authz_unverified">>}, decode(ResBody)),
    %% The caller's token must never appear in the response body.
    ?assertEqual(nomatch, binary:match(iolist_to_binary(ResBody), Token)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Run ON THE BROKER NODE (invoked via rpc). Stub the registry dispatch so it
%% returns the success result without touching a real LDAP server. meck is
%% loaded onto the broker node by setup_meck/1 during setup_broker/2.
mock_dispatch_ok() ->
    ok = meck:new(aws_auth_validate_registry, [passthrough, no_link]),
    ok = meck:expect(aws_auth_validate_registry, dispatch, fun(_Method, _Body) -> ok end),
    ok.

%% Run ON THE BROKER NODE (invoked via rpc). Stub the registry dispatch so it
%% RAISES with the given secret-bearing term, exercising the handler's
%% with_semaphore/6 crash-dump guard. The secret is embedded in the raised reason
%% -- the worst case for a crash-report leak.
mock_dispatch_raise(Secret) ->
    ok = meck:new(aws_auth_validate_registry, [passthrough, no_link]),
    ok = meck:expect(aws_auth_validate_registry, dispatch, fun(_Method, _Body) ->
        erlang:error({boom, Secret})
    end),
    ok.

unmock_dispatch() ->
    catch meck:unload(aws_auth_validate_registry),
    ok.

%% Runs ON THE BROKER NODE. True when the optional authz layer can run there --
%% i.e. rabbitmq_auth_backend_oauth2's scope-decision functions are loadable.
%% Delegates to the layer's own availability probe (code:ensure_loaded-backed),
%% so this matches exactly what the request path checks at runtime.
oauth2_backend_available() ->
    aws_auth_validate_oauth_authz:available().

%% Runs ON THE BROKER NODE. True when rabbitmq_auth_backend_oauth2 is loaded at
%% all, regardless of whether it exposes the arity-4 scope API. Lets
%% init_per_testcase tell "backend genuinely absent" (legitimate skip) from
%% "backend present but too old" (hard failure) -- see Luke's coverage-gap note.
oauth2_backend_loaded() ->
    aws_auth_validate_oauth_authz:backend_loaded().

%% Runs ON THE BROKER NODE. True when the record-typed authz evaluator was
%% compiled into the aws plugin build (the Makefile saw the arity-4 scope API in
%% oauth2.hrl). Lets init_per_testcase scope the present-but-unusable hard
%% failure to a genuine portability regression rather than a pre-floor series.
oauth2_evaluator_compiled_in() ->
    aws_auth_validate_oauth_authz:evaluator_compiled_in().

%% Runs ON THE BROKER NODE. Stub ONLY the network seam so the REAL backend +
%% registry + authz layer run: (1) aws_auth_validate_net:resolve_and_pin/2 skips
%% DNS and pins the JWKS host to itself, and (2) httpc:request/5 returns the
%% supplied JWKS for the test IdP host, passing through anything else so the
%% broker's own outbound calls are unaffected. Both mecks are passthrough.
mock_oauth_network(JwksJson) ->
    catch meck:unload(aws_auth_validate_net),
    catch meck:unload(httpc),
    ok = meck:new(aws_auth_validate_net, [passthrough, no_link]),
    meck:expect(aws_auth_validate_net, resolve_and_pin, fun(#{host := Host} = Url, _Policy) ->
        {ok, Url, Host}
    end),
    ok = meck:new(httpc, [passthrough, unstick, no_link]),
    meck:expect(httpc, request, fun(Method, Req, HttpOpts, Opts, Profile) ->
        Url = aws_auth_validate_oauth_test_helpers:request_url(Req),
        case string:find(Url, "idp.example.com") of
            nomatch ->
                meck:passthrough([Method, Req, HttpOpts, Opts, Profile]);
            _ ->
                {ok, {{"HTTP/1.1", 200, "OK"}, [], JwksJson}}
        end
    end),
    ok.

unmock_oauth_network() ->
    catch meck:unload(httpc),
    catch meck:unload(aws_auth_validate_net),
    ok.

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
            %% Load meck onto the broker node so success_returns_204 can stub
            %% the registry dispatch there.
            ok = rabbit_ct_broker_helpers:setup_meck(Config3),
            Config3
    end.

%% Runs ON THE BROKER NODE (invoked via rpc with Module=?MODULE). Spawns a
%% persistent process that acquires the only semaphore slot and blocks until
%% told to release, so the slot stays held across an HTTP request issued from
%% the CT node. Returns the holder pid (on the broker node).
hold_slot(Parent) ->
    spawn(fun() ->
        {ok, _Ref} = aws_auth_validate_semaphore:acquire(),
        Parent ! slot_held,
        receive
            release -> ok
        after 30_000 -> ok
        end
    end).

%% Poll the semaphore's current holder count until it reaches Target.
wait_for_current(_Config, Target, 0) ->
    {error, {timeout_waiting_for_current, Target}};
wait_for_current(Config, Target, N) ->
    case rabbit_ct_broker_helpers:rpc(Config, 0, aws_auth_validate_semaphore, current, []) of
        Target ->
            ok;
        _ ->
            timer:sleep(20),
            wait_for_current(Config, Target, N - 1)
    end.

base_body() ->
    #{
        <<"servers">> => [<<"127.0.0.1">>],
        <<"port">> => 389,
        <<"user_dn">> => <<"cn=test,dc=example,dc=com">>,
        <<"password">> => <<"unused">>
    }.

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

oauth_body() ->
    #{
        <<"jwks_uri">> => <<"https://idp.example.com/.well-known/jwks.json">>
    }.

%% An oauth validation body carrying a customer-supplied access_token plus an
%% authz_check block. The scope config (resource_server_id + scope_prefix) mirrors
%% the documented OAuth recipe; the minted token's scope is `rabbitmq.read:*/*'.
%% Permission is parameterised so one helper drives both the grant (read) and the
%% deny (configure) case against the same token.
oauth_authz_body(Token, Permission) ->
    (oauth_body())#{
        <<"access_token">> => Token,
        <<"resource_server_id">> => <<"rabbitmq">>,
        <<"scope_prefix">> => <<"rabbitmq.">>,
        <<"authz_check">> => #{
            <<"vhost">> => <<"/">>,
            <<"resource">> => <<"my-queue">>,
            <<"permission">> => Permission
        }
    }.

decode(ResBody) ->
    case rabbit_json:try_decode(iolist_to_binary(ResBody)) of
        {ok, Map} -> Map;
        _ -> #{}
    end.
