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
    all/0, groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    feature_disabled_returns_404/1,
    unknown_method_returns_404/1,
    bad_json_returns_400/1,
    body_too_large_returns_400/1,
    config_conflict_returns_422/1,
    missing_servers_returns_400/1,
    rate_limit_returns_429/1,
    capacity_exhausted_returns_503/1,
    method_disabled_returns_404/1,
    options_returns_allowed_methods/1,
    get_returns_405/1,
    password_not_in_response/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbitmq_ct_helpers/include/rabbit_mgmt_test.hrl").
-include("aws.hrl").

-define(API, "/aws/auth/validate/ldap-simple-bind").

all() ->
    [
        {group, feature_disabled},
        {group, feature_enabled}
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
            rate_limit_returns_429,
            capacity_exhausted_returns_503,
            method_disabled_returns_404,
            options_returns_allowed_methods,
            get_returns_405,
            password_not_in_response
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
        {auth_validation_max_concurrent, 1},
        {auth_validation_max_body_size, 1024},
        {auth_validation_rate_limit_window_seconds, 60},
        {auth_validation_rate_limit_max_requests, 3}
    ]).

end_per_group(_Group, Config) ->
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_broker_helpers:teardown_steps()
    ).

init_per_testcase(method_disabled_returns_404 = TC, Config) ->
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, set_env,
        [aws, auth_validation_enabled_methods, [{<<"ldap-simple-bind">>, false}]]
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(rate_limit_returns_429 = TC, Config) ->
    %% Reset the rate limiter so prior tests can't interfere.
    catch rabbit_ct_broker_helpers:rpc(
        Config, 0, aws_auth_validate_rate_limiter, reset, []
    ),
    rabbit_ct_helpers:testcase_started(Config, TC);
init_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TC).

end_per_testcase(method_disabled_returns_404 = TC, Config) ->
    rabbit_ct_broker_helpers:rpc(
        Config, 0, application, unset_env, [aws, auth_validation_enabled_methods]
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

rate_limit_returns_429(Config) ->
    %% rate limit window=60s, max=3 (configured in init_per_group)
    Body = (base_body())#{<<"servers">> => [<<"127.0.0.1">>], <<"port">> => 1}, %% guaranteed-fail port
    %% three requests should all proceed past rate limit; fourth gets 429
    [_ = put_request(Config, ?API, Body) || _ <- lists:seq(1, 3)],
    {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, Body),
    ?assertEqual(429, Code).

capacity_exhausted_returns_503(Config) ->
    %% Hold the only semaphore slot, then make a request and expect 503.
    catch rabbit_ct_broker_helpers:rpc(
        Config, 0, aws_auth_validate_rate_limiter, reset, []
    ),
    {ok, _Ref} = rabbit_ct_broker_helpers:rpc(
        Config, 0, aws_auth_validate_semaphore, acquire, []
    ),
    try
        Body = base_body(),
        {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, Body),
        ?assertEqual(503, Code)
    after
        rabbit_ct_broker_helpers:rpc(
            Config, 0, application, set_env,
            [aws, auth_validation_max_concurrent, 1]
        )
    end.

method_disabled_returns_404(Config) ->
    Body = base_body(),
    {ok, {{_, Code, _}, _, _}} = put_request(Config, ?API, Body),
    ?assertEqual(404, Code).

options_returns_allowed_methods(Config) ->
    {ok, {{_, Code, _}, Headers, _}} = rabbit_mgmt_test_util:req(
        Config, 0, options, ?API,
        [rabbit_mgmt_test_util:auth_header("guest", "guest")]
    ),
    ?assertEqual(200, Code),
    Allow = string:to_upper(proplists:get_value("allow", Headers, "")),
    ?assert(string:str(Allow, "PUT") > 0),
    ?assert(string:str(Allow, "OPTIONS") > 0).

get_returns_405(Config) ->
    {ok, {{_, Code, _}, _, _}} = rabbit_mgmt_test_util:req(
        Config, 0, get, ?API,
        [rabbit_mgmt_test_util:auth_header("guest", "guest")]
    ),
    ?assertEqual(405, Code).

password_not_in_response(Config) ->
    %% Property 6: response body must never contain the submitted password.
    Password = <<"super-secret-pAssw0rd!">>,
    Body = (base_body())#{<<"password">> => Password,
                          <<"servers">> => [<<"127.0.0.1">>],
                          <<"port">> => 1},
    {ok, {{_, _Code, _}, _, ResBody}} = put_request(Config, ?API, Body),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(ResBody), Password)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

setup_broker(Config0, ExtraEnv) ->
    Config1 = rabbit_ct_helpers:set_config(Config0, [
        {rmq_nodename_suffix, ?MODULE}
    ]),
    Config2 = rabbit_ct_helpers:merge_app_env(Config1, {aws, ExtraEnv}),
    rabbit_ct_helpers:run_setup_steps(
        Config2,
        rabbit_ct_broker_helpers:setup_steps()
    ).

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
        Config, 0, put, Path,
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
