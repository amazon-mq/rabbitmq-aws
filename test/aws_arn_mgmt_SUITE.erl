%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_arn_mgmt_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
    bad_resource_name_returns_405/1,
    invalid_json_returns_400/1,
    http_get_returns_405/1,
    http_head_returns_405/1,
    http_post_returns_405/1,
    http_delete_returns_405/1,
    http_options_returns_200/1,
    empty_arns_returns_422/1,
    malformed_assume_role_arn_returns_400/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbitmq_ct_helpers/include/rabbit_mgmt_test.hrl").
-include("aws.hrl").

-import(rabbit_ct_helpers, [get_config/2]).
-import(rabbit_mgmt_test_util, [http_put/4]).

all() ->
    [
        bad_resource_name_returns_405,
        invalid_json_returns_400,
        http_get_returns_405,
        http_head_returns_405,
        http_post_returns_405,
        http_delete_returns_405,
        http_options_returns_200,
        empty_arns_returns_422,
        malformed_assume_role_arn_returns_400
    ].

init_per_suite(Config0) ->
    ok = inets:start(),
    Config1 = init_config(Config0),
    rabbit_ct_helpers:log_environment(),
    rabbit_ct_helpers:run_setup_steps(
        Config1,
        rabbit_ct_broker_helpers:setup_steps()
    ).

end_per_suite(Config) ->
    ok = inets:stop(),
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_broker_helpers:teardown_steps()
    ).

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

init_config(Config) ->
    C = [
        {api, "/aws/arn/validate"},
        {assume_role_arn, <<"arn:aws:iam::111111111111:role/MyCustomRole">>},
        {arn0, <<"arn:aws:secretsmanager:us-east-1:999999999999:secret:the-secret-AAAA">>},
        {arn1, <<"arn:aws:secretsmanager:us-east-1:999999999999:secret:another-secret-BBBB">>},
        {invalid_arn, <<"foo-baz-bat">>}
    ],
    rabbit_ct_helpers:set_config(Config, C).

bad_resource_name_returns_405(Config) ->
    AssumeRoleArn = get_config(Config, assume_role_arn),
    Arn0 = get_config(Config, arn0),
    Arn1 = get_config(Config, arn1),
    %% NB: bad resource name
    http_put(
        Config,
        "/aws/arn/validate-foobar",
        #{
            'assume_role_arn' => AssumeRoleArn,
            'arns' => [Arn0, Arn1]
        },
        ?METHOD_NOT_ALLOWED
    ).

invalid_json_returns_400(Config) ->
    Api = get_config(Config, api),
    %% Invalid JSON should return 400 Bad Request
    rabbit_mgmt_test_util:http_put_raw(
        Config,
        Api,
        "{invalid json",
        ?BAD_REQUEST
    ).

http_get_returns_405(Config) ->
    assert_405(get, Config).

http_head_returns_405(Config) ->
    assert_405(head, Config).

http_post_returns_405(Config) ->
    assert_405(post, Config).

http_delete_returns_405(Config) ->
    assert_405(delete, Config).

http_options_returns_200(Config) ->
    Api = get_config(Config, api),
    {ok, {{_, OptionsCode, _}, OptionsHeaders, _OptionsResBody}} =
        rabbit_mgmt_test_util:req(
            Config,
            0,
            options,
            Api,
            [rabbit_mgmt_test_util:auth_header("guest", "guest")]
        ),
    ?assertEqual(?OK, OptionsCode),
    AllowHeader = proplists:get_value("allow", OptionsHeaders),
    ?assert(string:str(string:to_upper(AllowHeader), "PUT") > 0),
    ?assert(string:str(string:to_upper(AllowHeader), "OPTIONS") > 0),
    %% Should NOT contain GET or HEAD
    ?assertEqual(0, string:str(string:to_upper(AllowHeader), "GET")),
    ?assertEqual(0, string:str(string:to_upper(AllowHeader), "HEAD")).

empty_arns_returns_422(Config) ->
    Api = get_config(Config, api),
    %% Empty arns array -> 422
    http_put(Config, Api, #{arns => []}, ?UNPROCESSABLE_ENTITY).

malformed_assume_role_arn_returns_400(Config) ->
    Api = get_config(Config, api),
    Arn0 = get_config(Config, arn0),
    Arn1 = get_config(Config, arn1),
    InvalidArn = get_config(Config, invalid_arn),
    %% Malformed assume_role_arn field -> 400
    http_put(
        Config,
        Api,
        #{
            'assume_role_arn' => InvalidArn,
            'arns' => [Arn0, Arn1]
        },
        ?BAD_REQUEST
    ).

assert_405(post, Config) ->
    %% NB: post requires a body or httpc complains.
    Api = get_config(Config, api),
    ?assertMatch(
        {ok, {{_, ?METHOD_NOT_ALLOWED, _}, _Headers, _ResBody}},
        rabbit_mgmt_test_util:req(
            Config,
            0,
            post,
            Api,
            [rabbit_mgmt_test_util:auth_header("guest", "guest")],
            "{}"
        )
    );
assert_405(Method, Config) ->
    Api = get_config(Config, api),
    ?assertMatch(
        {ok, {{_, ?METHOD_NOT_ALLOWED, _}, _Headers, _ResBody}},
        rabbit_mgmt_test_util:req(
            Config,
            0,
            Method,
            Api,
            [rabbit_mgmt_test_util:auth_header("guest", "guest")]
        )
    ).
