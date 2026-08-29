%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_node_health_config_tests).

-include_lib("eunit/include/eunit.hrl").

with_env(Key, Value, Body) ->
    application:set_env(aws, Key, Value),
    try
        Body()
    after
        application:unset_env(aws, Key)
    end.

enabled_defaults_to_false_test() ->
    application:unset_env(aws, node_health_enabled),
    ?assertEqual(false, aws_node_health_config:enabled()).

enabled_true_test() ->
    with_env(node_health_enabled, true, fun() ->
        ?assertEqual(true, aws_node_health_config:enabled())
    end).

enabled_non_true_is_false_test() ->
    with_env(node_health_enabled, yes, fun() ->
        ?assertEqual(false, aws_node_health_config:enabled())
    end).

interval_defaults_test() ->
    application:unset_env(aws, node_health_interval_ms),
    ?assertEqual(1000, aws_node_health_config:interval_ms()).

interval_override_test() ->
    with_env(node_health_interval_ms, 2500, fun() ->
        ?assertEqual(2500, aws_node_health_config:interval_ms())
    end).

interval_out_of_range_falls_back_test() ->
    with_env(node_health_interval_ms, 99999999, fun() ->
        ?assertEqual(1000, aws_node_health_config:interval_ms())
    end),
    with_env(node_health_interval_ms, 0, fun() ->
        ?assertEqual(1000, aws_node_health_config:interval_ms())
    end),
    with_env(node_health_interval_ms, not_an_int, fun() ->
        ?assertEqual(1000, aws_node_health_config:interval_ms())
    end).

%% A too-small interval (below the 500 ms floor) is rejected and falls back to
%% the default rather than hammering the failure detector.
interval_below_floor_falls_back_test() ->
    with_env(node_health_interval_ms, 1, fun() ->
        ?assertEqual(1000, aws_node_health_config:interval_ms())
    end),
    with_env(node_health_interval_ms, 499, fun() ->
        ?assertEqual(1000, aws_node_health_config:interval_ms())
    end).

%% The floor itself (500 ms) is accepted.
interval_floor_boundary_is_accepted_test() ->
    with_env(node_health_interval_ms, 500, fun() ->
        ?assertEqual(500, aws_node_health_config:interval_ms())
    end).

window_override_test() ->
    with_env(node_health_window, 60, fun() ->
        ?assertEqual(60, aws_node_health_config:window())
    end).

stale_ticks_override_test() ->
    with_env(node_health_stale_ticks, 8, fun() ->
        ?assertEqual(8, aws_node_health_config:stale_ticks())
    end).

worker_config_has_expected_defaults_test() ->
    lists:foreach(
        fun(K) -> application:unset_env(aws, K) end,
        [node_health_interval_ms, node_health_window, node_health_stale_ticks]
    ),
    Config = aws_node_health_config:worker_config(),
    ?assertEqual(1000, maps:get(interval_ms, Config)),
    ?assertEqual(30, maps:get(window_max, Config)),
    ?assertEqual(5, maps:get(stale_ticks, Config)),
    ?assert(is_map(maps:get(analysis, Config))).
