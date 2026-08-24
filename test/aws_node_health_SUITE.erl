%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% End-to-end CT suite for the node-health feature on a REAL broker node.
%%
%% The eunit suites cover the pure scorer, the worker gen_server, and the
%% metric sample builders in isolation. This suite adds the integration layer
%% those cannot: it boots an actual broker with the plugin, lets aws_sup start
%% the worker and register the Prometheus collector from configuration, and
%% then verifies the wiring end to end -- the toggle, the live collector
%% rendering the (aws-namespaced) metric families through prometheus, and the
%% worker's verdict driven through its public API.
%%
%% node-health is a cluster feature, but a single node cannot exercise cross-
%% node attribution on its own. Rather than stand up a multi-node cluster (no
%% other suite here does, and real per-node faults are not reproducible in CT),
%% the verdict paths are driven deterministically by casting synthetic peer
%% rows into the running worker and calling refresh/0. The feature is booted
%% with window = 1, stale_ticks = 1, confirm_ticks = 1, clear_ticks = 1 and a
%% very long interval so a single injected snapshot decides the verdict, only
%% freshly-cast rows survive into each cycle (keeping test cases isolated), and
%% the periodic timer never interferes.
-module(aws_node_health_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%% Synthetic peer/observer node names used to build injected matrices. They are
%% never real nodes -- only data keys the scorer reasons over.
-define(FAULTY, 'rabbit@nh_faulty').
-define(OBS_A, 'rabbit@nh_a').
-define(OBS_B, 'rabbit@nh_b').
-define(OBS_C, 'rabbit@nh_c').

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [
        {group, feature_enabled},
        {group, feature_disabled}
    ].

groups() ->
    [
        {feature_enabled, [], [
            worker_and_collector_running,
            baseline_clean,
            injected_suspect_via_p2,
            injected_cluster_wide,
            renamed_families_render
        ]},
        {feature_disabled, [], [
            disabled_no_worker_no_metrics
        ]}
    ].

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(feature_enabled, Config) ->
    setup_broker(Config, [
        {node_health_enabled, true},
        %% One snapshot decides; only just-cast rows survive a cycle; the
        %% published flags flip on a single confirming/clearing cycle; the
        %% periodic timer effectively never fires during the suite.
        {node_health_window, 1},
        {node_health_stale_ticks, 1},
        {node_health_confirm_ticks, 1},
        {node_health_clear_ticks, 1},
        {node_health_interval_ms, 3600000}
    ]);
init_per_group(feature_disabled, Config) ->
    setup_broker(Config, [{node_health_enabled, false}]).

end_per_group(_Group, Config) ->
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_broker_helpers:teardown_steps()
    ).

init_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TC),
    Config.

end_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, TC),
    Config.

setup_broker(Config0, ExtraEnv) ->
    Config1 = rabbit_ct_helpers:set_config(Config0, [{rmq_nodename_suffix, ?MODULE}]),
    Config2 = rabbit_ct_helpers:merge_app_env(Config1, {aws, ExtraEnv}),
    rabbit_ct_helpers:run_setup_steps(
        Config2,
        rabbit_ct_broker_helpers:setup_steps()
    ).

%%====================================================================
%% feature_enabled
%%====================================================================

%% The toggle brings the worker and the collector online.
worker_and_collector_running(Config) ->
    ?assertNotEqual(undefined, rpc(Config, erlang, whereis, [aws_node_health_worker])),
    Collectors = rpc(Config, prometheus_registry, collectors, [default]),
    ?assert(lists:member(aws_node_health_metrics, Collectors)).

%% A healthy node (no peers, empty own row) yields a clean verdict.
baseline_clean(Config) ->
    #{verdict := Verdict} = refresh(Config),
    ?assertEqual(clean, Verdict).

%% Two independent observers seeing one node extreme -> that node is the
%% attributed suspect (P2: extreme for the whole window and dominating).
injected_suspect_via_p2(Config) ->
    cast_row(Config, ?OBS_A, #{?OBS_B => 0.0, ?FAULTY => 1.0}),
    cast_row(Config, ?OBS_B, #{?OBS_A => 0.0, ?FAULTY => 1.0}),
    #{verdict := Verdict, scores := Scores} = refresh(Config),
    ?assertEqual({suspect, ?FAULTY}, Verdict),
    ?assertEqual(1, maps:get(suspected, maps:get(?FAULTY, Scores))).

%% Every node elevated and bidirectional with none dominating -> the symmetric
%% condition is reported cluster-wide and no single node is blamed.
injected_cluster_wide(Config) ->
    cast_row(Config, ?OBS_A, #{?OBS_B => 0.9, ?OBS_C => 0.9}),
    cast_row(Config, ?OBS_B, #{?OBS_A => 0.9, ?OBS_C => 0.9}),
    cast_row(Config, ?OBS_C, #{?OBS_A => 0.9, ?OBS_B => 0.9}),
    #{verdict := Verdict} = refresh(Config),
    ?assertEqual(cluster_wide, Verdict).

%% The live collector renders exactly the aws-namespaced family names through
%% the real prometheus text format, and none of the pre-rename names.
renamed_families_render(Config) ->
    %% Drive a suspect first so the suspected/confidence families carry samples.
    cast_row(Config, ?OBS_A, #{?OBS_B => 0.0, ?FAULTY => 1.0}),
    cast_row(Config, ?OBS_B, #{?OBS_A => 0.0, ?FAULTY => 1.0}),
    _ = refresh(Config),
    Text = iolist_to_binary(rpc(Config, prometheus_text_format, format, [])),
    [
        ?assertNotEqual(nomatch, binary:match(Text, N))
     || N <- [
            <<"rabbitmq_aws_node_health_peer_down_probability">>,
            <<"rabbitmq_aws_node_health_peer_down_suspected">>,
            <<"rabbitmq_aws_node_health_peer_down_confidence">>,
            <<"rabbitmq_aws_node_health_cluster_congested">>
        ]
    ],
    [
        ?assertEqual(nomatch, binary:match(Text, N))
     || N <- [
            <<"rabbitmq_peer_down_probability">>,
            <<"rabbitmq_peer_down_suspected">>,
            <<"rabbitmq_peer_down_confidence">>,
            <<"rabbitmq_cluster_congested">>
        ]
    ].

%%====================================================================
%% feature_disabled
%%====================================================================

%% With the toggle off, the worker is not started and no node-health metric
%% families are rendered. (The collector module may still be discoverable in the
%% registry, but with the worker down collect_mf/2 reports unavailable and emits
%% nothing, so none of the aws-namespaced families appear in a scrape -- that is
%% the observable that matters.)
disabled_no_worker_no_metrics(Config) ->
    ?assertEqual(undefined, rpc(Config, erlang, whereis, [aws_node_health_worker])),
    Text = iolist_to_binary(rpc(Config, prometheus_text_format, format, [])),
    ?assertEqual(nomatch, binary:match(Text, <<"rabbitmq_aws_node_health_">>)).

%%====================================================================
%% Helpers (run against node 0)
%%====================================================================

rpc(Config, M, F, A) ->
    rabbit_ct_broker_helpers:rpc(Config, 0, M, F, A).

%% Record a synthetic observer row in the running worker. The cast is queued
%% before the subsequent refresh call in the worker's mailbox, so the row is in
%% place when refresh/0 runs its cycle.
cast_row(Config, Observer, View) ->
    ok = rpc(Config, gen_server, cast, [aws_node_health_worker, {peer_row, Observer, View}]).

refresh(Config) ->
    rpc(Config, aws_node_health_worker, refresh, []).
