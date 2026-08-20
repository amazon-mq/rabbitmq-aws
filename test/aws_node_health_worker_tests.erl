%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_node_health_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Pure helpers
%%--------------------------------------------------------------------

record_row_overwrites_with_latest_tick_test() ->
    R1 = aws_node_health_worker:record_row(#{}, rmq1, #{rmq0 => 1.0}, 5),
    ?assertEqual({5, #{rmq0 => 1.0}}, maps:get(rmq1, R1)),
    R2 = aws_node_health_worker:record_row(R1, rmq1, #{rmq0 => 0.5}, 7),
    ?assertEqual({7, #{rmq0 => 0.5}}, maps:get(rmq1, R2)).

assemble_snapshot_evicts_stale_rows_test() ->
    Rows = #{
        rmq0 => {10, #{rmq1 => 0.0, rmq2 => 0.0}},
        rmq1 => {10, #{rmq0 => 1.0, rmq2 => 0.0}},
        %% last refreshed at tick 3; at tick 12 with stale_ticks 5 this is dropped
        rmq2 => {3, #{rmq0 => 1.0, rmq1 => 0.0}}
    },
    Snapshot = aws_node_health_worker:assemble_snapshot(Rows, 12, 5),
    ?assertEqual([rmq0, rmq1], lists:sort(maps:keys(Snapshot))),
    ?assertEqual(#{rmq0 => 1.0, rmq2 => 0.0}, maps:get(rmq1, Snapshot)).

push_window_keeps_most_recent_first_and_trims_test() ->
    Snap = fun(N) -> #{tag => N} end,
    Window = lists:foldl(
        fun(N, Acc) -> aws_node_health_worker:push_window(Acc, Snap(N), 3) end,
        [],
        [1, 2, 3, 4]
    ),
    ?assertEqual([Snap(4), Snap(3), Snap(2)], Window).

%%--------------------------------------------------------------------
%% gen_server integration (injected sampler and peers; no real cluster)
%%--------------------------------------------------------------------

worker_config(SampleFun) ->
    #{
        self_node => rmq0,
        peers_fun => fun() -> [] end,
        sample_fun => SampleFun,
        window_max => 30,
        stale_ticks => 100,
        %% large so the periodic timer never fires during the test
        interval_ms => 3600000,
        analysis => aws_node_health:default_config()
    }.

with_worker(SampleFun, Body) ->
    {ok, Pid} = aws_node_health_worker:start_link(worker_config(SampleFun)),
    try
        Body()
    after
        gen_server:stop(Pid)
    end.

fresh_worker_is_clean_test() ->
    with_worker(fun() -> #{} end, fun() ->
        ?assertEqual(#{verdict => clean, scores => #{}}, aws_node_health_worker:latest())
    end).

worker_attributes_single_fault_test() ->
    %% rmq0 is faulty (egress loss): both peers see rmq0 down, rmq0 sees peers
    %% fine, and the peers see each other cleanly.
    SampleFun = fun() -> #{rmq1 => 0.0, rmq2 => 0.0} end,
    with_worker(SampleFun, fun() ->
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 1.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 1.0, rmq1 => 0.0}}),
        _ = aws_node_health_worker:refresh(),
        _ = aws_node_health_worker:refresh(),
        Latest = aws_node_health_worker:refresh(),
        ?assertEqual({suspect, rmq0}, maps:get(verdict, Latest)),
        #{scores := Scores} = Latest,
        ?assertEqual(1, maps:get(suspected, maps:get(rmq0, Scores))),
        ?assertEqual(0, maps:get(suspected, maps:get(rmq1, Scores)))
    end).

worker_stays_clean_when_peers_healthy_test() ->
    SampleFun = fun() -> #{rmq1 => 0.0, rmq2 => 0.0} end,
    with_worker(SampleFun, fun() ->
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 0.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 0.0, rmq1 => 0.0}}),
        Latest = aws_node_health_worker:refresh(),
        ?assertEqual(clean, maps:get(verdict, Latest))
    end).
