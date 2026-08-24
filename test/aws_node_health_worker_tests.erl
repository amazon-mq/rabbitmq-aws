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
        %% window_max = 1 keeps the window always "full" (one snapshot), so these
        %% integration tests exercise the gen_server plumbing and the debounce in
        %% isolation, without the extreme-fraction warmup (which needs the window
        %% to fill). The windowing/fraction math is covered by aws_node_health's
        %% own unit tests, which use full 30-sample windows.
        window_max => 1,
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
        {_OwnRow, Latest} = aws_node_health_worker:report(),
        ?assertEqual(#{verdict => clean, scores => #{}}, Latest)
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

%% Hysteresis: even with the raw verdict pointing at rmq0 every tick, the
%% published `suspected` must not flip until the suspect has held for
%% confirm_ticks consecutive ticks. Here confirm_ticks=3: ticks 1 and 2 stay
%% clean (armed but not confirmed); tick 3 publishes the suspect.
worker_debounces_suspect_until_confirm_ticks_test() ->
    SampleFun = fun() -> #{rmq1 => 0.0, rmq2 => 0.0} end,
    Cfg = (worker_config(SampleFun))#{confirm_ticks => 3, clear_ticks => 3},
    {ok, Pid} = aws_node_health_worker:start_link(Cfg),
    try
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 1.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 1.0, rmq1 => 0.0}}),
        ?assertEqual(clean, maps:get(verdict, aws_node_health_worker:refresh())),
        ?assertEqual(clean, maps:get(verdict, aws_node_health_worker:refresh())),
        L3 = aws_node_health_worker:refresh(),
        ?assertEqual({suspect, rmq0}, maps:get(verdict, L3)),
        ?assertEqual(1, maps:get(suspected, maps:get(rmq0, maps:get(scores, L3))))
    after
        gen_server:stop(Pid)
    end.

%% Hysteresis for the cluster_wide verdict: uniform congestion (every node sees
%% every peer elevated) must not publish cluster_wide until it has held for
%% confirm_ticks consecutive ticks. With confirm_ticks=3, ticks 1 and 2 stay
%% clean (armed but not confirmed); tick 3 publishes cluster_wide.
worker_debounces_cluster_wide_until_confirm_ticks_test() ->
    SampleFun = fun() -> #{rmq1 => 0.9, rmq2 => 0.9} end,
    Cfg = (worker_config(SampleFun))#{confirm_ticks => 3, clear_ticks => 3},
    {ok, Pid} = aws_node_health_worker:start_link(Cfg),
    try
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 0.9, rmq2 => 0.9}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 0.9, rmq1 => 0.9}}),
        ?assertEqual(clean, maps:get(verdict, aws_node_health_worker:refresh())),
        ?assertEqual(clean, maps:get(verdict, aws_node_health_worker:refresh())),
        ?assertEqual(cluster_wide, maps:get(verdict, aws_node_health_worker:refresh()))
    after
        gen_server:stop(Pid)
    end.

%% Once published, cluster_wide must hold until clear_ticks consecutive
%% non-cluster_wide ticks. With clear_ticks=3, the first two healthy ticks after
%% congestion clears still read cluster_wide; the third clears to clean.
worker_holds_cluster_wide_until_clear_ticks_test() ->
    T = ets:new(nh_sampler, [set, public]),
    ets:insert(T, {row, #{rmq1 => 0.9, rmq2 => 0.9}}),
    SampleFun = fun() ->
        [{row, R}] = ets:lookup(T, row),
        R
    end,
    Cfg = (worker_config(SampleFun))#{confirm_ticks => 2, clear_ticks => 3},
    {ok, Pid} = aws_node_health_worker:start_link(Cfg),
    try
        %% Phase 1: uniform congestion -> confirm cluster_wide.
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 0.9, rmq2 => 0.9}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 0.9, rmq1 => 0.9}}),
        _ = aws_node_health_worker:refresh(),
        ?assertEqual(cluster_wide, maps:get(verdict, aws_node_health_worker:refresh())),
        %% Phase 2: congestion clears -> hold until clear_ticks misses.
        ets:insert(T, {row, #{rmq1 => 0.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 0.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 0.0, rmq1 => 0.0}}),
        ?assertEqual(cluster_wide, maps:get(verdict, aws_node_health_worker:refresh())),
        ?assertEqual(cluster_wide, maps:get(verdict, aws_node_health_worker:refresh())),
        ?assertEqual(clean, maps:get(verdict, aws_node_health_worker:refresh()))
    after
        gen_server:stop(Pid),
        ets:delete(T)
    end.

%% Regression (review finding): a confirmed suspect must NOT stay latched once
%% the raw verdict turns cluster_wide - a genuine symmetric condition must clear
%% the held suspect at once, not be overridden by it. clear_ticks is set high so
%% only the cluster_wide immediate-clear (not the miss counter) can clear it
%% within the test; a mutable sampler and small window let the cluster-wide
%% matrix fill quickly.
worker_clears_confirmed_suspect_when_verdict_turns_cluster_wide_test() ->
    T = ets:new(nh_sampler, [set, public]),
    ets:insert(T, {row, #{rmq1 => 0.0, rmq2 => 0.0}}),
    SampleFun = fun() ->
        [{row, R}] = ets:lookup(T, row),
        R
    end,
    Cfg = (worker_config(SampleFun))#{confirm_ticks => 2, clear_ticks => 10},
    {ok, Pid} = aws_node_health_worker:start_link(Cfg),
    try
        %% Phase 1: single fault -> confirm rmq0.
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 1.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 1.0, rmq1 => 0.0}}),
        _ = aws_node_health_worker:refresh(),
        ?assertEqual({suspect, rmq0}, maps:get(verdict, aws_node_health_worker:refresh())),
        %% Phase 2: uniform cluster-wide congestion (every node sees every peer high).
        ets:insert(T, {row, #{rmq1 => 0.9, rmq2 => 0.9}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 0.9, rmq2 => 0.9}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 0.9, rmq1 => 0.9}}),
        _ = aws_node_health_worker:refresh(),
        _ = aws_node_health_worker:refresh(),
        L = aws_node_health_worker:refresh(),
        ?assertEqual(cluster_wide, maps:get(verdict, L)),
        ?assertEqual(0, maps:get(suspected, maps:get(rmq0, maps:get(scores, L))))
    after
        gen_server:stop(Pid),
        ets:delete(T)
    end.
