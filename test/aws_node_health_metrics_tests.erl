%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_node_health_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Pure sample builders
%%--------------------------------------------------------------------

probability_samples_test() ->
    Samples = aws_node_health_metrics:probability_samples(#{rmq1 => 0.9, rmq2 => 0.1}),
    ?assertEqual(
        lists:sort([{[{peer, rmq1}], 0.9}, {[{peer, rmq2}], 0.1}]),
        lists:sort(Samples)
    ).

suspected_samples_test() ->
    Scores = #{
        rmq0 => #{suspected => 1, confidence => 0.9, inbound => 1.0},
        rmq1 => #{suspected => 0, confidence => 0.0, inbound => 0.0}
    },
    ?assertEqual(
        lists:sort([{[{peer, rmq0}], 1}, {[{peer, rmq1}], 0}]),
        lists:sort(aws_node_health_metrics:suspected_samples(other, Scores))
    ).

confidence_samples_test() ->
    Scores = #{
        rmq0 => #{suspected => 1, confidence => 0.9, inbound => 1.0},
        rmq1 => #{suspected => 0, confidence => 0.0, inbound => 0.0}
    },
    ?assertEqual(
        lists:sort([{[{peer, rmq0}], 0.9}, {[{peer, rmq1}], 0.0}]),
        lists:sort(aws_node_health_metrics:confidence_samples(other, Scores))
    ).

%% Self is excluded from the suspected/confidence gauges so a node never reports
%% itself as a suspected-down peer and the peer domain matches probability.
samples_exclude_self_test() ->
    Scores = #{
        rmq0 => #{suspected => 1, confidence => 0.9, inbound => 1.0},
        rmq1 => #{suspected => 0, confidence => 0.0, inbound => 0.0}
    },
    ?assertEqual([{[{peer, rmq1}], 0}], aws_node_health_metrics:suspected_samples(rmq0, Scores)),
    ?assertEqual([{[{peer, rmq1}], 0.0}], aws_node_health_metrics:confidence_samples(rmq0, Scores)).

%%--------------------------------------------------------------------
%% collect_mf/2 against a running worker
%%--------------------------------------------------------------------

worker_config(SampleFun) ->
    #{
        self_node => rmq0,
        peers_fun => fun() -> [] end,
        sample_fun => SampleFun,
        window_max => 30,
        stale_ticks => 100,
        interval_ms => 3600000,
        analysis => aws_node_health:default_config()
    }.

drain_mfs(Acc) ->
    receive
        {mf, MF} -> drain_mfs([MF | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

collect_mf_emits_three_families_test() ->
    SampleFun = fun() -> #{rmq1 => 0.0, rmq2 => 0.0} end,
    {ok, Pid} = aws_node_health_worker:start_link(worker_config(SampleFun)),
    try
        gen_server:cast(aws_node_health_worker, {peer_row, rmq1, #{rmq0 => 1.0, rmq2 => 0.0}}),
        gen_server:cast(aws_node_health_worker, {peer_row, rmq2, #{rmq0 => 1.0, rmq1 => 0.0}}),
        _ = aws_node_health_worker:refresh(),
        Self = self(),
        ?assertEqual(
            ok,
            aws_node_health_metrics:collect_mf(default, fun(MF) -> Self ! {mf, MF} end)
        ),
        ?assertEqual(3, length(drain_mfs([])))
    after
        gen_server:stop(Pid)
    end.

collect_mf_is_noop_when_worker_absent_test() ->
    Self = self(),
    ?assertEqual(
        ok,
        aws_node_health_metrics:collect_mf(default, fun(MF) -> Self ! {mf, MF} end)
    ),
    ?assertEqual([], drain_mfs([])).
