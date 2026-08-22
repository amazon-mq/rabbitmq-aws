%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Calibration tests for aws_node_health, driven by captured peer-down
%% probability samples (see aws_node_health_fixtures). Each phase of each
%% capture has a known ground truth, and the detector must classify it
%% correctly with a single shared configuration.

-module(aws_node_health_tests).

-include_lib("eunit/include/eunit.hrl").

phase(Run, Name) ->
    {Name, Snapshots} = lists:keyfind(Name, 1, Run),
    Snapshots.

verdict(Run, Name) ->
    Result = aws_node_health:analyze(#{}, phase(Run, Name)),
    maps:get(verdict, Result).

result(Run, Name) ->
    aws_node_health:analyze(#{}, phase(Run, Name)).

%% Ground-truth verdicts, one config for all.

single_node_fault_is_attributed_test() ->
    ?assertEqual(
        {suspect, rmq0},
        verdict(aws_node_health_fixtures:run1(), single_fault_periodic)
    ).

%% Live-captured oscillating fault: the suspect flaps to ~1.0 on each burst then
%% re-normalises downward, so it spends little time above the elevated threshold
%% yet crosses the extreme threshold repeatedly while the clean pair stays at
%% 0.0. A time-above-threshold measure starves here; the flap-rate path attributes
%% it. This is the regression that motivated the flap-rate P1 trigger.
oscillating_fault_is_attributed_test() ->
    ?assertEqual(
        {suspect, rmq0},
        verdict(aws_node_health_fixtures:oscillating_fault(), oscillating_single_fault)
    ).

quiet_baseline_is_clean_test() ->
    ?assertEqual(clean, verdict(aws_node_health_fixtures:run2(), baseline)),
    ?assertEqual(clean, verdict(aws_node_health_fixtures:run3(), baseline)).

mild_uniform_congestion_is_not_attributed_test() ->
    %% The false-positive guard: under uniform light loss the busiest node
    %% reads elevated with no real fault; it must not be blamed.
    ?assertEqual(clean, verdict(aws_node_health_fixtures:run3(), uniform_4pct)).

fault_under_background_loss_is_attributed_test() ->
    ?assertEqual(
        {suspect, rmq0},
        verdict(aws_node_health_fixtures:run3(), rmq0_25pct)
    ).

heavy_uniform_congestion_is_cluster_wide_test() ->
    ?assertEqual(
        cluster_wide,
        verdict(aws_node_health_fixtures:run2(), uniform_8pct)
    ).

severe_fault_under_background_loss_is_attributed_test() ->
    ?assertEqual(
        {suspect, rmq0},
        verdict(aws_node_health_fixtures:run2(), rmq0_40pct)
    ).

%% Score-shape checks: the boolean and confidence must agree with the verdict.

attributed_node_scores_are_consistent_test() ->
    #{scores := Scores} = result(aws_node_health_fixtures:run1(), single_fault_periodic),
    #{
        rmq0 := #{suspected := S0, confidence := C0},
        rmq1 := #{suspected := S1},
        rmq2 := #{suspected := S2}
    } = Scores,
    ?assertEqual(1, S0),
    ?assertEqual(0, S1),
    ?assertEqual(0, S2),
    ?assert(C0 > 0.0).

cluster_wide_suspects_no_node_test() ->
    #{scores := Scores} = result(aws_node_health_fixtures:run2(), uniform_8pct),
    lists:foreach(
        fun({_Node, #{suspected := Susp, confidence := Conf}}) ->
            ?assertEqual(0, Susp),
            ?assertEqual(0.0, Conf)
        end,
        maps:to_list(Scores)
    ).

empty_window_is_clean_test() ->
    ?assertEqual(
        #{verdict => clean, scores => #{}},
        aws_node_health:analyze(#{}, [])
    ).

%% A non-empty window whose snapshots carry no observers must not crash.
observerless_window_is_clean_test() ->
    ?assertEqual(
        #{verdict => clean, scores => #{}},
        aws_node_health:analyze(#{}, [#{}, #{}])
    ).

%% A peer seen in only 2 of 30 snapshots, both extreme, must not be attributed:
%% the sustained/extreme fractions are over the whole window, so 2/30 is far
%% below the thresholds.
sparsely_observed_peer_is_not_attributed_test() ->
    Seen = #{rmq1 => #{rmq0 => 1.0, rmq2 => 0.0}, rmq2 => #{rmq0 => 1.0, rmq1 => 0.0}},
    Unseen = #{rmq1 => #{rmq2 => 0.0}, rmq2 => #{rmq1 => 0.0}},
    Window = [Seen, Seen] ++ lists:duplicate(28, Unseen),
    ?assertEqual(clean, maps:get(verdict, aws_node_health:analyze(#{}, Window))).

%% When the verdict is clean, no peer's confidence may be non-zero, even if the
%% candidate was mildly elevated (below the sustained fraction).
clean_verdict_reports_zero_confidence_test() ->
    Elevated = #{rmq1 => #{rmq0 => 0.6, rmq2 => 0.0}, rmq2 => #{rmq0 => 0.6, rmq1 => 0.0}},
    Quiet = #{rmq1 => #{rmq0 => 0.0, rmq2 => 0.0}, rmq2 => #{rmq0 => 0.0, rmq1 => 0.0}},
    Window = lists:duplicate(3, Elevated) ++ lists:duplicate(7, Quiet),
    #{verdict := Verdict, scores := Scores} = aws_node_health:analyze(#{}, Window),
    ?assertEqual(clean, Verdict),
    ?assertEqual(0.0, maps:get(confidence, maps:get(rmq0, Scores))).

%% P3 (bidirectional): under cluster-wide masking the suspect's inbound hovers at
%% the extreme threshold (so P2's fraction starves) and a background-elevated peer
%% compresses the margin (so P2's margin fails) -- yet rmq0 sees EVERY peer
%% degraded in its own row (bidirectional), which a healthy node would not. P3
%% must attribute rmq0. Mirrors the in-vivo node1@42%-over-6% case.
bidirectional_masked_fault_is_attributed_via_p3_test() ->
    A = #{
        rmq1 => #{rmq0 => 0.96, rmq2 => 0.45},
        rmq0 => #{rmq1 => 0.60, rmq2 => 0.65},
        rmq2 => #{rmq0 => 0.94, rmq1 => 0.28}
    },
    B = #{
        rmq1 => #{rmq0 => 0.84, rmq2 => 0.45},
        rmq0 => #{rmq1 => 0.60, rmq2 => 0.65},
        rmq2 => #{rmq0 => 0.85, rmq1 => 0.28}
    },
    Window = lists:duplicate(15, A) ++ lists:duplicate(15, B),
    ?assertEqual({suspect, rmq0}, maps:get(verdict, aws_node_health:analyze(#{}, Window))).

%% A congestion-elevated but healthy node reads high inbound (same as the mild
%% fault above) but sees its own peers normally -- its own row is low, so it is
%% not bidirectional and P3 must NOT fire. This is the false positive that simply
%% lowering the inbound thresholds would cause.
congestion_elevated_healthy_node_is_not_attributed_test() ->
    A = #{
        rmq1 => #{rmq0 => 0.95, rmq2 => 0.16},
        rmq0 => #{rmq1 => 0.17, rmq2 => 0.17},
        rmq2 => #{rmq0 => 0.93, rmq1 => 0.16}
    },
    B = #{
        rmq1 => #{rmq0 => 0.86, rmq2 => 0.16},
        rmq0 => #{rmq1 => 0.17, rmq2 => 0.17},
        rmq2 => #{rmq0 => 0.87, rmq1 => 0.16}
    },
    Window = lists:duplicate(15, A) ++ lists:duplicate(15, B),
    ?assertEqual(clean, maps:get(verdict, aws_node_health:analyze(#{}, Window))).

%% P3 needs the suspect's own gossiped row to confirm bidirectionality. When that
%% row is absent (its gossip never arrived) P3 cannot fire, and with the other
%% paths also not firing the verdict is clean rather than a guess.
p3_requires_suspect_own_row_test() ->
    A = #{rmq1 => #{rmq0 => 0.96, rmq2 => 0.10}, rmq2 => #{rmq0 => 0.94, rmq1 => 0.10}},
    B = #{rmq1 => #{rmq0 => 0.84, rmq2 => 0.10}, rmq2 => #{rmq0 => 0.85, rmq1 => 0.10}},
    Window = lists:duplicate(15, A) ++ lists:duplicate(15, B),
    ?assertEqual(clean, maps:get(verdict, aws_node_health:analyze(#{}, Window))).
