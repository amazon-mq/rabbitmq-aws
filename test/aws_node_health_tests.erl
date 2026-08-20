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
