%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Property-based tests for the pure node-health scorer, aws_node_health:analyze/2.
%% These complement the example-based cases in aws_node_health_tests by probing
%% invariants that must hold for ANY observer-by-peer matrix, i.e. the boundaries
%% the scenario tests do not enumerate (sparse and absent rows, varied node counts
%% including more than three, fully symmetric matrices):
%%   * analyze/2 never crashes and always returns a well-formed verdict + scores;
%%   * the scores never contradict the verdict -- only the attributed suspect reads
%%     suspected=1 and non-zero confidence, and every score stays in [0,1];
%%   * a fully symmetric matrix is never blamed on a single node;
%%   * below three observed nodes the verdict is unconditionally clean.
-module(prop_aws_node_health_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ITERATIONS, 1000).
-define(NODES, [rmq0, rmq1, rmq2, rmq3, rmq4]).

all() ->
    [
        prop_analyze_never_crashes,
        prop_verdict_scores_consistent,
        prop_uniform_matrix_not_attributed,
        prop_below_three_nodes_is_clean
    ].

%%--------------------------------------------------------------------
%% Generators
%%--------------------------------------------------------------------

prob() ->
    float(0.0, 1.0).

%% 2..5 distinct node names (nodes are interchangeable, so a prefix suffices).
node_set() ->
    ?LET(N, integer(2, 5), lists:sublist(?NODES, N)).

%% One observer's view over its peers: each peer is present with a probability
%% in [0,1] or absent (sparse rows are the norm mid-gossip).
gen_view(Nodes, Self) ->
    ?LET(
        Pairs,
        [{P, oneof([absent, prob()])} || P <- Nodes, P =/= Self],
        maps:from_list([{P, V} || {P, V} <- Pairs, V =/= absent])
    ).

%% One snapshot: each node is an observer (with a view) or absent this tick.
gen_snapshot(Nodes) ->
    ?LET(
        Rows,
        [{O, oneof([absent, gen_view(Nodes, O)])} || O <- Nodes],
        maps:from_list([{O, V} || {O, V} <- Rows, V =/= absent])
    ).

%% A rolling window of 1..30 snapshots over a fixed node set.
gen_window(Nodes) ->
    ?LET(Len, integer(1, 30), vector(Len, gen_snapshot(Nodes))).

matrix() ->
    ?LET(Nodes, node_set(), ?LET(Window, gen_window(Nodes), {Nodes, Window})).

%% A fully symmetric window: every present observer sees every peer at the same
%% constant probability, repeated for the whole window. No node dominates, so no
%% single node may be attributed.
uniform_matrix() ->
    ?LET(
        {Nodes, V, Len},
        {node_set(), prob(), integer(1, 30)},
        begin
            Snapshot = maps:from_list([
                {O, maps:from_list([{P, V} || P <- Nodes, P =/= O])}
             || O <- Nodes
            ]),
            lists:duplicate(Len, Snapshot)
        end
    ).

two_node_window() ->
    gen_window(lists:sublist(?NODES, 2)).

%%--------------------------------------------------------------------
%% Properties
%%--------------------------------------------------------------------

prop_analyze_never_crashes(_Config) ->
    rabbit_ct_proper_helpers:run_proper(
        fun() ->
            ?FORALL(
                {_Nodes, Window},
                matrix(),
                begin
                    #{verdict := V, scores := S} = aws_node_health:analyze(#{}, Window),
                    valid_verdict(V) andalso is_map(S)
                end
            )
        end,
        [],
        ?ITERATIONS
    ).

prop_verdict_scores_consistent(_Config) ->
    rabbit_ct_proper_helpers:run_proper(
        fun() ->
            ?FORALL(
                {_Nodes, Window},
                matrix(),
                begin
                    #{verdict := V, scores := S} = aws_node_health:analyze(#{}, Window),
                    Suspect = suspect_of(V),
                    SuspectPresent = Suspect =:= none orelse maps:is_key(Suspect, S),
                    SuspectPresent andalso
                        maps:fold(
                            fun(N, Score, Acc) -> Acc andalso score_ok(N, Score, Suspect) end,
                            true,
                            S
                        )
                end
            )
        end,
        [],
        ?ITERATIONS
    ).

prop_uniform_matrix_not_attributed(_Config) ->
    rabbit_ct_proper_helpers:run_proper(
        fun() ->
            ?FORALL(
                Window,
                uniform_matrix(),
                case maps:get(verdict, aws_node_health:analyze(#{}, Window)) of
                    {suspect, _} -> false;
                    _ -> true
                end
            )
        end,
        [],
        ?ITERATIONS
    ).

prop_below_three_nodes_is_clean(_Config) ->
    rabbit_ct_proper_helpers:run_proper(
        fun() ->
            ?FORALL(
                Window,
                two_node_window(),
                clean =:= maps:get(verdict, aws_node_health:analyze(#{}, Window))
            )
        end,
        [],
        ?ITERATIONS
    ).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

valid_verdict(clean) -> true;
valid_verdict(cluster_wide) -> true;
valid_verdict({suspect, N}) -> is_atom(N);
valid_verdict(_) -> false.

suspect_of({suspect, N}) -> N;
suspect_of(_) -> none.

%% A node's score is consistent with the verdict: suspected is 0 or 1 and set iff
%% the node is the attributed suspect; confidence is in [0,1] and non-zero only
%% for the suspect; inbound is in [0,1].
score_ok(N, #{inbound := In, confidence := C, suspected := Su}, Suspect) ->
    lists:member(Su, [0, 1]) andalso
        In >= 0.0 andalso In =< 1.0 andalso
        C >= 0.0 andalso C =< 1.0 andalso
        (Su =:= 1) =:= (N =:= Suspect) andalso
        (N =:= Suspect orelse C =:= 0.0).
