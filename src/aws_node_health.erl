%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% This module attributes a degraded peer node from a window of per-node
%% peer-down probability snapshots.
%%
%% Each node observes the reachability of its peers (via the node failure
%% detector) as a probability in [0.0, 1.0]. A single node's view is only one
%% row of the picture; the nodes share their rows so that any node holds the
%% full observer x peer matrix. This module consumes a window of those matrices
%% and decides whether one node is the culprit, or whether the degradation is
%% cluster-wide (and so must not be attributed to a single node).
%%
%% The decision is deliberately two-path, because a single threshold on the
%% suspect's own level cannot work: empirically a real single-node fault and
%% mild cluster-wide congestion on the busiest node produce similar suspect
%% levels, while a heavier real fault produces a much higher one. The two paths
%% are:
%%
%%   P1 (isolated): every other node is pristine AND the suspect flaps -- its
%%      probability crosses the extreme threshold repeatedly within the window.
%%      This catches a partial/periodic fault whose signal oscillates rather
%%      than pinning high (the node failure detector re-normalises a steady loss
%%      downward over time, so a time-above-threshold measure starves, but each
%%      loss burst still produces a fresh upward crossing). The tell is that the
%%      rest of the cluster stays quiet while one node keeps spiking.
%%   P2 (extreme): the suspect is extreme for most of the window AND leads the
%%      next node by a wide margin. This catches a severe fault that pins high
%%      even when the other nodes are themselves mildly elevated by background
%%      loss.
%%
%% A cluster-wide guard fires first: if two or more nodes are sustained-elevated
%% without one extreme leader, the condition is symmetric and is reported as
%% cluster_wide rather than blamed on any node.

-module(aws_node_health).

-export([default_config/0, analyze/2]).

-type prob() :: float().
-type view() :: #{node() => prob()}.
-type snapshot() :: #{node() => view()}.
-type verdict() :: clean | cluster_wide | {suspect, node()}.
-type score() :: #{inbound => float(), confidence => float(), suspected => 0 | 1}.
-type result() :: #{verdict => verdict(), scores => #{node() => score()}}.

-export_type([snapshot/0, verdict/0, result/0]).

-spec default_config() -> map().
default_config() ->
    %% inbound prob considered elevated (used by the cluster-wide test)
    #{
        elevated => 0.5,
        %% inbound prob considered extreme (P2 threshold and the flap crossing level)
        extreme => 0.9,
        %% a non-suspect node with median inbound below this is quiet
        pristine => 0.05,
        %% P1: min upward crossings of `extreme` by the suspect within the window
        flap_min => 2,
        %% P2: fraction of window suspect is extreme
        extreme_frac => 0.8,
        %% P2: min lead of suspect over next node
        margin => 0.5,
        %% this many elevated nodes => cluster_wide
        cluster_min_nodes => 2
    }.

-spec analyze(map(), [snapshot()]) -> result().
analyze(_Config, []) ->
    #{verdict => clean, scores => #{}};
analyze(Config0, Window) ->
    case all_nodes(Window) of
        [] ->
            %% A non-empty window whose snapshots carry no observers yields no
            %% nodes to judge; treat it as clean rather than crashing.
            #{verdict => clean, scores => #{}};
        Nodes ->
            analyze(maps:merge(default_config(), Config0), Window, Nodes)
    end.

-spec analyze(map(), [snapshot()], [node()]) -> result().
analyze(Config, Window, Nodes) ->
    Med = #{N => node_median(Window, N) || N <- Nodes},
    FracExtreme = #{N => frac_elevated(Window, N, maps:get(extreme, Config)) || N <- Nodes},
    Candidate = argmax_median(Nodes, Med),
    Others = [N || N <- Nodes, N =/= Candidate],
    OthersMaxMed = lists:max([0.0 | [maps:get(N, Med) || N <- Others]]),
    OthersPristine = lists:all(
        fun(N) -> maps:get(N, Med) < maps:get(pristine, Config) end,
        Others
    ),
    NumElevated = length([N || N <- Nodes, maps:get(N, Med) >= maps:get(elevated, Config)]),
    Margin = maps:get(Candidate, Med) - OthersMaxMed,
    FlapMin = maps:get(flap_min, Config),
    Flaps = flap_count(Window, Candidate, maps:get(extreme, Config)),

    P1Gate = OthersPristine,
    P2Gate = Margin >= maps:get(margin, Config),
    P1Fire = P1Gate andalso Flaps >= FlapMin,
    P2Fire = P2Gate andalso maps:get(Candidate, FracExtreme) >= maps:get(extreme_frac, Config),
    ClusterWide = NumElevated >= maps:get(cluster_min_nodes, Config),

    Verdict =
        if
            P2Fire -> {suspect, Candidate};
            ClusterWide -> cluster_wide;
            P1Fire -> {suspect, Candidate};
            true -> clean
        end,

    %% Confidence is reported only for a named suspect. A clean or cluster-wide
    %% verdict carries no per-node confidence, so the confidence gauge can never
    %% contradict the suspected flag (a peer that is not suspected reads 0.0).
    CandConf =
        case Verdict of
            {suspect, _} ->
                P1Conf =
                    case P1Gate of
                        true -> min(1.0, Flaps / (2 * FlapMin));
                        false -> 0.0
                    end,
                P2Conf = maybe_conf(P2Gate, Candidate, FracExtreme),
                lists:max([P1Conf, P2Conf]);
            _ ->
                0.0
        end,
    Scores =
        #{
            N => #{
                inbound => maps:get(N, Med),
                confidence => confidence_for(N, Candidate, CandConf),
                suspected => suspected_for(N, Verdict)
            }
         || N <- Nodes
        },
    #{verdict => Verdict, scores => Scores}.

maybe_conf(true, Candidate, Fracs) -> maps:get(Candidate, Fracs);
maybe_conf(false, _Candidate, _Fracs) -> 0.0.

confidence_for(N, N, CandConf) -> CandConf;
confidence_for(_, _, _) -> 0.0.

suspected_for(N, {suspect, N}) -> 1;
suspected_for(_, _) -> 0.

%% A node's inbound score at one snapshot is the median of the other nodes'
%% views of it. Absent views are skipped; a snapshot with no observers of N
%% contributes nothing to N's series.
-spec snapshot_inbound(snapshot(), node()) -> none | {ok, float()}.
snapshot_inbound(Snapshot, N) ->
    Vals = [
        maps:get(N, View)
     || {Observer, View} <- maps:to_list(Snapshot),
        Observer =/= N,
        is_map(View),
        maps:is_key(N, View)
    ],
    case Vals of
        [] -> none;
        _ -> {ok, median(Vals)}
    end.

inbound_series(Window, N) ->
    [M || Snapshot <- Window, {ok, M} <- [snapshot_inbound(Snapshot, N)]].

node_median(Window, N) ->
    case inbound_series(Window, N) of
        [] -> 0.0;
        Series -> median(Series)
    end.

argmax_median(Nodes, Med) ->
    [First | Rest] = lists:sort(Nodes),
    lists:foldl(
        fun(N, Best) ->
            case maps:get(N, Med) > maps:get(Best, Med) of
                true -> N;
                false -> Best
            end
        end,
        First,
        Rest
    ).

all_nodes(Window) ->
    lists:usort(
        lists:flatten(
            [
                [Observer | maps:keys(View)]
             || Snapshot <- Window, {Observer, View} <- maps:to_list(Snapshot)
            ]
        )
    ).

-spec median([number()]) -> float().
median(List) ->
    Sorted = lists:sort(List),
    N = length(Sorted),
    case N rem 2 of
        1 -> float(lists:nth((N div 2) + 1, Sorted));
        0 -> (lists:nth(N div 2, Sorted) + lists:nth((N div 2) + 1, Sorted)) / 2
    end.

%% Fraction of the whole window in which N's inbound score is at or above
%% Threshold. The denominator is the window length, not the number of snapshots
%% in which N happened to be observed, so a peer seen in only a handful of
%% snapshots cannot reach a high fraction (and be falsely attributed) on a
%% couple of samples.
-spec frac_elevated([snapshot()], node(), number()) -> float().
frac_elevated([], _N, _Threshold) ->
    0.0;
frac_elevated(Window, N, Threshold) ->
    Elevated = [V || V <- inbound_series(Window, N), V >= Threshold],
    length(Elevated) / length(Window).

%% Number of upward crossings of Threshold in N's inbound series across the
%% window: transitions from below the threshold to at-or-above it. A periodic
%% fault produces one crossing per loss burst even as the detector re-normalises
%% the baseline downward, so this holds up where a time-above-threshold measure
%% starves. A steady, pinned-high signal produces no crossings after the first,
%% which is why P2 (fraction extreme) covers that case separately.
-spec flap_count([snapshot()], node(), number()) -> non_neg_integer().
flap_count(Window, N, Threshold) ->
    count_upcrossings(inbound_series(Window, N), Threshold, undefined, 0).

-spec count_upcrossings([float()], number(), float() | undefined, non_neg_integer()) ->
    non_neg_integer().
count_upcrossings([], _Threshold, _Prev, Acc) ->
    Acc;
count_upcrossings([V | Rest], Threshold, Prev, Acc) ->
    Acc2 =
        case Prev of
            P when is_number(P), P < Threshold, V >= Threshold -> Acc + 1;
            _ -> Acc
        end,
    count_upcrossings(Rest, Threshold, V, Acc2).
