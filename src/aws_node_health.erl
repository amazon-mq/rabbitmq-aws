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
%% The decision is deliberately multi-path, because a single threshold on the
%% suspect's own level cannot work: empirically a real single-node fault and
%% mild cluster-wide congestion on the busiest node produce similar suspect
%% levels, while a heavier real fault produces a much higher one. The paths
%% are:
%%
%%   P1 (isolated): every other node is pristine AND the suspect flaps -- its
%%      probability crosses the extreme threshold repeatedly within the window.
%%      This catches an intermittent/periodic fault whose signal oscillates
%%      rather than pinning high: the loss comes and goes (e.g. bursts of drops
%%      separated by clean intervals), so the detector's probability spikes on
%%      each burst and falls back between them. A fraction-of-time-above-a-
%%      threshold measure starves on that low duty cycle, but each burst still
%%      produces a fresh upward crossing. The tell is that the rest of the
%%      cluster stays quiet while one node keeps spiking.
%%   P2 (extreme): the suspect is extreme for most of the window AND leads the
%%      next node by a wide margin. This catches a sustained fault that pins the
%%      probability high (verified in vivo: continuous ~48% loss keeps the
%%      failure-detector probability pinned near 1.0, it does not re-normalise
%%      downward), even when the other nodes are themselves mildly elevated by
%%      background loss.
%%   P3 (bidirectional): the suspect is elevated inbound AND its own outbound row
%%      shows every peer degraded -- the signature of a single node whose own link
%%      is bad, since its lost ACKs stall its inbound too. This attributes a masked
%%      fault that P2's margin misses under cluster-wide congestion, where a
%%      congestion-elevated healthy node looks identical by inbound level but is
%%      NOT bidirectional (it still sees its own peers normally). It fires only
%%      when the suspect's own outbound DOMINATES every other node's by a margin,
%%      so uniform congestion (where all nodes are roughly equally bidirectional)
%%      is left to the cluster-wide test rather than pinned on one node. It needs
%%      the suspect's own gossiped row to be present, so it complements rather than
%%      replaces P2 (which covers a severe fault whose own row may be missing).
%%
%% Precedence: the two dominant-single-node paths, P2 (sustained-extreme with a
%% wide margin) and P3 (bidirectional dominance), are evaluated BEFORE the
%% cluster-wide guard, so a node that clearly dominates is attributed even when a
%% background peer is also elevated. The cluster-wide guard then catches the
%% symmetric case -- two or more nodes sustained-elevated without one dominant
%% leader -- and reports cluster_wide rather than blaming any node; only the
%% weaker P1 flap path is considered after it.

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
        cluster_min_nodes => 2,
        %% P3 (bidirectional isolated fault): the suspect's inbound median must be
        %% at least this...
        bidir_inbound => 0.5,
        %% ...AND the suspect's own view of EVERY peer (the minimum over its own
        %% outbound row) must be at least this. A real single-node fault is
        %% bidirectional -- the faulty node's lost ACKs stall its inbound, so it
        %% sees all peers degraded -- whereas a merely congestion-elevated healthy
        %% node still sees its peers normally. This lets P3 attribute a masked
        %% fault that P2's margin misses, without the false positives that simply
        %% lowering the inbound thresholds would cause.
        bidir_outbound => 0.4,
        %% ...AND the suspect's own outbound must EXCEED every other node's own
        %% outbound by this margin. Under uniform cluster-wide congestion every
        %% node is roughly equally bidirectional, so none dominates and P3 stays
        %% out (the condition is cluster_wide); under a real single-node fault the
        %% culprit sees all peers degraded while the healthy nodes each see one
        %% peer (the culprit) high and the other low, so the culprit's own
        %% outbound clearly dominates. This relative test replaces an absolute
        %% one, which flickered false-positive at borderline uniform loss.
        bidir_margin => 0.2
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
analyze(_Config, _Window, Nodes) when length(Nodes) < 3 ->
    %% The cross-check majority attribution relies on requires at least three
    %% observed nodes; a two-node matrix cannot tell which side of a link is
    %% bad, so any verdict at that size is unreliable. Return the neutral
    %% verdict and empty scores rather than emit a nonsense attribution during
    %% transient rolling-restart windows.
    #{verdict => clean, scores => empty_scores(Nodes)};
analyze(Config, Window, Nodes) ->
    Extreme = maps:get(extreme, Config),
    %% Compute each node's inbound series once and reuse it for the median,
    %% the extreme fraction, and the flap count (each of which would otherwise
    %% rescan the whole window).
    InSeries = #{N => inbound_series(Window, N) || N <- Nodes},
    %% Extreme-fraction denominator: the configured window size, not the current
    %% (possibly still-filling) length. Right after boot or a worker restart the
    %% window is short; dividing by its length would let a couple of extreme ticks
    %% clear the "extreme for most of the window" bar that a full window needs
    %% ~extreme_frac * window samples for. Using the configured window imposes a
    %% brief warmup (the window must fill) before P2/P3 attribute, which is
    %% appropriate for a detector meant to catch sustained faults. Falls back to
    %% the actual length when the caller does not supply `window` (e.g. tests).
    WindowDenom = max(length(Window), maps:get(window, Config, length(Window))),
    Med = #{N => median_or_zero(maps:get(N, InSeries)) || N <- Nodes},
    FracExtreme =
        #{
            N => frac_at_least(maps:get(N, InSeries), Extreme, WindowDenom)
         || N <- Nodes
        },
    Flaps = #{N => flap_count(maps:get(N, InSeries), Extreme) || N <- Nodes},
    OwnOut = #{N => own_outbound_min(Window, N, Nodes) || N <- Nodes},
    OwnOutSeen = #{N => own_outbound_seen(Window, N, Nodes) || N <- Nodes},

    %% P2/P3 use the highest-median candidate: the node the *other* observers
    %% report as most degraded on average.
    P2Candidate = argmax_by(Nodes, Med),
    P2Others = [N || N <- Nodes, N =/= P2Candidate],
    OthersMaxMed = lists:max([0.0 | [maps:get(N, Med) || N <- P2Others]]),
    Margin = maps:get(P2Candidate, Med) - OthersMaxMed,
    NumElevated = length([N || N <- Nodes, maps:get(N, Med) >= maps:get(elevated, Config)]),
    P2Fire =
        Margin >= maps:get(margin, Config) andalso
            maps:get(P2Candidate, FracExtreme) >= maps:get(extreme_frac, Config),
    ClusterWide = NumElevated >= maps:get(cluster_min_nodes, Config),

    %% P3 (bidirectional): the candidate is bidirectionally degraded when its
    %% inbound is elevated, its own outbound row shows every peer degraded, AND
    %% its own outbound DOMINATES every other node's by a margin. The dominance
    %% test is what keeps P3 out under uniform congestion (where every node is
    %% roughly equally bidirectional, so none dominates -> left to the cluster-wide
    %% test), while still attributing a real single-node fault. But dominance is
    %% only meaningful when the *other* nodes' own gossip rows have actually
    %% been observed AND carry at least one peer view; otherwise
    %% own_outbound_min for those nodes reads 0.0 (an absent row, or a present
    %% but empty one from a just-restarted node) and any elevated candidate
    %% would trivially "dominate" that zero. Gate P3 on every other node having
    %% a usable own row (one that yields an own-outbound value) in the window,
    %% so the guard cannot fire under a real cluster-wide congestion event where
    %% gossip has stalled or a peer has just restarted.
    BidirInbound = maps:get(bidir_inbound, Config),
    BidirOutbound = maps:get(bidir_outbound, Config),
    BidirMargin = maps:get(bidir_margin, Config),
    CandOwnOut = maps:get(P2Candidate, OwnOut),
    OthersOwnOutMax = lists:max([0.0 | [maps:get(N, OwnOut) || N <- P2Others]]),
    P3OthersRowsPresent = lists:all(fun(N) -> maps:get(N, OwnOutSeen) end, P2Others),
    P3Fire =
        maps:get(P2Candidate, Med) >= BidirInbound andalso
            CandOwnOut >= BidirOutbound andalso
            CandOwnOut - OthersOwnOutMax >= BidirMargin andalso
            P3OthersRowsPresent,

    %% P1 (isolated flap): pick the node that flaps the most, not the one with
    %% the highest median. An intermittent low-duty-cycle fault has near-zero
    %% median (spikes to ~1.0 during bursts, decays to ~0.0 between them), so
    %% choosing by median would not name it and its flaps would go uncounted. P1
    %% therefore chooses its own candidate independently of P2/P3.
    P1Candidate = argmax_by(Nodes, Flaps),
    P1Others = [N || N <- Nodes, N =/= P1Candidate],
    Pristine = maps:get(pristine, Config),
    FlapMin = maps:get(flap_min, Config),
    %% Others must be quiet on BOTH signals for P1 to attribute: elevated
    %% median or their own flapping would make this a cluster-level condition,
    %% not an isolated fault.
    P1OthersPristine = lists:all(
        fun(N) ->
            maps:get(N, Med) < Pristine andalso
                maps:get(N, Flaps) < FlapMin
        end,
        P1Others
    ),
    P1Fire = P1OthersPristine andalso maps:get(P1Candidate, Flaps) >= FlapMin,

    %% Verdict and confidence are tied together: each firing path publishes its
    %% own candidate and its own confidence, so a suspect's confidence always
    %% describes the path that actually attributed it. When nothing fires we
    %% report cluster_wide (if applicable) or clean, with no suspect.
    {Verdict, CandConf} =
        if
            P2Fire ->
                {{suspect, P2Candidate}, maps:get(P2Candidate, FracExtreme)};
            P3Fire ->
                {{suspect, P2Candidate}, CandOwnOut};
            ClusterWide ->
                {cluster_wide, 0.0};
            P1Fire ->
                {{suspect, P1Candidate}, min(1.0, maps:get(P1Candidate, Flaps) / (2 * FlapMin))};
            true ->
                {clean, 0.0}
        end,

    Scores =
        #{
            N => #{
                inbound => maps:get(N, Med),
                confidence => confidence_for(N, Verdict, CandConf),
                suspected => suspected_for(N, Verdict)
            }
         || N <- Nodes
        },
    #{verdict => Verdict, scores => Scores}.

%% The candidate confidence applies only to the attributed suspect; every other
%% node reads 0.0. Derived from the verdict, like suspected_for/2, so the two
%% cannot disagree.
confidence_for(N, {suspect, N}, CandConf) -> CandConf;
confidence_for(_, _, _) -> 0.0.

suspected_for(N, {suspect, N}) -> 1;
suspected_for(_, _) -> 0.

empty_scores(Nodes) ->
    #{N => #{inbound => 0.0, confidence => 0.0, suspected => 0} || N <- Nodes}.

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

%% Median of a precomputed inbound series, or 0.0 when the node was never
%% observed in the window.
-spec median_or_zero([float()]) -> float().
median_or_zero([]) -> 0.0;
median_or_zero(Series) -> median(Series).

%% Node with the greatest value in Scores. Sorts the nodes first so ties break
%% lexicographically (deterministic), then folds picking the strictly-greater
%% element. Scores must contain a numeric value for every node in Nodes.
-spec argmax_by([node()], #{node() => number()}) -> node().
argmax_by(Nodes, Scores) ->
    [First | Rest] = lists:sort(Nodes),
    lists:foldl(
        fun(N, Best) ->
            case maps:get(N, Scores) > maps:get(Best, Scores) of
                true -> N;
                false -> Best
            end
        end,
        First,
        Rest
    ).

%% Whether a node's own gossip row is usable for the P3 dominance comparison:
%% it must yield at least one own-view median in the window. A fully-absent row
%% and a present-but-empty one (a just-restarted node that gossiped before
%% observing any peer) both leave own_outbound_min at 0.0, so P3 must treat them
%% alike; letting an elevated candidate dominate that zero is the misattribution
%% the guard prevents.
-spec own_outbound_seen([snapshot()], node(), [node()]) -> boolean().
own_outbound_seen(Window, N, Nodes) ->
    Peers = [Peer || Peer <- Nodes, Peer =/= N],
    lists:any(fun(Peer) -> own_view_median(Window, N, Peer) =/= none end, Peers).

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

%% Fraction of the window (by Denom) in which a node's inbound score is at or
%% above Threshold. Denom is the configured window size, not the number of
%% samples in the series, so (a) a peer seen in only a handful of snapshots
%% cannot reach a high fraction on a couple of samples, and (b) a still-filling
%% window right after boot cannot reach the fraction on a couple of extreme
%% ticks either -- both require ~Denom*frac genuine extreme samples.
-spec frac_at_least([float()], number(), pos_integer()) -> float().
frac_at_least(Series, Threshold, Denom) ->
    Elevated = [V || V <- Series, V >= Threshold],
    length(Elevated) / max(1, Denom).

%% Number of upward crossings of Threshold in a node's inbound series: transitions
%% from below the threshold to at-or-above it. An intermittent fault produces one
%% crossing per loss burst -- the probability falls back between bursts and
%% re-crosses on the next -- so this holds up where a fraction-of-time-above-
%% threshold measure starves on the low duty cycle. A sustained fault instead
%% pins the signal high and produces no crossings after the first, which is why
%% P2 (fraction extreme) covers that case separately.
-spec flap_count([float()], number()) -> non_neg_integer().
flap_count(Series, Threshold) ->
    %% Series is newest-first (push_window/3 prepends); up-crossing counting is
    %% time-directional, so reverse to chronological order first. Counting
    %% newest-first would tally falling edges and miss a burst still active at
    %% the newest sample, undercounting by one.
    count_upcrossings(lists:reverse(Series), Threshold, undefined, 0).

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

%% N's own outbound health: for each other node, the median over the window of
%% N's OWN view of that node (M[N][Peer]), then the minimum across peers. A real
%% single-node fault is bidirectional -- the faulty node's lost ACKs stall its
%% inbound, so it sees EVERY peer degraded and this minimum is high -- whereas a
%% congestion-elevated but healthy node sees its peers roughly normally, so the
%% minimum stays low. Returns 0.0 when N's own row never appears in the window
%% (its gossip never arrived), so P3 cannot fire on an unconfirmed suspect.
-spec own_outbound_min([snapshot()], node(), [node()]) -> float().
own_outbound_min(Window, N, Nodes) ->
    PeerMeds = [
        M
     || Peer <- Nodes,
        Peer =/= N,
        {ok, M} <- [own_view_median(Window, N, Peer)]
    ],
    case PeerMeds of
        [] -> 0.0;
        _ -> lists:min(PeerMeds)
    end.

%% Median over the window of Observer's own view of Peer (M[Observer][Peer]),
%% skipping snapshots where Observer's row is absent or does not mention Peer.
-spec own_view_median([snapshot()], node(), node()) -> none | {ok, float()}.
own_view_median(Window, Observer, Peer) ->
    Vals = lists:filtermap(
        fun(Snapshot) ->
            case maps:get(Observer, Snapshot, undefined) of
                View when is_map(View) ->
                    case maps:find(Peer, View) of
                        {ok, P} -> {true, P};
                        error -> false
                    end;
                _ ->
                    false
            end
        end,
        Window
    ),
    case Vals of
        [] -> none;
        _ -> {ok, median(Vals)}
    end.
