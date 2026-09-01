%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Gossip worker for node-health attribution.
%%
%% Each node samples its own view of every peer's down-probability from the
%% node failure detector, then casts that row to the other nodes. Every node
%% therefore accumulates the full observer x peer matrix and can, on its own,
%% decide whether a single peer is degraded (see aws_node_health).
%%
%% The worker keeps a bounded rolling window of assembled matrices and, each
%% tick, recomputes the verdict and per-node scores. Those are held in state
%% and read live by the Prometheus collector at scrape time (never pushed), so
%% a slow or crashed scrape can never block the detector.
%%
%% Sampling, the peer list, and the local node name are injectable so the
%% detector can be driven in tests without a real cluster or failure detector.

-module(aws_node_health_worker).

-behaviour(gen_server).

-include("aws.hrl").

-export([start_link/0, start_link/1, report/0, refresh/0]).

%% Bound on how long a metrics scrape will wait for the worker to reply. If the
%% worker's mailbox is backed up past this, the scrape treats it as unavailable
%% rather than stalling.
-define(REPORT_TIMEOUT_MS, 1000).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Exported for unit tests of the otherwise-internal pure helpers.
-ifdef(TEST).
-export([
    record_row/4,
    strip_ticks/1,
    prune_stale_rows/3,
    push_window/3,
    valid_row/1,
    resolve_published/4,
    default_config/0
]).
-endif.

-type view() :: #{node() => float()}.
-type snapshot() :: #{node() => view()}.
-type rows() :: #{node() => {integer(), view()}}.

-export_type([view/0]).

-record(state, {
    self_node :: node(),
    peers_fun :: fun(() -> [node()]),
    sample_fun :: fun(() -> view()),
    rows = #{} :: rows(),
    own_row = #{} :: view(),
    window = [] :: [snapshot()],
    window_max :: pos_integer(),
    stale_ticks :: non_neg_integer(),
    interval_ms :: pos_integer(),
    analysis :: map(),
    tick = 0 :: integer(),
    latest = #{verdict => clean, scores => #{}} :: aws_node_health:result(),
    %% Hysteresis (debounce) so a noisy single-tick verdict cannot flip the
    %% published `suspected` flag. `latest` above is the DEBOUNCED result the
    %% collector reads; these fields carry the debounce state between ticks.
    confirm_ticks :: pos_integer(),
    clear_ticks :: pos_integer(),
    %% node currently being armed toward confirmation, and its consecutive count
    deb_stream = none :: node() | none,
    deb_arm = 0 :: non_neg_integer(),
    %% published suspect (or none), its held confidence, and consecutive misses
    deb_confirmed = none :: node() | none,
    deb_conf = 0.0 :: float(),
    deb_miss = 0 :: non_neg_integer(),
    %% Independent debounce for the cluster_wide (symmetric congestion) verdict,
    %% reusing confirm_ticks/clear_ticks: whether it is currently published, the
    %% consecutive cluster_wide count toward confirmation, and the consecutive
    %% non-cluster_wide count toward clearing.
    cw_confirmed = false :: boolean(),
    cw_arm = 0 :: non_neg_integer(),
    cw_miss = 0 :: non_neg_integer()
}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% This node's own most recent view of its peers (its raw failure-detector
%% row) together with the latest verdict and scores, fetched in one call so a
%% scrape reads a consistent pair from a single tick and blocks at most
%% REPORT_TIMEOUT_MS rather than making two separate calls.
-spec report() -> {view(), aws_node_health:result()}.
report() ->
    gen_server:call(?MODULE, report, ?REPORT_TIMEOUT_MS).

%% Run one sample/gossip/compute cycle synchronously and return the new latest.
%% The periodic timer runs the same cycle; this is for tests and diagnostics.
-spec refresh() -> aws_node_health:result().
refresh() ->
    gen_server:call(?MODULE, refresh).

%%--------------------------------------------------------------------
%% Defaults / config
%%--------------------------------------------------------------------

%% Runtime dependencies (local node, peers, sampler) come from here; the
%% operator-tunable numeric knobs and scoring thresholds come from
%% aws_node_health_config, layered on top so an explicit start_link/1 config
%% still overrides everything.
-spec default_config() -> map().
default_config() ->
    Runtime = #{
        %% local node whose view is sampled and gossiped
        self_node => node(),
        %% cluster peers to gossip rows to: the configured cluster members
        %% other than this node. Uses list_members/0 (a local metadata-store
        %% read), deliberately NOT list_running/0. list_running/0 does a
        %% cluster-wide erpc:multicall to every member's rabbit:is_running with
        %% a 10s timeout (rabbit_nodes:?FILTER_RPC_TIMEOUT), so a
        %% network-degraded peer -- the very fault this detector exists to
        %% catch -- would block the sample/gossip cycle for up to 10s per tick.
        %% That throttles the decision window ~10x, so the elevated signal
        %% never fills enough of the window to cross the firing thresholds and
        %% the culprit is never attributed. Gossiping a row to a down or
        %% unreachable member is a harmless no-op (the cast is dropped), so
        %% filtering by "running" buys nothing and costs correctness.
        peers_fun => fun() -> rabbit_nodes:list_members() -- [node()] end,
        %% samples this node's per-peer down-probability view
        sample_fun => fun sample_failure_probabilities/0
    },
    maps:merge(Runtime, aws_node_health_config:worker_config()).

%% Bound on how long a single failure-probability sample may take. Kept well
%% below interval_ms so a backed-up aten_sink cannot stall the cycle.
-define(SAMPLE_TIMEOUT_MS, 500).

%% The node failure detector exposes each node's view of its peers as a map of
%% peer -> probability, read via aten_sink:get_failure_probabilities/0, which
%% does a gen_server:call to aten_sink with aten's default 5000ms timeout. Under
%% the very congestion this detector exists to attribute, aten_sink's mailbox can
%% back up, so we must not run that call unbounded on the cycle path (that would
%% reintroduce the same stall the list_running fix removed). Run it in a
%% short-lived monitored process and give up after SAMPLE_TIMEOUT_MS, yielding an
%% empty view (as for any other sampling failure) so the loop stays responsive.
-spec sample_failure_probabilities() -> view().
sample_failure_probabilities() ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        View =
            try
                aten_sink:get_failure_probabilities()
            catch
                _:_ -> #{}
            end,
        Parent ! {sample_result, self(), View}
    end),
    receive
        {sample_result, Pid, View} when is_map(View) ->
            erlang:demonitor(Ref, [flush]),
            View;
        {'DOWN', Ref, process, Pid, Reason} ->
            ?AWS_LOG_ERROR("failed to sample peer failure probabilities: ~tp", [Reason]),
            #{}
    after ?SAMPLE_TIMEOUT_MS ->
        erlang:demonitor(Ref, [flush]),
        exit(Pid, kill),
        ?AWS_LOG_ERROR("peer failure-probability sample timed out after ~b ms", [
            ?SAMPLE_TIMEOUT_MS
        ]),
        #{}
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Config0) ->
    Config = maps:merge(default_config(), Config0),
    Interval = maps:get(interval_ms, Config),
    State = #state{
        self_node = maps:get(self_node, Config),
        peers_fun = maps:get(peers_fun, Config),
        sample_fun = maps:get(sample_fun, Config),
        window_max = maps:get(window_max, Config),
        stale_ticks = maps:get(stale_ticks, Config),
        interval_ms = Interval,
        %% Tell the scorer the configured window size so its extreme-fraction
        %% denominator is the full window, not the still-filling length.
        analysis = (maps:get(analysis, Config))#{window => maps:get(window_max, Config)},
        confirm_ticks = maps:get(confirm_ticks, Config),
        clear_ticks = maps:get(clear_ticks, Config)
    },
    schedule_tick(Interval),
    {ok, State}.

handle_call(report, _From, State) ->
    {reply, {State#state.own_row, State#state.latest}, State};
handle_call(refresh, _From, State0) ->
    State = cycle(State0),
    {reply, State#state.latest, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({peer_row, From, Row}, State) when is_map(Row) ->
    %% A row is a #{peer => probability_float}. Reject a row whose values are
    %% not numeric so a version-skewed or buggy peer cannot crash the cycle
    %% path downstream in median/1. Silently drop unknown casts.
    case valid_row(Row) of
        true ->
            Rows = record_row(State#state.rows, From, Row, State#state.tick),
            {noreply, State#state{rows = Rows}};
        false ->
            ?AWS_LOG_WARNING(
                "node_health: dropping malformed peer_row from ~p "
                "(invalid keys or out-of-range values)",
                [From]
            ),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State0) ->
    State = cycle(State0),
    schedule_tick(State#state.interval_ms),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal: the sample/gossip/compute cycle
%%--------------------------------------------------------------------

cycle(State0) ->
    Tick = State0#state.tick + 1,
    SampleFun = State0#state.sample_fun,
    OwnRow = SampleFun(),
    Rows0 = record_row(State0#state.rows, State0#state.self_node, OwnRow, Tick),
    gossip(State0#state.peers_fun, State0#state.self_node, OwnRow),
    StaleTicks = State0#state.stale_ticks,
    %% Prune stale rows once, then derive this tick's snapshot (observer -> view)
    %% from the pruned map, so the staleness predicate runs a single pass. The
    %% prune is written back to persistent state so node names that never come
    %% back (e.g. after an instance replacement) drop out of state, not just out
    %% of each transient snapshot.
    Rows = prune_stale_rows(Rows0, Tick, StaleTicks),
    Snapshot = strip_ticks(Rows),
    Window = push_window(State0#state.window, Snapshot, State0#state.window_max),
    Raw = aws_node_health:analyze(State0#state.analysis, Window),
    {State1, Published} = debounce(Raw, State0),
    State1#state{
        rows = Rows,
        own_row = OwnRow,
        window = Window,
        tick = Tick,
        latest = Published
    }.

%% Hysteresis. `analyze/2` produces a fresh verdict each tick; that per-tick
%% verdict is noisy (a healthy node can momentarily win the argmax under
%% congestion), so the published `suspected` flag must not flip on a single
%% tick. A node becomes the published suspect only after it is the raw suspect
%% for `confirm_ticks` consecutive ticks, and stays suspected until it is no
%% longer the raw suspect for `clear_ticks` consecutive ticks. Applies to every
%% path (P1/P2/P3) uniformly, since it debounces the final verdict.
%%
%% This suspect debouncer is a node-tracking, two-phase state machine: it tracks
%% which node is armed/confirmed, carries that node's confidence, re-arms (to 1)
%% when the raw suspect changes node, and clears at once on a `cluster_wide`
%% verdict. It deliberately does NOT share a helper with debounce_cluster_wide/4,
%% which debounces a plain boolean flag: unifying them would have to parameterize
%% away the node identity, confidence, and per-stream re-arm, adding more
%% complexity than the shared confirm/clear threshold check removes.
-spec debounce(aws_node_health:result(), #state{}) -> {#state{}, aws_node_health:result()}.
debounce(Raw, State) ->
    RawVerdict = maps:get(verdict, Raw),
    RawSuspect =
        case RawVerdict of
            {suspect, N} -> N;
            _ -> none
        end,
    %% Arm the counter for the current raw suspect (reset on a change or none).
    {Stream, Arm} =
        case RawSuspect of
            none -> {none, 0};
            S when S =:= State#state.deb_stream -> {S, State#state.deb_arm + 1};
            S -> {S, 1}
        end,
    ConfirmTicks = State#state.confirm_ticks,
    ClearTicks = State#state.clear_ticks,
    {Confirmed, Conf, Miss} =
        case RawVerdict of
            cluster_wide ->
                %% An explicit symmetric verdict clears any held suspect at once.
                %% This is safe because the scorer evaluates the dominant-node
                %% paths (P2/P3) BEFORE the cluster-wide guard, so a raw
                %% cluster_wide verdict is emitted only when no node dominates
                %% this tick (see aws_node_health:analyze/3's verdict order,
                %% pinned by the precedence tests in aws_node_health_tests). A
                %% stale suspect must not be held under it, which would defeat
                %% the very guard cluster_wide exists to provide. If a future
                %% scorer change ever let cluster_wide co-occur with a dominating
                %% node, revisit this instant clear.
                {none, 0.0, 0};
            _ ->
                case State#state.deb_confirmed of
                    none ->
                        case RawSuspect =/= none andalso Arm >= ConfirmTicks of
                            true -> {RawSuspect, raw_conf(Raw, RawSuspect), 0};
                            false -> {none, 0.0, 0}
                        end;
                    C ->
                        case RawSuspect =:= C of
                            %% still the raw suspect: refresh held confidence, reset misses
                            true ->
                                {C, raw_conf(Raw, C), 0};
                            false ->
                                Miss0 = State#state.deb_miss + 1,
                                case Miss0 >= ClearTicks of
                                    true -> {none, 0.0, 0};
                                    false -> {C, State#state.deb_conf, Miss0}
                                end
                        end
                end
        end,
    %% Independent debounce for the cluster_wide (symmetric congestion) verdict,
    %% reusing the same confirm/clear thresholds: it is asserted only after the
    %% raw verdict has been cluster_wide for confirm_ticks consecutive ticks, and
    %% de-asserted only after clear_ticks consecutive non-cluster_wide ticks. This
    %% keeps the congestion signal from flapping on a single noisy tick, exactly
    %% as the suspect flag is debounced.
    {CwOn, CwArm, CwMiss} = debounce_cluster_wide(RawVerdict, State, ConfirmTicks, ClearTicks),
    State1 = State#state{
        deb_stream = Stream,
        deb_arm = Arm,
        deb_confirmed = Confirmed,
        deb_conf = Conf,
        deb_miss = Miss,
        cw_confirmed = CwOn,
        cw_arm = CwArm,
        cw_miss = CwMiss
    },
    {State1, resolve_published(Confirmed, Conf, CwOn, maps:get(scores, Raw))}.

%% Debounce the cluster_wide verdict with the same confirm/clear thresholds used
%% for the suspect flag. Returns {NowOn, Arm, Miss}: cluster_wide is asserted
%% after confirm_ticks consecutive cluster_wide raw verdicts and held until
%% clear_ticks consecutive non-cluster_wide ones.
%%
%% This is a plain boolean-signal debouncer, kept as its own function rather
%% than shared with the suspect debouncer in debounce/2. That one is a
%% node-tracking machine (it carries node identity, confidence, and a per-stream
%% re-arm); collapsing the two into one helper would obscure both rather than
%% simplify either.
-spec debounce_cluster_wide(aws_node_health:verdict(), #state{}, pos_integer(), pos_integer()) ->
    {boolean(), non_neg_integer(), non_neg_integer()}.
debounce_cluster_wide(RawVerdict, State, ConfirmTicks, ClearTicks) ->
    IsCw = RawVerdict =:= cluster_wide,
    Arm =
        case IsCw of
            true -> State#state.cw_arm + 1;
            false -> 0
        end,
    Miss =
        case IsCw of
            true -> 0;
            false -> State#state.cw_miss + 1
        end,
    On =
        case State#state.cw_confirmed of
            false -> IsCw andalso Arm >= ConfirmTicks;
            true -> not (Miss >= ClearTicks)
        end,
    {On, Arm, Miss}.

%% Confidence the raw result assigned to node N (0.0 if absent).
-spec raw_conf(aws_node_health:result(), node()) -> float().
raw_conf(Raw, N) ->
    case maps:get(N, maps:get(scores, Raw), undefined) of
        #{confidence := C} -> C;
        _ -> 0.0
    end.

%% Resolve the published result from the debounced signals, so the verdict is
%% decided in exactly one place. A confirmed single-node suspect takes
%% precedence over cluster_wide (a dominant fault is the more actionable signal,
%% and the scorer already evaluates the dominant-node paths before the symmetric
%% guard), but only when it still has a score to attribute. A suspect that has
%% dropped out of the raw scores (its row went stale, e.g. it crashed or fully
%% partitioned) cannot be given a consistent suspected=1 sample, so it falls
%% back to the other held signal -- cluster_wide when that is confirmed, else
%% clean -- rather than masking a real, confirmed congestion as clean. Only the
%% confirmed node ever reads suspected=1 (with the held confidence); every other
%% node reads 0, and raw `inbound` scores are preserved. verdict and scores
%% therefore never contradict.
-spec resolve_published(node() | none, float(), boolean(), map()) -> aws_node_health:result().
resolve_published(none, _Conf, CwOn, Scores) ->
    #{verdict => non_suspect_verdict(CwOn), scores => zero_suspect(Scores)};
resolve_published(Confirmed, Conf, CwOn, Scores) ->
    case maps:is_key(Confirmed, Scores) of
        true ->
            Scored = maps:map(
                fun
                    (N, S) when N =:= Confirmed -> S#{suspected => 1, confidence => Conf};
                    (_N, S) -> S#{suspected => 0, confidence => 0.0}
                end,
                Scores
            ),
            #{verdict => {suspect, Confirmed}, scores => Scored};
        false ->
            #{verdict => non_suspect_verdict(CwOn), scores => zero_suspect(Scores)}
    end.

%% The verdict when no single-node suspect is published: the held cluster_wide
%% signal if it is confirmed, otherwise clean.
-spec non_suspect_verdict(boolean()) -> clean | cluster_wide.
non_suspect_verdict(true) -> cluster_wide;
non_suspect_verdict(false) -> clean.

-spec zero_suspect(map()) -> map().
zero_suspect(Scores) ->
    maps:map(fun(_N, S) -> S#{suspected => 0, confidence => 0.0} end, Scores).

gossip(PeersFun, Self, Row) ->
    %% PeersFun typically reads the cluster metadata store
    %% (rabbit_nodes:list_members). During metadata-store instability -- the
    %% exact condition this detector targets -- that read can raise; a
    %% propagated exception here would crash the worker and, under the shared
    %% supervisor intensity, could take down sibling features. Fall back to an
    %% empty peer list on exception: a missed gossip round is recoverable, a
    %% crash-loop is not.
    Peers =
        try
            PeersFun()
        catch
            Class:Reason ->
                ?AWS_LOG_WARNING(
                    "node_health: peers_fun raised ~p:~p; gossiping to no peers this tick",
                    [Class, Reason]
                ),
                []
        end,
    lists:foreach(
        fun(Peer) ->
            gen_server:cast({?MODULE, Peer}, {peer_row, Self, Row})
        end,
        %% Exclude Self defensively: gossip and sampling use the configured
        %% self_node, so never cast our own row back to ourselves even if the
        %% peer list (which excludes node()) disagrees with self_node.
        [Peer || Peer <- Peers, Peer =/= Self]
    ).

schedule_tick(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), tick).

%%--------------------------------------------------------------------
%% Internal: pure helpers (unit-tested)
%%--------------------------------------------------------------------

%% Store the latest row seen from an observer, stamped with the current tick so
%% stale rows can later be evicted.
-spec record_row(rows(), node(), view(), integer()) -> rows().
record_row(Rows, Observer, Row, Tick) ->
    Rows#{Observer => {Tick, Row}}.

%% Drop the record-tick from each (already-pruned) row, yielding the
%% observer -> view snapshot the scorer consumes. A crashed or partitioned peer
%% has already fallen out via prune_stale_rows/3, so the surviving observers
%% carry the signal about it.
-spec strip_ticks(rows()) -> snapshot().
strip_ticks(Rows) ->
    maps:map(fun(_Observer, {_RowTick, Row}) -> Row end, Rows).

%% Return the rows map with stale entries removed. On AWS, node names change
%% with every instance replacement (the ip-A-B-C-D hostname is derived from the
%% private IP), so the persistent state.rows map would otherwise accumulate a
%% row for every distinct node name ever observed over the broker's lifetime.
%% assemble_snapshot/3 only filters into a transient per-tick map; use this to
%% write the same filter back through to state.
-spec prune_stale_rows(rows(), integer(), non_neg_integer()) -> rows().
prune_stale_rows(Rows, Tick, StaleTicks) ->
    maps:filter(
        fun(_Observer, {RowTick, _Row}) -> Tick - RowTick =< StaleTicks end,
        Rows
    ).

%% Prepend the newest snapshot and keep at most Max (most recent first).
-spec push_window([snapshot()], snapshot(), pos_integer()) -> [snapshot()].
push_window(Window, Snapshot, Max) ->
    lists:sublist([Snapshot | Window], Max).

%% A well-formed row is a map of node() => probability in [0.0, 1.0]. is_map/1
%% alone (the handle_cast/2 guard) vets neither keys nor values, so a
%% version-skewed or buggy peer could otherwise cast a row with a non-atom key
%% (injecting a phantom node into all_nodes/1 and the scores) or a value outside
%% [0,1] (breaching the score contract and able to inflate a median past the
%% extreme/elevated thresholds), besides a non-number that would crash the pure
%% scorer inside median/1. Reject the whole row if any entry is malformed.
%% Callers must have already established that Row is a map (the handle_cast
%% guard does).
-spec valid_row(view()) -> boolean().
valid_row(Row) ->
    maps:fold(
        fun(K, V, Acc) -> Acc andalso is_atom(K) andalso is_probability(V) end,
        true,
        Row
    ).

-spec is_probability(term()) -> boolean().
is_probability(V) ->
    is_number(V) andalso V >= 0.0 andalso V =< 1.0.
