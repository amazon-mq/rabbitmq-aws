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

-export([start_link/0, start_link/1, latest/0, own_view/0, refresh/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Exported for unit tests of the otherwise-internal pure helpers.
-ifdef(TEST).
-export([record_row/4, assemble_snapshot/3, push_window/3, default_config/0]).
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
    latest = #{verdict => clean, scores => #{}} :: aws_node_health:result()
}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(default_config()).

-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% Latest computed verdict and per-node scores. Read by the metrics collector
%% at scrape time.
-spec latest() -> aws_node_health:result().
latest() ->
    gen_server:call(?MODULE, latest).

%% This node's own most recent view of its peers (its raw failure-detector
%% row), the value behind the per-node rabbitmq_peer_down_probability metric.
-spec own_view() -> view().
own_view() ->
    gen_server:call(?MODULE, own_view).

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
        %% cluster peers to gossip rows to
        peers_fun => fun() -> nodes() end,
        %% samples this node's per-peer down-probability view
        sample_fun => fun sample_failure_probabilities/0
    },
    maps:merge(Runtime, aws_node_health_config:worker_config()).

%% The node failure detector exposes each node's view of its peers as a map of
%% peer -> probability. Sampling must never crash the worker, so any failure
%% (detector not started, transient error) yields an empty view.
-spec sample_failure_probabilities() -> view().
sample_failure_probabilities() ->
    try
        aten_sink:get_failure_probabilities()
    catch
        Class:Reason:Stacktrace ->
            ?AWS_LOG_ERROR("failed to sample peer failure probabilities: ~tp", [
                {Class, Reason}
            ]),
            ?AWS_LOG_DEBUG("~tp", [Stacktrace]),
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
        analysis = maps:get(analysis, Config)
    },
    schedule_tick(Interval),
    {ok, State}.

handle_call(latest, _From, State) ->
    {reply, State#state.latest, State};
handle_call(own_view, _From, State) ->
    {reply, State#state.own_row, State};
handle_call(refresh, _From, State0) ->
    State = cycle(State0),
    {reply, State#state.latest, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({peer_row, From, Row}, State) when is_map(Row) ->
    Rows = record_row(State#state.rows, From, Row, State#state.tick),
    {noreply, State#state{rows = Rows}};
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
    Snapshot = assemble_snapshot(Rows0, Tick, State0#state.stale_ticks),
    Window = push_window(State0#state.window, Snapshot, State0#state.window_max),
    Latest = aws_node_health:analyze(State0#state.analysis, Window),
    State0#state{
        rows = Rows0,
        own_row = OwnRow,
        window = Window,
        tick = Tick,
        latest = Latest
    }.

gossip(PeersFun, Self, Row) ->
    lists:foreach(
        fun(Peer) ->
            gen_server:cast({?MODULE, Peer}, {peer_row, Self, Row})
        end,
        PeersFun()
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

%% Build one observer x peer snapshot from the known rows, dropping any row not
%% refreshed within StaleTicks of the current tick. A crashed or partitioned
%% peer thus falls out of the matrix rather than pinning a stale view; the
%% surviving observers still carry the signal about it.
-spec assemble_snapshot(rows(), integer(), non_neg_integer()) -> snapshot().
assemble_snapshot(Rows, Tick, StaleTicks) ->
    #{
        Observer => Row
     || Observer := {RowTick, Row} <- Rows,
        Tick - RowTick =< StaleTicks
    }.

%% Prepend the newest snapshot and keep at most Max (most recent first).
-spec push_window([snapshot()], snapshot(), pos_integer()) -> [snapshot()].
push_window(Window, Snapshot, Max) ->
    lists:sublist([Snapshot | Window], Max).
