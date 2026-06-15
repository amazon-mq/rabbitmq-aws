%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Per-source-IP fixed-window rate limiter. A periodic sweep evicts
%% stale entries whose window has fully expired, bounding state size.
-module(aws_auth_validate_rate_limiter).

-behaviour(gen_server).

-export([start_link/1, check/1, reset/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(rate_state, {
    counters = #{} :: #{inet:ip_address() => {non_neg_integer(), integer()}},
    window_ms :: pos_integer(),
    max_per_window :: pos_integer(),
    sweep_interval_ms :: pos_integer()
}).

-type config() :: #{
    window_ms => pos_integer(),
    max_per_window => pos_integer(),
    sweep_interval_ms => pos_integer()
}.

-define(DEFAULT_WINDOW_MS, 60_000).
-define(DEFAULT_MAX_PER_WINDOW, 10).
-define(DEFAULT_SWEEP_INTERVAL_MS, 30_000).

-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec check(inet:ip_address()) -> ok | {error, rate_limited}.
check(IP) ->
    gen_server:call(?MODULE, {check, IP}).

-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, reset).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Config) when is_map(Config) ->
    State = #rate_state{
        window_ms = maps:get(window_ms, Config, ?DEFAULT_WINDOW_MS),
        max_per_window = maps:get(max_per_window, Config, ?DEFAULT_MAX_PER_WINDOW),
        sweep_interval_ms = maps:get(sweep_interval_ms, Config, ?DEFAULT_SWEEP_INTERVAL_MS)
    },
    schedule_sweep(State#rate_state.sweep_interval_ms),
    {ok, State}.

handle_call(
    {check, IP},
    _From,
    #rate_state{
        counters = Counters0,
        window_ms = WindowMs,
        max_per_window = Max
    } = State
) ->
    Now = erlang:monotonic_time(millisecond),
    case maps:get(IP, Counters0, undefined) of
        undefined ->
            {reply, ok, State#rate_state{counters = maps:put(IP, {1, Now}, Counters0)}};
        {_Count, WindowStart} when (Now - WindowStart) >= WindowMs ->
            {reply, ok, State#rate_state{counters = maps:put(IP, {1, Now}, Counters0)}};
        {Count, _WindowStart} when Count >= Max ->
            {reply, {error, rate_limited}, State};
        {Count, WindowStart} ->
            Counters1 = maps:put(IP, {Count + 1, WindowStart}, Counters0),
            {reply, ok, State#rate_state{counters = Counters1}}
    end;
handle_call(reset, _From, State) ->
    {reply, ok, State#rate_state{counters = #{}}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    sweep,
    #rate_state{
        counters = Counters0,
        window_ms = WindowMs,
        sweep_interval_ms = SweepMs
    } = State
) ->
    Now = erlang:monotonic_time(millisecond),
    Counters1 = maps:filter(
        fun(_IP, {_Count, WindowStart}) -> (Now - WindowStart) < WindowMs end,
        Counters0
    ),
    schedule_sweep(SweepMs),
    {noreply, State#rate_state{counters = Counters1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

schedule_sweep(SweepMs) ->
    erlang:send_after(SweepMs, self(), sweep).
