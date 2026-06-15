%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Counting semaphore that bounds concurrent outbound auth-validation
%% requests. Holders are monitored so a crashed handler frees its slot
%% automatically.
-module(aws_auth_validate_semaphore).

-behaviour(gen_server).

-export([start_link/1, acquire/0, release/1, current/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(sem_state, {
    max :: pos_integer(),
    current = 0 :: non_neg_integer(),
    holders = #{} :: #{reference() => pid()}
}).

-type config() :: #{max => pos_integer()}.

-define(DEFAULT_MAX, 5).

-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec acquire() -> {ok, reference()} | {error, full}.
acquire() ->
    gen_server:call(?MODULE, acquire).

-spec release(reference()) -> ok.
release(Ref) ->
    gen_server:call(?MODULE, {release, Ref}).

-spec current() -> non_neg_integer().
current() ->
    gen_server:call(?MODULE, get_current).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Config) when is_map(Config) ->
    {ok, #sem_state{max = maps:get(max, Config, ?DEFAULT_MAX)}}.

handle_call(acquire, _From, #sem_state{max = Max, current = Current} = State) when
    Current >= Max
->
    {reply, {error, full}, State};
handle_call(
    acquire,
    {Pid, _Tag},
    #sem_state{
        current = Current,
        holders = Holders0
    } = State
) ->
    Ref = erlang:monitor(process, Pid),
    Holders1 = maps:put(Ref, Pid, Holders0),
    {reply, {ok, Ref}, State#sem_state{
        current = Current + 1,
        holders = Holders1
    }};
handle_call(
    {release, Ref},
    _From,
    #sem_state{
        current = Current,
        holders = Holders0
    } = State
) ->
    case maps:take(Ref, Holders0) of
        {_Pid, Holders1} ->
            erlang:demonitor(Ref, [flush]),
            {reply, ok, State#sem_state{
                current = Current - 1,
                holders = Holders1
            }};
        error ->
            %% Already released (e.g. via DOWN). Treat idempotently.
            {reply, ok, State}
    end;
handle_call(get_current, _From, #sem_state{current = Current} = State) ->
    {reply, Current, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {'DOWN', Ref, process, _Pid, _Reason},
    #sem_state{
        current = Current,
        holders = Holders0
    } = State
) ->
    case maps:take(Ref, Holders0) of
        {_Holder, Holders1} ->
            {noreply, State#sem_state{
                current = Current - 1,
                holders = Holders1
            }};
        error ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
