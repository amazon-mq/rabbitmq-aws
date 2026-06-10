%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the auth-validation subsystem's standalone workers: the
%% per-IP rate limiter and the concurrency semaphore. Tests for the
%% registry, LDAP backend, and HTTP pipeline are added alongside those
%% modules in later changes.
-module(aws_auth_validate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(IP1, {10, 0, 0, 1}).
-define(IP2, {10, 0, 0, 2}).

%%--------------------------------------------------------------------
%% Rate limiter
%%--------------------------------------------------------------------

rate_limiter_test_() ->
    {foreach,
        fun() ->
            {ok, Pid} = aws_auth_validate_rate_limiter:start_link(#{
                window_ms => 60_000,
                max_per_window => 3,
                sweep_interval_ms => 60_000
            }),
            Pid
        end,
        fun stop/1, [
            {"allows up to max then rejects", fun() ->
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1)),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1)),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1)),
                ?assertEqual(
                    {error, rate_limited},
                    aws_auth_validate_rate_limiter:check(?IP1)
                )
            end},
            {"per-IP isolation", fun() ->
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ?assertEqual(
                    {error, rate_limited},
                    aws_auth_validate_rate_limiter:check(?IP1)
                ),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP2))
            end},
            {"reset clears counters", fun() ->
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ok = aws_auth_validate_rate_limiter:check(?IP1),
                ?assertEqual(
                    {error, rate_limited},
                    aws_auth_validate_rate_limiter:check(?IP1)
                ),
                ok = aws_auth_validate_rate_limiter:reset(),
                ?assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1))
            end}
        ]}.

rate_limiter_window_expiry_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_rate_limiter:start_link(#{
                window_ms => 50,
                max_per_window => 1,
                sweep_interval_ms => 60_000
            }),
            Pid
        end,
        fun stop/1, fun(_) ->
            ok = aws_auth_validate_rate_limiter:check(?IP1),
            ?assertEqual(
                {error, rate_limited},
                aws_auth_validate_rate_limiter:check(?IP1)
            ),
            timer:sleep(80),
            [?_assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1))]
        end}.

%% The periodic sweep is the rate limiter's memory-safety mechanism: it
%% evicts counters whose window has fully expired so per-IP state cannot
%% grow without bound for IPs that are seen once and never again.
%%
%% This must be proven by observing the internal counter map shrink, NOT
%% by re-checking an IP: a fresh check would reset an expired window via
%% the on-check expiry path in handle_call/3, which would pass whether or
%% not the sweep ever ran. We register two IPs, then wait (with no further
%% checks) for their windows to lapse and a sweep to fire, and assert the
%% counter map is emptied by the sweep alone.
rate_limiter_sweep_eviction_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_rate_limiter:start_link(#{
                window_ms => 40,
                max_per_window => 5,
                sweep_interval_ms => 30
            }),
            Pid
        end,
        fun stop/1, fun(Pid) ->
            ok = aws_auth_validate_rate_limiter:check(?IP1),
            ok = aws_auth_validate_rate_limiter:check(?IP2),
            ?assertEqual(2, counter_count(Pid)),
            %% Wait for both windows to lapse (40ms) and at least one sweep
            %% (every 30ms) to fire. No checks in between, so only the
            %% sweep can remove these entries.
            timer:sleep(120),
            [?_assertEqual(0, counter_count(Pid))]
        end}.

%% Number of per-IP counters currently held by the rate limiter. Read from
%% the gen_server's internal state: the counters field is the sole map in
%% the #rate_state{} record, so we locate it by type rather than by tuple
%% position (robust to field reordering).
counter_count(Pid) ->
    State = sys:get_state(Pid),
    [Counters] = [E || E <- tuple_to_list(State), is_map(E)],
    maps:size(Counters).

%%--------------------------------------------------------------------
%% Semaphore
%%--------------------------------------------------------------------

semaphore_test_() ->
    {foreach,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 2}),
            Pid
        end,
        fun stop/1, [
            {"acquire/release sequence", fun() ->
                {ok, R1} = aws_auth_validate_semaphore:acquire(),
                {ok, R2} = aws_auth_validate_semaphore:acquire(),
                ?assertEqual({error, full}, aws_auth_validate_semaphore:acquire()),
                ok = aws_auth_validate_semaphore:release(R1),
                {ok, R3} = aws_auth_validate_semaphore:acquire(),
                ok = aws_auth_validate_semaphore:release(R2),
                ok = aws_auth_validate_semaphore:release(R3),
                ?assertEqual(0, aws_auth_validate_semaphore:current())
            end}
        ]}.

semaphore_crashed_holder_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 1}),
            Pid
        end,
        fun stop/1, fun(_) ->
            Self = self(),
            Worker = spawn(fun() ->
                {ok, _Ref} = aws_auth_validate_semaphore:acquire(),
                Self ! acquired,
                receive
                    die -> exit(boom)
                end
            end),
            receive
                acquired -> ok
            after 1_000 -> ?assert(false)
            end,
            ?assertEqual({error, full}, aws_auth_validate_semaphore:acquire()),
            Worker ! die,
            wait_until_zero(50),
            [?_assertMatch({ok, _}, aws_auth_validate_semaphore:acquire())]
        end}.

%% Correctness property under real parallelism: with many more contending
%% processes than slots, the semaphore must never hand out more than `max`
%% slots at once, and once everyone is done `current` must return to 0.
%% Each worker acquires, briefly holds (recording the peak concurrency
%% seen across all holders via a shared counter), then releases.
semaphore_concurrent_cap_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 3}),
            Pid
        end,
        fun stop/1, fun(_) ->
            Max = 3,
            Workers = 30,
            Self = self(),
            %% Tracks how many slots are held concurrently and the peak.
            Tracker = spawn_link(fun() -> tracker_loop(0, 0, Self) end),
            Pids = [
                spawn(fun() -> contend(Tracker, Self) end)
             || _ <- lists:seq(1, Workers)
            ],
            %% Wait for every worker to finish a full acquire/hold/release.
            [
                receive
                    {done, P} -> ok
                after 5_000 -> ?assert(false)
                end
             || P <- Pids
            ],
            Tracker ! {peak, self()},
            Peak =
                receive
                    {peak_value, V} -> V
                after 1_000 -> -1
                end,
            [
                ?_assert(Peak =< Max),
                ?_assert(Peak >= 1),
                ?_assertEqual(0, settle_to_zero(100))
            ]
        end}.

%% One contending worker: try to acquire (retrying on full), hold briefly
%% while bumping the live-holder count, then release and report back.
contend(Tracker, Owner) ->
    case aws_auth_validate_semaphore:acquire() of
        {ok, Ref} ->
            Tracker ! inc,
            timer:sleep(5),
            Tracker ! dec,
            ok = aws_auth_validate_semaphore:release(Ref),
            Owner ! {done, self()};
        {error, full} ->
            timer:sleep(2),
            contend(Tracker, Owner)
    end.

%% Shared peak-concurrency tracker. Holds the current live count and the
%% maximum ever observed; replies to {peak, From} with the max.
tracker_loop(Cur, Peak, Owner) ->
    receive
        inc ->
            Cur1 = Cur + 1,
            tracker_loop(Cur1, max(Cur1, Peak), Owner);
        dec ->
            tracker_loop(Cur - 1, Peak, Owner);
        {peak, From} ->
            From ! {peak_value, Peak},
            tracker_loop(Cur, Peak, Owner)
    end.

%% Releasing the same ref twice (or after the holder is already gone) must
%% be idempotent and must never drive `current` below zero or corrupt the
%% holder set. Implementation treats an unknown ref as a no-op.
semaphore_idempotent_release_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 2}),
            Pid
        end,
        fun stop/1, fun(_) ->
            {ok, Ref} = aws_auth_validate_semaphore:acquire(),
            ?assertEqual(1, aws_auth_validate_semaphore:current()),
            ok = aws_auth_validate_semaphore:release(Ref),
            ?assertEqual(0, aws_auth_validate_semaphore:current()),
            %% Double release: must stay at 0, not go negative.
            ok = aws_auth_validate_semaphore:release(Ref),
            %% A bogus ref the semaphore never issued is also a no-op.
            ok = aws_auth_validate_semaphore:release(make_ref()),
            [
                ?_assertEqual(0, aws_auth_validate_semaphore:current()),
                %% Capacity is intact: both slots are still acquirable.
                ?_assertMatch({ok, _}, aws_auth_validate_semaphore:acquire()),
                ?_assertMatch({ok, _}, aws_auth_validate_semaphore:acquire())
            ]
        end}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Poll until the semaphore's current count reaches 0, returning it.
settle_to_zero(0) ->
    aws_auth_validate_semaphore:current();
settle_to_zero(N) ->
    case aws_auth_validate_semaphore:current() of
        0 ->
            0;
        _ ->
            timer:sleep(10),
            settle_to_zero(N - 1)
    end.

stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    timer:sleep(10),
    ok.

wait_until_zero(0) ->
    ?assertEqual(0, aws_auth_validate_semaphore:current());
wait_until_zero(N) ->
    case aws_auth_validate_semaphore:current() of
        0 ->
            ok;
        _ ->
            timer:sleep(10),
            wait_until_zero(N - 1)
    end.
