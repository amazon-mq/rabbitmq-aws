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
        fun stop/1,
        [
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
        fun stop/1,
        fun(_) ->
            ok = aws_auth_validate_rate_limiter:check(?IP1),
            ?assertEqual(
                {error, rate_limited},
                aws_auth_validate_rate_limiter:check(?IP1)
            ),
            timer:sleep(80),
            [?_assertEqual(ok, aws_auth_validate_rate_limiter:check(?IP1))]
        end}.

%%--------------------------------------------------------------------
%% Semaphore
%%--------------------------------------------------------------------

semaphore_test_() ->
    {foreach,
        fun() ->
            {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 2}),
            Pid
        end,
        fun stop/1,
        [
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
        fun stop/1,
        fun(_) ->
            Self = self(),
            Worker = spawn(fun() ->
                {ok, _Ref} = aws_auth_validate_semaphore:acquire(),
                Self ! acquired,
                receive
                    die -> exit(boom)
                end
            end),
            receive acquired -> ok after 1_000 -> ?assert(false) end,
            ?assertEqual({error, full}, aws_auth_validate_semaphore:acquire()),
            Worker ! die,
            wait_until_zero(50),
            [?_assertMatch({ok, _}, aws_auth_validate_semaphore:acquire())]
        end}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    timer:sleep(10),
    ok.

wait_until_zero(0) ->
    ?assertEqual(0, aws_auth_validate_semaphore:current());
wait_until_zero(N) ->
    case aws_auth_validate_semaphore:current() of
        0 -> ok;
        _ -> timer:sleep(10), wait_until_zero(N - 1)
    end.
