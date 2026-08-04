-module(aws_lib_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% with_retries/3 tests
%%
%% max_retries is the total number of attempts allowed (including the first).
%% With max_retries=5 the closure runs up to 5 times. With max_retries=0 the
%% loop returns immediately without calling AttemptFun.
%% ============================================================================

success_on_first_attempt_test() ->
    AttemptFun = fun(Ctx) -> {ok, result_value, Ctx} end,
    Opts = #{max_retries => 3, wait_time_ms => 0},
    ?assertEqual(
        {ok, result_value, initial_ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, initial_ctx)
    ).

success_after_retries_test() ->
    %% Use a process dictionary counter to track attempts
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        Count = erlang:get(attempt_count),
        erlang:put(attempt_count, Count + 1),
        case Count of
            N when N < 2 ->
                {retry, {error, transient}, Ctx};
            _ ->
                {ok, success, Ctx}
        end
    end,
    Opts = #{max_retries => 5, wait_time_ms => 0},
    ?assertEqual(
        {ok, success, my_ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, my_ctx)
    ),
    %% 2 retries + 1 success = 3 total attempts
    ?assertEqual(3, erlang:get(attempt_count)),
    erlang:erase(attempt_count).

exhaustion_test() ->
    %% All attempts fail with retry -- should exhaust and return error.
    %% max_retries=3 means 3 attempts total.
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        erlang:put(attempt_count, erlang:get(attempt_count) + 1),
        {retry, {error, always_fails}, Ctx}
    end,
    Opts = #{max_retries => 3, wait_time_ms => 0},
    ?assertEqual(
        {error, {error, always_fails}, start_ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, start_ctx)
    ),
    %% Exactly 3 attempts were made
    ?assertEqual(3, erlang:get(attempt_count)),
    erlang:erase(attempt_count).

stop_short_circuits_test() ->
    %% A stop result returns immediately without retrying
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        Count = erlang:get(attempt_count),
        erlang:put(attempt_count, Count + 1),
        {stop, {permanent_error, not_found}, Ctx}
    end,
    Opts = #{max_retries => 5, wait_time_ms => 0},
    ?assertEqual(
        {error, {permanent_error, not_found}, ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, ctx)
    ),
    %% Only one attempt was made (no retries after stop)
    ?assertEqual(1, erlang:get(attempt_count)),
    erlang:erase(attempt_count).

context_threading_test() ->
    %% Each attempt receives the updated context from the previous attempt.
    %% Starting at 0, each attempt increments the context. At ctx >= 3 it
    %% succeeds; otherwise it retries.
    AttemptFun = fun(Ctx) ->
        NewCtx = Ctx + 1,
        case NewCtx >= 3 of
            true -> {ok, done, NewCtx};
            false -> {retry, not_ready, NewCtx}
        end
    end,
    Opts = #{max_retries => 5, wait_time_ms => 0},
    ?assertEqual(
        {ok, done, 3},
        aws_lib_retry:with_retries(AttemptFun, Opts, 0)
    ).

on_retry_callback_test() ->
    %% Verify the on_retry callback is invoked with correct arguments
    Self = self(),
    OnRetry = fun(AttemptNum, Error, Ctx) ->
        Self ! {on_retry, AttemptNum, Error, Ctx},
        ok
    end,
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        Count = erlang:get(attempt_count),
        erlang:put(attempt_count, Count + 1),
        case Count of
            2 -> {ok, done, Ctx};
            N -> {retry, {fail, N}, Ctx}
        end
    end,
    Opts = #{max_retries => 5, wait_time_ms => 0, on_retry => OnRetry},
    ?assertEqual(
        {ok, done, the_ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, the_ctx)
    ),
    %% Two retriable failures -> two on_retry callbacks
    ?assertEqual({on_retry, 1, {fail, 0}, the_ctx}, receive_msg()),
    ?assertEqual({on_retry, 2, {fail, 1}, the_ctx}, receive_msg()),
    erlang:erase(attempt_count).

on_exhausted_callback_test() ->
    %% Verify on_exhausted transforms the final error.
    %% max_retries=2 means 2 attempts; on_exhausted receives the LAST error
    %% from the final attempt (passed as LastError to the base case).
    OnExhausted = fun(TotalAttempts, LastError, _Ctx) ->
        {exhausted, TotalAttempts, LastError}
    end,
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        N = erlang:get(attempt_count),
        erlang:put(attempt_count, N + 1),
        {retry, {timeout, N}, Ctx}
    end,
    Opts = #{max_retries => 2, wait_time_ms => 0, on_exhausted => OnExhausted},
    Result = aws_lib_retry:with_retries(AttemptFun, Opts, my_ctx),
    %% on_exhausted receives the last error from the final retry (attempt 2,
    %% which is the error from the loop iteration that slept and then hit the
    %% base case; i.e., the error logged by the last on_retry call).
    ?assertEqual(
        {error, {exhausted, 2, {timeout, 1}}, my_ctx},
        Result
    ),
    erlang:erase(attempt_count).

zero_retries_test() ->
    %% With max_retries = 0, no attempt is made and the loop returns
    %% immediately with on_exhausted(undefined) or undefined.
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        erlang:put(attempt_count, erlang:get(attempt_count) + 1),
        {retry, failed, Ctx}
    end,
    Opts = #{max_retries => 0, wait_time_ms => 0},
    ?assertEqual(
        {error, undefined, ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, ctx)
    ),
    %% No attempt was made
    ?assertEqual(0, erlang:get(attempt_count)),
    erlang:erase(attempt_count).

context_preserved_on_stop_test() ->
    %% The context returned by the stopping attempt is propagated
    AttemptFun = fun(_Ctx) -> {stop, bad_request, updated_ctx} end,
    Opts = #{max_retries => 5, wait_time_ms => 0},
    ?assertEqual(
        {error, bad_request, updated_ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, original_ctx)
    ).

context_preserved_on_exhaustion_test() ->
    %% The context from the LAST retry is carried into the base case.
    %% max_retries=3: attempt(3) -> retry, ctx+1 -> sleep -> attempt(2) ->
    %% retry, ctx+1 -> sleep -> attempt(1) -> retry, ctx+1 -> sleep ->
    %% base(0) with ctx from last retry. 3 attempts, ctx goes 0->1->2->3.
    AttemptFun = fun(Ctx) ->
        {retry, err, Ctx + 1}
    end,
    Opts = #{max_retries => 3, wait_time_ms => 0},
    ?assertEqual(
        {error, err, 3},
        aws_lib_retry:with_retries(AttemptFun, Opts, 0)
    ).

single_attempt_success_test() ->
    %% max_retries=1 means exactly one attempt. If it succeeds, done.
    AttemptFun = fun(Ctx) -> {ok, yep, Ctx} end,
    Opts = #{max_retries => 1, wait_time_ms => 0},
    ?assertEqual(
        {ok, yep, ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, ctx)
    ).

single_attempt_failure_test() ->
    %% max_retries=1 means one attempt. If it retries, on_retry is called,
    %% then the base case fires.
    Self = self(),
    OnRetry = fun(AttemptNum, Error, _Ctx) ->
        Self ! {on_retry, AttemptNum, Error},
        ok
    end,
    AttemptFun = fun(Ctx) -> {retry, oops, Ctx} end,
    Opts = #{max_retries => 1, wait_time_ms => 0, on_retry => OnRetry},
    ?assertEqual(
        {error, oops, ctx},
        aws_lib_retry:with_retries(AttemptFun, Opts, ctx)
    ),
    ?assertEqual({on_retry, 1, oops}, receive_msg()).

wait_fun_test() ->
    %% wait_time_ms can be a fun/2 for custom backoff strategies.
    Self = self(),
    WaitFun = fun(AttemptNumber, _Ctx) ->
        Self ! {wait, AttemptNumber},
        0
    end,
    erlang:put(attempt_count, 0),
    AttemptFun = fun(Ctx) ->
        N = erlang:get(attempt_count),
        erlang:put(attempt_count, N + 1),
        case N of
            2 -> {ok, done, Ctx};
            _ -> {retry, failed, Ctx}
        end
    end,
    Opts = #{max_retries => 5, wait_time_ms => WaitFun},
    ?assertEqual({ok, done, ctx}, aws_lib_retry:with_retries(AttemptFun, Opts, ctx)),
    ?assertEqual({wait, 1}, receive_msg()),
    ?assertEqual({wait, 2}, receive_msg()),
    erlang:erase(attempt_count).

no_wait_before_exhaustion_test() ->
    %% The sleep is skipped before the final attempt's exhaustion: sleeping
    %% there would delay the returned error by a full backoff interval without
    %% buying another attempt (issue #81 review, now enforced by the loop).
    %% With max_retries=3 and every attempt retriable, there are 3 attempts and
    %% exactly 2 waits -- one fewer than the attempt count.
    Self = self(),
    WaitFun = fun(AttemptNumber, _Ctx) ->
        Self ! {wait, AttemptNumber},
        0
    end,
    AttemptFun = fun(Ctx) -> {retry, boom, Ctx} end,
    Opts = #{max_retries => 3, wait_time_ms => WaitFun},
    ?assertEqual({error, boom, ctx}, aws_lib_retry:with_retries(AttemptFun, Opts, ctx)),
    ?assertEqual({wait, 1}, receive_msg()),
    ?assertEqual({wait, 2}, receive_msg()),
    %% No third wait: the third attempt is the last, so it does not sleep.
    ?assertEqual(timeout, receive_msg()).

single_retry_never_waits_test() ->
    %% With max_retries=1 the only attempt is the final one, so a retriable
    %% failure exhausts immediately with no wait at all.
    Self = self(),
    WaitFun = fun(AttemptNumber, _Ctx) ->
        Self ! {wait, AttemptNumber},
        0
    end,
    AttemptFun = fun(Ctx) -> {retry, boom, Ctx} end,
    Opts = #{max_retries => 1, wait_time_ms => WaitFun},
    ?assertEqual({error, boom, ctx}, aws_lib_retry:with_retries(AttemptFun, Opts, ctx)),
    ?assertEqual(timeout, receive_msg()).

%% ============================================================================
%% Helpers
%% ============================================================================

receive_msg() ->
    receive
        Msg -> Msg
    after 100 ->
        timeout
    end.
