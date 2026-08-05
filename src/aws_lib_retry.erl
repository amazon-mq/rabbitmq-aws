%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

%% Standalone retry primitive extracted from aws_lib (issue #85). The loop owns
%% attempt sequencing, the inter-attempt sleep, exhaustion, and short-circuit;
%% the caller supplies the attempt closure and threading context.
%%
%% wait_time_ms is either a fixed non_neg_integer() or a
%% fun(AttemptNumber, Ctx) -> non_neg_integer(), so a caller can plug in a
%% backoff strategy (aws_lib passes exponential-backoff-with-jitter, issue #81)
%% without this module knowing the policy. The sleep is skipped before the final
%% attempt's exhaustion so an exhausted request does not pay a trailing backoff
%% interval that buys no further attempt (issue #81 review).
-module(aws_lib_retry).

%% The include registers this module in the erlang.mk dependency graph
%% (aws.d) so it is compiled alongside the other aws_lib_* modules.
-include("aws_lib.hrl").

-export([with_retries/3]).

-export_type([retry_ctx/0, attempt_result/0, attempt_fun/0, retry_opts/0, wait_time_ms/0]).

-type retry_ctx() :: term().

-type attempt_result() ::
    {ok, term(), retry_ctx()}
    | {retry, term(), retry_ctx()}
    | {stop, term(), retry_ctx()}.

-type attempt_fun() :: fun((retry_ctx()) -> attempt_result()).

-type wait_time_ms() ::
    non_neg_integer() | fun((non_neg_integer(), retry_ctx()) -> non_neg_integer()).

-type retry_opts() :: #{
    max_retries := non_neg_integer(),
    wait_time_ms := wait_time_ms(),
    on_retry => fun((non_neg_integer(), term(), retry_ctx()) -> ok),
    on_exhausted => fun((non_neg_integer(), term(), retry_ctx()) -> term())
}.

-spec with_retries(attempt_fun(), retry_opts(), retry_ctx()) ->
    {ok, term(), retry_ctx()} | {error, term(), retry_ctx()}.
%% @doc Execute AttemptFun in a retry loop, threading Ctx across attempts.
%%
%% max_retries is the total number of attempts allowed (including the first).
%% With max_retries=5 the function is called up to 5 times. With max_retries=0
%% no attempt is made and the loop returns immediately with an error.
%%
%% AttemptFun receives the current context and returns one of:
%%   {ok, Result, Ctx1}    -- success; loop stops and returns {ok, Result, Ctx1}
%%   {retry, Error, Ctx1}  -- retriable failure; sleep and try again
%%   {stop, Error, Ctx1}   -- non-retriable failure; loop stops immediately
%%
%% On exhaustion (max_retries attempts consumed), the on_exhausted callback
%% (if provided) transforms the last error into the final error term; otherwise
%% the last error is returned as-is.
%%
%% The on_retry callback (if provided) is invoked after each retriable failure
%% (before sleeping) with (AttemptNumber, Error, Ctx) for logging or metrics.
%% @end
with_retries(AttemptFun, Opts, Ctx) ->
    MaxRetries = maps:get(max_retries, Opts),
    loop(AttemptFun, Opts, Ctx, MaxRetries, undefined).

-spec loop(attempt_fun(), retry_opts(), retry_ctx(), non_neg_integer(), term()) ->
    {ok, term(), retry_ctx()} | {error, term(), retry_ctx()}.
%% Base case: no attempts remaining. The on_exhausted callback (if provided)
%% transforms the last error into the final error term.
loop(_AttemptFun, Opts, Ctx, 0, LastError) ->
    FinalError =
        case maps:find(on_exhausted, Opts) of
            {ok, OnExhausted} ->
                MaxRetries = maps:get(max_retries, Opts),
                OnExhausted(MaxRetries, LastError, Ctx);
            error ->
                LastError
        end,
    {error, FinalError, Ctx};
loop(AttemptFun, Opts, Ctx, AttemptsLeft, _LastError) ->
    case AttemptFun(Ctx) of
        {ok, Result, Ctx1} ->
            {ok, Result, Ctx1};
        {stop, Error, Ctx1} ->
            {error, Error, Ctx1};
        {retry, Error, Ctx1} ->
            %% Invoke the on_retry callback (for logging/metrics) then sleep.
            MaxRetries = maps:get(max_retries, Opts),
            AttemptNumber = MaxRetries - AttemptsLeft + 1,
            case maps:find(on_retry, Opts) of
                {ok, OnRetry} ->
                    OnRetry(AttemptNumber, Error, Ctx1);
                error ->
                    ok
            end,
            %% Sleep only when another attempt will follow. AttemptsLeft =< 1
            %% means the next recursion hits the exhaustion clause, so sleeping
            %% here would delay the returned error by a full backoff interval
            %% without buying another attempt (issue #81 review). This matters
            %% on the broker-boot ARN resolution path, where it halves the
            %% worst-case delay for an unreachable service.
            case AttemptsLeft =< 1 of
                true ->
                    ok;
                false ->
                    WaitTimeMs = compute_wait(
                        maps:get(wait_time_ms, Opts), AttemptNumber, Ctx1
                    ),
                    timer:sleep(WaitTimeMs)
            end,
            loop(AttemptFun, Opts, Ctx1, AttemptsLeft - 1, Error)
    end.

%% Resolve the configured wait to a concrete millisecond value: a fixed integer
%% passes through; a fun is called with the attempt ordinal and context so a
%% caller can compute an attempt-dependent backoff (issue #81).
-spec compute_wait(wait_time_ms(), non_neg_integer(), retry_ctx()) -> non_neg_integer().
compute_wait(Ms, _AttemptNumber, _Ctx) when is_integer(Ms) -> Ms;
compute_wait(Fun, AttemptNumber, Ctx) when is_function(Fun, 2) -> Fun(AttemptNumber, Ctx).
