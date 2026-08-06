%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Prometheus metrics collector for the auth-validation endpoint.
%%
%% Implements the `prometheus_collector` behaviour (pull-based): Prometheus
%% scrapes /api/metrics, which invokes collect_mf/2 on every registered
%% collector. No outbound calls are made on the request hot path -- observe/3
%% just increments lock-free OTP `counters` stored in `persistent_term`.
%%
%% Counter layout (single counters ref, write_concurrency):
%%   Slots 1..NUM_REQUESTS_SLOTS       -- requests_total per (method, category)
%%   Slots HIST_BASE+1..HIST_BASE+NUM  -- histogram per method (buckets+sum+count)
%%
%% Semaphore gauges are read live from the semaphore gen_server at scrape time,
%% not stored in counters.
-module(aws_auth_validate_metrics).

-behaviour(prometheus_collector).

-export([register/0, deregister/0, observe/3]).
-export([deregister_cleanup/1, collect_mf/2]).

-ifdef(TEST).
-export([counter_ref/0, method_index/1, category_index/1, histogram_base/0]).
-endif.

-import(prometheus_model_helpers, [create_mf/4]).

-include("aws.hrl").

%%--------------------------------------------------------------------
%% Constants
%%--------------------------------------------------------------------

%% Histogram bucket boundaries (milliseconds). Chosen to cover the range from
%% fast local validation (~10ms) to slow DNS/TLS negotiation (~10s timeout).
-define(HISTOGRAM_BUCKETS, [10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]).
-define(NUM_BUCKETS, 10).

%% Per-method histogram layout: 10 bucket counters + 1 sum + 1 count = 12 slots.
-define(SLOTS_PER_HIST, 12).

%% Methods -- indexed 0..3 for counter math.
-define(METHODS, [<<"ldap">>, <<"http">>, <<"oauth">>, <<"tls">>]).
-define(NUM_METHODS, 4).

%% Result categories -- indexed 0..14. This is the full set of categories
%% that can appear in the audit trail (see aws_auth_validate_mgmt).
-define(CATEGORIES, [
    success,
    input_invalid,
    body_too_large,
    connection_failed,
    tls_failed,
    query_invalid,
    auth_failed,
    config_conflict,
    authz_unverified,
    token_expired,
    token_invalid,
    capacity_exhausted,
    unknown_method,
    method_disabled,
    internal_error
]).
-define(NUM_CATEGORIES, 15).

%% Total counter slots:
%%   requests_total: NUM_METHODS * NUM_CATEGORIES = 4 * 15 = 60
%%   histogram: NUM_METHODS * SLOTS_PER_HIST = 4 * 12 = 48
%%   Grand total: 108
-define(NUM_REQUESTS_SLOTS, (?NUM_METHODS * ?NUM_CATEGORIES)).
-define(HIST_BASE, ?NUM_REQUESTS_SLOTS).
-define(TOTAL_SLOTS, (?NUM_REQUESTS_SLOTS + ?NUM_METHODS * ?SLOTS_PER_HIST)).

%% persistent_term keys
-define(PT_COUNTERS, {?MODULE, counters}).

%%--------------------------------------------------------------------
%% Registration
%%--------------------------------------------------------------------

-spec register() -> ok.
register() ->
    Counters = counters:new(?TOTAL_SLOTS, [write_concurrency]),
    persistent_term:put(?PT_COUNTERS, Counters),
    prometheus_registry:register_collector(?MODULE),
    ?AWS_LOG_INFO("auth_validate metrics: registered collector"),
    ok.

-spec deregister() -> ok.
deregister() ->
    prometheus_registry:deregister_collector(?MODULE),
    persistent_term:erase(?PT_COUNTERS),
    ?AWS_LOG_INFO("auth_validate metrics: deregistered collector"),
    ok.

%%--------------------------------------------------------------------
%% Hot-path observation (called after every audit)
%%--------------------------------------------------------------------

%% @doc Record a completed validation request.
%%
%% Called after each audit call in aws_auth_validate_mgmt. If metrics are not
%% registered (feature disabled, or deregistered during shutdown), this is a
%% silent no-op -- it never crashes the caller.
-spec observe(binary(), atom(), non_neg_integer()) -> ok.
observe(Method, Category, DurationMs) ->
    case persistent_term:get(?PT_COUNTERS, undefined) of
        undefined ->
            %% Metrics not registered -- silently skip.
            ok;
        Counters ->
            observe_request(Counters, Method, Category),
            observe_duration(Counters, Method, DurationMs),
            ok
    end.

%%--------------------------------------------------------------------
%% prometheus_collector callbacks
%%--------------------------------------------------------------------

deregister_cleanup(_Registry) ->
    ok.

%% @doc Called by the Prometheus registry on each scrape. Reads counters and
%% the semaphore state, then emits metric families via Callback.
collect_mf(_Registry, Callback) ->
    case persistent_term:get(?PT_COUNTERS, undefined) of
        undefined ->
            ok;
        Counters ->
            emit_requests_total(Counters, Callback),
            emit_duration_histogram(Counters, Callback),
            emit_capacity_exhausted_total(Counters, Callback),
            emit_semaphore_gauges(Callback),
            ok
    end.

%%--------------------------------------------------------------------
%% Internal: observation helpers
%%--------------------------------------------------------------------

observe_request(Counters, Method, Category) ->
    case request_slot(Method, Category) of
        undefined -> ok;
        Slot -> counters:add(Counters, Slot, 1)
    end.

observe_duration(Counters, Method, DurationMs) ->
    case method_index(Method) of
        undefined ->
            ok;
        MethodIdx ->
            BucketPos = find_bucket_pos(DurationMs),
            %% Increment the bucket counter (1-based within the method's histogram range).
            BucketSlot = ?HIST_BASE + MethodIdx * ?SLOTS_PER_HIST + BucketPos,
            counters:add(Counters, BucketSlot, 1),
            %% Increment the sum slot (bucket positions are 1..NUM_BUCKETS, sum is NUM_BUCKETS+1).
            SumSlot = ?HIST_BASE + MethodIdx * ?SLOTS_PER_HIST + ?NUM_BUCKETS + 1,
            counters:add(Counters, SumSlot, DurationMs),
            %% Increment the count slot (NUM_BUCKETS+2).
            CountSlot = ?HIST_BASE + MethodIdx * ?SLOTS_PER_HIST + ?NUM_BUCKETS + 2,
            counters:add(Counters, CountSlot, 1)
    end.

%%--------------------------------------------------------------------
%% Internal: emission helpers (called from collect_mf/2)
%%--------------------------------------------------------------------

emit_requests_total(Counters, Callback) ->
    Metrics = lists:foldl(
        fun(Method, Acc) ->
            MethodIdx = method_index(Method),
            lists:foldl(
                fun(Category, InnerAcc) ->
                    CatIdx = category_index(Category),
                    Slot = MethodIdx * ?NUM_CATEGORIES + CatIdx + 1,
                    Value = counters:get(Counters, Slot),
                    case Value of
                        0 -> InnerAcc;
                        _ -> [{[{method, Method}, {result, Category}], Value} | InnerAcc]
                    end
                end,
                Acc,
                ?CATEGORIES
            )
        end,
        [],
        ?METHODS
    ),
    Callback(
        create_mf(
            rabbitmq_aws_auth_validation_requests_total,
            "Total number of auth-validation requests",
            counter,
            Metrics
        )
    ).

emit_duration_histogram(Counters, Callback) ->
    Metrics = lists:filtermap(
        fun(Method) ->
            MethodIdx = method_index(Method),
            Base = ?HIST_BASE + MethodIdx * ?SLOTS_PER_HIST,
            Count = counters:get(Counters, Base + ?NUM_BUCKETS + 2),
            case Count of
                0 ->
                    false;
                _ ->
                    Sum = counters:get(Counters, Base + ?NUM_BUCKETS + 1),
                    %% Build cumulative bucket list: [{BoundaryValue, CumulativeCount}]
                    Buckets = build_cumulative_buckets(Counters, Base),
                    {true, {[{method, Method}], Buckets, Count, Sum}}
            end
        end,
        ?METHODS
    ),
    Callback(
        create_mf(
            rabbitmq_aws_auth_validation_duration_milliseconds,
            "Duration of auth-validation requests in milliseconds",
            histogram,
            Metrics
        )
    ).

emit_capacity_exhausted_total(Counters, Callback) ->
    %% capacity_exhausted is category index 11 (0-based). Read directly from
    %% the requests_total counter slots for each method.
    CatIdx = category_index(capacity_exhausted),
    Metrics = lists:filtermap(
        fun(Method) ->
            MethodIdx = method_index(Method),
            Slot = MethodIdx * ?NUM_CATEGORIES + CatIdx + 1,
            Value = counters:get(Counters, Slot),
            case Value of
                0 -> false;
                _ -> {true, {[{method, Method}], Value}}
            end
        end,
        ?METHODS
    ),
    Callback(
        create_mf(
            rabbitmq_aws_auth_validation_capacity_exhausted_total,
            "Total number of requests rejected due to semaphore capacity exhaustion",
            counter,
            Metrics
        )
    ).

emit_semaphore_gauges(Callback) ->
    %% Read live from the semaphore gen_server at scrape time. If the
    %% semaphore is not running (feature disabled mid-flight or crashed),
    %% emit nothing rather than crashing the scrape.
    case try_semaphore_usage() of
        {InUse, Capacity} ->
            Callback(
                create_mf(
                    rabbitmq_aws_auth_validation_semaphore_in_use,
                    "Number of semaphore slots currently held by in-flight validations",
                    gauge,
                    [{[], InUse}]
                )
            ),
            Callback(
                create_mf(
                    rabbitmq_aws_auth_validation_semaphore_capacity,
                    "Configured maximum concurrent validation slots",
                    gauge,
                    [{[], Capacity}]
                )
            );
        unavailable ->
            ok
    end.

%%--------------------------------------------------------------------
%% Internal: index math
%%--------------------------------------------------------------------

%% @doc Map a method binary to a 0-based index. Returns `undefined` for
%% unknown methods (they are not tracked in metrics).
-spec method_index(binary()) -> non_neg_integer() | undefined.
method_index(<<"ldap">>) -> 0;
method_index(<<"http">>) -> 1;
method_index(<<"oauth">>) -> 2;
method_index(<<"tls">>) -> 3;
method_index(_) -> undefined.

%% @doc Map a result category atom to a 0-based index.
-spec category_index(atom()) -> non_neg_integer() | undefined.
category_index(success) -> 0;
category_index(input_invalid) -> 1;
category_index(body_too_large) -> 2;
category_index(connection_failed) -> 3;
category_index(tls_failed) -> 4;
category_index(query_invalid) -> 5;
category_index(auth_failed) -> 6;
category_index(config_conflict) -> 7;
category_index(authz_unverified) -> 8;
category_index(token_expired) -> 9;
category_index(token_invalid) -> 10;
category_index(capacity_exhausted) -> 11;
category_index(unknown_method) -> 12;
category_index(method_disabled) -> 13;
category_index(internal_error) -> 14;
category_index(_) -> undefined.

%% @doc Compute the 1-based counter slot for a given (method, category) pair.
%% Returns `undefined` if either method or category is unknown.
request_slot(Method, Category) ->
    case {method_index(Method), category_index(Category)} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {MethodIdx, CatIdx} -> MethodIdx * ?NUM_CATEGORIES + CatIdx + 1
    end.

%% @doc Find which bucket a duration value falls into (1-based position).
%% Bucket boundaries: 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000.
%% A value of exactly the boundary goes into that bucket (less-than-or-equal).
find_bucket_pos(Ms) when Ms =< 10 -> 1;
find_bucket_pos(Ms) when Ms =< 25 -> 2;
find_bucket_pos(Ms) when Ms =< 50 -> 3;
find_bucket_pos(Ms) when Ms =< 100 -> 4;
find_bucket_pos(Ms) when Ms =< 250 -> 5;
find_bucket_pos(Ms) when Ms =< 500 -> 6;
find_bucket_pos(Ms) when Ms =< 1000 -> 7;
find_bucket_pos(Ms) when Ms =< 2500 -> 8;
find_bucket_pos(Ms) when Ms =< 5000 -> 9;
find_bucket_pos(_Ms) -> 10.

%% @doc Build cumulative bucket list from per-bucket counters.
%% Prometheus histograms require cumulative counts: each bucket's count
%% includes all observations in lower buckets.
build_cumulative_buckets(Counters, Base) ->
    Boundaries = ?HISTOGRAM_BUCKETS,
    {Buckets, _} = lists:mapfoldl(
        fun(Boundary, CumAcc) ->
            Pos = find_bucket_pos(Boundary),
            Raw = counters:get(Counters, Base + Pos),
            NewCum = CumAcc + Raw,
            {{Boundary, NewCum}, NewCum}
        end,
        0,
        Boundaries
    ),
    Buckets.

%% @doc Read current semaphore usage without crashing. Returns
%% {InUse, Capacity} or `unavailable` if the process is not running.
try_semaphore_usage() ->
    try
        aws_auth_validate_semaphore:usage()
    catch
        exit:{noproc, _} -> unavailable;
        exit:{timeout, _} -> unavailable
    end.

%% @doc Return the histogram base offset for test introspection.
-ifdef(TEST).
histogram_base() -> ?HIST_BASE.
-endif.

%% @doc Return the counters ref for test introspection (returns undefined if
%% not registered).
-ifdef(TEST).
counter_ref() -> persistent_term:get(?PT_COUNTERS, undefined).
-endif.
