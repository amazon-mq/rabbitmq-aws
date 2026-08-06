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
%%   Slots 1..75 (NUM_REQUESTS_SLOTS)  -- requests_total per (method, category)
%%   Slots 76..140 (HIST_BASE+1..end)  -- histogram per method
%%
%% Per-method histogram (13 slots each):
%%   Positions 1..10  -- finite bucket counters (le=10..10000)
%%   Position 11      -- overflow bucket (le="+Inf", durations > 10000ms)
%%   Position 12      -- sum of observed durations (NUM_BUCKETS+1)
%%   Position 13      -- count of observations (NUM_BUCKETS+2)
%%
%% Semaphore gauges are read live from the semaphore gen_server at scrape time,
%% not stored in counters.
-module(aws_auth_validate_metrics).

-behaviour(prometheus_collector).

-export([register/0, deregister/0, observe/3]).
-export([deregister_cleanup/1, collect_mf/2]).

-ifdef(TEST).
-export([
    counter_ref/0, method_index/1, category_index/1, histogram_base/0, build_cumulative_buckets/2
]).
-endif.

-import(prometheus_model_helpers, [create_mf/4]).

-include("aws.hrl").

%%--------------------------------------------------------------------
%% Constants
%%--------------------------------------------------------------------

%% Histogram bucket boundaries (milliseconds). Chosen to cover the range from
%% fast local validation (~10ms) to slow DNS/TLS negotiation (~10s timeout).
%% An 11th overflow bucket (slot 11) catches durations > 10000ms; emitted as
%% le="+Inf" during scrape.
-define(HISTOGRAM_BUCKETS, [10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]).
-define(NUM_BUCKETS, 11).

%% Per-method histogram layout: 11 bucket counters + 1 sum + 1 count = 13 slots.
-define(SLOTS_PER_HIST, 13).

%% Methods -- indexed 0..4 for counter math. The synthetic "unknown" method
%% (index 4) collects requests_total for unrecognized method paths without
%% exposing the raw caller-supplied string as a label (fixed cardinality).
-define(METHODS, [<<"ldap">>, <<"http">>, <<"oauth">>, <<"tls">>, <<"unknown">>]).
-define(NUM_METHODS, 5).

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
%%   requests_total: NUM_METHODS * NUM_CATEGORIES = 5 * 15 = 75
%%   histogram: NUM_METHODS * SLOTS_PER_HIST = 5 * 13 = 65
%%   Grand total: 140
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
    case persistent_term:get(?PT_COUNTERS, undefined) of
        undefined ->
            Counters = counters:new(?TOTAL_SLOTS, [write_concurrency]),
            persistent_term:put(?PT_COUNTERS, Counters);
        _Existing ->
            %% Counters already allocated -- preserve existing values.
            ok
    end,
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
%%
%% Duration is recorded ONLY for categories representing actual backend/outbound
%% work (see is_timed_category/1). Pre-connection rejects (input_invalid,
%% body_too_large, config_conflict, capacity_exhausted, method_disabled,
%% unknown_method, internal_error, query_invalid) are excluded so they do not
%% contaminate latency percentiles with near-zero noise.
-spec observe(binary(), atom(), non_neg_integer()) -> ok.
observe(Method, Category, DurationMs) ->
    case persistent_term:get(?PT_COUNTERS, undefined) of
        undefined ->
            %% Metrics not registered -- silently skip.
            ok;
        Counters ->
            observe_request(Counters, Method, Category),
            case is_timed_category(Category) of
                true -> observe_duration(Counters, Method, DurationMs);
                false -> ok
            end,
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

%% @doc Categories that represent actual backend/outbound work and should
%% contribute to the duration histogram. Categories excluded are pure
%% pre-connection rejects that complete without any network round-trip:
%%   - input_invalid, body_too_large: request-body validation
%%   - config_conflict: local config check
%%   - capacity_exhausted: semaphore full
%%   - method_disabled, unknown_method: routing reject
%%   - internal_error: unexpected crash path
%%   - query_invalid: LDAP query grammar parsing (happens inside parse_input,
%%     step 1 of validate/1, before any eldap:open)
-spec is_timed_category(atom()) -> boolean().
is_timed_category(success) -> true;
is_timed_category(auth_failed) -> true;
is_timed_category(connection_failed) -> true;
is_timed_category(tls_failed) -> true;
is_timed_category(authz_unverified) -> true;
is_timed_category(token_expired) -> true;
is_timed_category(token_invalid) -> true;
is_timed_category(_) -> false.

observe_duration(Counters, Method, DurationMs) ->
    case real_method_index(Method) of
        undefined ->
            %% Unknown methods do not record duration -- only the four real
            %% backends (ldap/http/oauth/tls) produce meaningful latency data.
            ok;
        MethodIdx ->
            BucketPos = find_bucket_pos(DurationMs),
            %% Increment the bucket counter (1-based within the method's histogram range).
            BucketSlot = ?HIST_BASE + MethodIdx * ?SLOTS_PER_HIST + BucketPos,
            counters:add(Counters, BucketSlot, 1),
            %% Increment the sum slot (NUM_BUCKETS+1 = position 12).
            SumSlot = ?HIST_BASE + MethodIdx * ?SLOTS_PER_HIST + ?NUM_BUCKETS + 1,
            counters:add(Counters, SumSlot, DurationMs),
            %% Increment the count slot (NUM_BUCKETS+2 = position 13).
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
            %% Count slot is at position NUM_BUCKETS+2 (=13) within the method's range.
            Count = counters:get(Counters, Base + ?NUM_BUCKETS + 2),
            case Count of
                0 ->
                    false;
                _ ->
                    %% Sum slot is at position NUM_BUCKETS+1 (=12).
                    Sum = counters:get(Counters, Base + ?NUM_BUCKETS + 1),
                    %% Build cumulative bucket list including le="+Inf".
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
%% methods not in the fixed label set (caller decides how to handle).
-spec method_index(binary()) -> non_neg_integer() | undefined.
method_index(<<"ldap">>) -> 0;
method_index(<<"http">>) -> 1;
method_index(<<"oauth">>) -> 2;
method_index(<<"tls">>) -> 3;
method_index(<<"unknown">>) -> 4;
method_index(_) -> undefined.

%% @doc Map a method binary to a 0-based index for the four real backends
%% only. Returns `undefined` for the synthetic "unknown" method and for
%% truly unrecognized methods -- used to gate duration recording.
-spec real_method_index(binary()) -> non_neg_integer() | undefined.
real_method_index(<<"ldap">>) -> 0;
real_method_index(<<"http">>) -> 1;
real_method_index(<<"oauth">>) -> 2;
real_method_index(<<"tls">>) -> 3;
real_method_index(_) -> undefined.

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
%% Unknown methods are mapped to the synthetic "unknown" method index so they
%% appear in requests_total. Returns `undefined` only for unknown categories.
request_slot(Method, Category) ->
    MethodIdx =
        case method_index(Method) of
            undefined -> method_index(<<"unknown">>);
            Idx -> Idx
        end,
    case category_index(Category) of
        undefined -> undefined;
        CatIdx -> MethodIdx * ?NUM_CATEGORIES + CatIdx + 1
    end.

%% @doc Find which bucket a duration value falls into (1-based position).
%% Bucket boundaries: 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000.
%% Position 11 is the overflow bucket for durations > 10000ms (emitted as
%% le="+Inf" at scrape time).
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
find_bucket_pos(Ms) when Ms =< 10000 -> 10;
find_bucket_pos(_Ms) -> 11.

%% @doc Build cumulative bucket list from per-bucket counters.
%% Prometheus histograms require cumulative counts: each bucket's count
%% includes all observations in lower buckets. The final entry is
%% {infinity, SampleCount} -- the mandatory le="+Inf" bucket.
%%
%% Slot positions are enumerated directly (1..length(HISTOGRAM_BUCKETS) for
%% finite boundaries, position 11 for overflow) rather than re-deriving them
%% through find_bucket_pos/1, so the layout is expressed once in the macros
%% and cannot drift.
build_cumulative_buckets(Counters, Base) ->
    Boundaries = ?HISTOGRAM_BUCKETS,
    Indexed = lists:zip(Boundaries, lists:seq(1, length(Boundaries))),
    {FiniteBuckets, CumAfterFinite} = lists:mapfoldl(
        fun({Boundary, Pos}, CumAcc) ->
            Raw = counters:get(Counters, Base + Pos),
            NewCum = CumAcc + Raw,
            {{Boundary, NewCum}, NewCum}
        end,
        0,
        Indexed
    ),
    %% Overflow bucket (position 11): durations > 10000ms.
    OverflowRaw = counters:get(Counters, Base + length(Boundaries) + 1),
    InfCum = CumAfterFinite + OverflowRaw,
    FiniteBuckets ++ [{infinity, InfCum}].

%% @doc Read current semaphore usage without crashing. Returns
%% {InUse, Capacity} or `unavailable` if the process is not running or
%% any unexpected error occurs. A crash here would fail the entire
%% /api/metrics scrape for every registered collector.
try_semaphore_usage() ->
    try
        aws_auth_validate_semaphore:usage()
    catch
        exit:{noproc, _} -> unavailable;
        exit:{timeout, _} -> unavailable;
        _:_ -> unavailable
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
