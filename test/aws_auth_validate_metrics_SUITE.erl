%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Common Test suite for aws_auth_validate_metrics.
%%
%% Tests cover:
%%   - Counter increments for requests_total
%%   - Histogram bucket population for duration
%%   - Overflow (+Inf) bucket for durations > 10000ms
%%   - Duration gating (pre-connection rejects excluded from histogram)
%%   - Unknown method tracking in requests_total
%%   - Dedicated capacity_exhausted counter
%%   - Semaphore gauge reads at scrape time
%%   - Semaphore exit catch-all (arbitrary exits degrade gracefully)
%%   - No-op behaviour when metrics are not registered
%%   - Crash safety (observe/3 never raises)
%%   - register/0 idempotency (preserves counters on re-registration)
-module(aws_auth_validate_metrics_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    requests_total_increments/1,
    duration_histogram_records/1,
    inf_bucket_exists/1,
    duration_gating_excludes_fast_rejects/1,
    unknown_method_counted/1,
    capacity_exhausted_increments/1,
    semaphore_gauges/1,
    semaphore_exit_catch_all/1,
    no_state_when_disabled/1,
    crash_safety/1,
    unknown_method_no_crash/1,
    unknown_category_no_crash/1,
    register_preserves_counters/1,
    deregister_collectors_removes_registered/1,
    deregister_collectors_ignores_feature_toggle/1,
    deregister_collectors_not_registered_noop/1,
    app_stop_deregisters/1
]).

all() ->
    [
        requests_total_increments,
        duration_histogram_records,
        inf_bucket_exists,
        duration_gating_excludes_fast_rejects,
        unknown_method_counted,
        capacity_exhausted_increments,
        semaphore_gauges,
        semaphore_exit_catch_all,
        no_state_when_disabled,
        crash_safety,
        unknown_method_no_crash,
        unknown_category_no_crash,
        register_preserves_counters,
        deregister_collectors_removes_registered,
        deregister_collectors_ignores_feature_toggle,
        deregister_collectors_not_registered_noop,
        app_stop_deregisters
    ].

init_per_suite(Config) ->
    %% Start applications that prometheus_registry needs.
    {ok, _} = application:ensure_all_started(prometheus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) when
    TestCase =:= no_state_when_disabled;
    TestCase =:= crash_safety;
    TestCase =:= deregister_collectors_not_registered_noop
->
    %% Ensure metrics are NOT registered for this test case.
    catch aws_auth_validate_metrics:deregister(),
    _ = persistent_term:erase({aws_auth_validate_metrics, counters}),
    Config;
init_per_testcase(_TestCase, Config) ->
    %% Ensure clean state: deregister any prior collector and erase counters.
    catch aws_auth_validate_metrics:deregister(),
    %% Register metrics fresh for each test case.
    aws_auth_validate_metrics:register(),
    Config.

end_per_testcase(TestCase, _Config) when
    TestCase =:= no_state_when_disabled;
    TestCase =:= crash_safety;
    TestCase =:= deregister_collectors_not_registered_noop
->
    ok;
end_per_testcase(_TestCase, _Config) ->
    %% Clean up: deregister collector and erase persistent_term.
    catch aws_auth_validate_metrics:deregister(),
    catch meck:unload(aws_auth_validate_semaphore),
    ok.

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

%% Verify that observe/3 increments the requests_total counter for multiple
%% (method, category) combinations.
requests_total_increments(_Config) ->
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 50),
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 30),
    aws_auth_validate_metrics:observe(<<"ldap">>, auth_failed, 100),
    aws_auth_validate_metrics:observe(<<"http">>, connection_failed, 200),
    aws_auth_validate_metrics:observe(<<"oauth">>, token_expired, 500),

    %% Read counters directly to verify.
    Counters = aws_auth_validate_metrics:counter_ref(),
    ?assertNotEqual(undefined, Counters),

    %% ldap + success: method_index=0, category_index=0, slot = 0*15 + 0 + 1 = 1
    ?assertEqual(2, counters:get(Counters, 1)),
    %% ldap + auth_failed: method_index=0, category_index=6, slot = 0*15 + 6 + 1 = 7
    ?assertEqual(1, counters:get(Counters, 7)),
    %% http + connection_failed: method_index=1, category_index=3, slot = 1*15 + 3 + 1 = 19
    ?assertEqual(1, counters:get(Counters, 19)),
    %% oauth + token_expired: method_index=2, category_index=9, slot = 2*15 + 9 + 1 = 40
    ?assertEqual(1, counters:get(Counters, 40)),
    ok.

%% Verify that observe/3 populates histogram buckets correctly for timed
%% categories. Uses the updated layout: SLOTS_PER_HIST=13, NUM_BUCKETS=11.
duration_histogram_records(_Config) ->
    %% Observe durations in different buckets (all with timed category `success`):
    %% 5ms -> bucket 1 (<=10), 75ms -> bucket 4 (<=100), 3000ms -> bucket 9 (<=5000)
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 5),
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 75),
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 3000),

    Counters = aws_auth_validate_metrics:counter_ref(),
    Base = aws_auth_validate_metrics:histogram_base(),

    %% ldap is method_index 0, so its histogram starts at Base + 0*13.
    %% Bucket 1 (<=10ms): slot = Base + 0*13 + 1 = Base + 1
    ?assertEqual(1, counters:get(Counters, Base + 1)),
    %% Bucket 4 (<=100ms): slot = Base + 0*13 + 4 = Base + 4
    ?assertEqual(1, counters:get(Counters, Base + 4)),
    %% Bucket 9 (<=5000ms): slot = Base + 0*13 + 9 = Base + 9
    ?assertEqual(1, counters:get(Counters, Base + 9)),
    %% Sum slot: Base + 0*13 + 12 = Base + 12 (NUM_BUCKETS+1)
    ?assertEqual(5 + 75 + 3000, counters:get(Counters, Base + 12)),
    %% Count slot: Base + 0*13 + 13 = Base + 13 (NUM_BUCKETS+2)
    ?assertEqual(3, counters:get(Counters, Base + 13)),
    ok.

%% Verify that durations > 10000ms land in the overflow bucket (position 11)
%% and that the +Inf bucket is emitted with cumulative count = sample_count.
inf_bucket_exists(_Config) ->
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 15000),
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 20000),
    %% Also add one in a finite bucket to verify cumulation.
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 5),

    Counters = aws_auth_validate_metrics:counter_ref(),
    Base = aws_auth_validate_metrics:histogram_base(),

    %% Overflow bucket slot: Base + 0*13 + 11 = Base + 11
    ?assertEqual(2, counters:get(Counters, Base + 11)),
    %% Finite bucket 1 (<=10ms): Base + 1
    ?assertEqual(1, counters:get(Counters, Base + 1)),
    %% Count slot: Base + 13 -- all three observations
    ?assertEqual(3, counters:get(Counters, Base + 13)),
    %% Sum slot: Base + 12
    ?assertEqual(15000 + 20000 + 5, counters:get(Counters, Base + 12)),

    %% Directly verify build_cumulative_buckets returns +Inf as last entry.
    Buckets = aws_auth_validate_metrics:build_cumulative_buckets(Counters, Base),
    %% Should be 11 entries: 10 finite boundaries + 1 infinity.
    ?assertEqual(11, length(Buckets)),
    %% Last bucket must be {infinity, 3} (total sample count).
    ?assertEqual({infinity, 3}, lists:last(Buckets)),
    %% First bucket (le=10) should be cumulative 1 (the 5ms observation).
    ?assertEqual({10, 1}, hd(Buckets)),

    %% Also verify collect_mf/2 does not crash.
    Collected = collect_all_mf(),
    ?assert(length(Collected) >= 1),
    ok.

%% Verify that pre-connection reject categories increment requests_total
%% but do NOT record duration in the histogram.
duration_gating_excludes_fast_rejects(_Config) ->
    aws_auth_validate_metrics:observe(<<"ldap">>, input_invalid, 50),
    aws_auth_validate_metrics:observe(<<"ldap">>, body_too_large, 20),
    aws_auth_validate_metrics:observe(<<"ldap">>, query_invalid, 30),

    Counters = aws_auth_validate_metrics:counter_ref(),
    Base = aws_auth_validate_metrics:histogram_base(),

    %% requests_total should have incremented for each.
    %% ldap + input_invalid: slot = 0*15 + 1 + 1 = 2
    ?assertEqual(1, counters:get(Counters, 2)),
    %% ldap + body_too_large: slot = 0*15 + 2 + 1 = 3
    ?assertEqual(1, counters:get(Counters, 3)),
    %% ldap + query_invalid: slot = 0*15 + 5 + 1 = 6
    ?assertEqual(1, counters:get(Counters, 6)),

    %% Duration histogram count for ldap must remain 0 -- none of these are
    %% timed categories.
    CountSlot = Base + 0 * 13 + 13,
    ?assertEqual(0, counters:get(Counters, CountSlot)),
    ok.

%% Verify that requests with an unrecognized method are counted under the
%% synthetic method="unknown" label in requests_total.
unknown_method_counted(_Config) ->
    aws_auth_validate_metrics:observe(<<"bogus">>, unknown_method, 10),

    Counters = aws_auth_validate_metrics:counter_ref(),
    %% unknown method_index=4, unknown_method category_index=12
    %% slot = 4*15 + 12 + 1 = 73
    ?assertEqual(1, counters:get(Counters, 73)),
    ok.

%% Verify that capacity_exhausted observations increment the correct counter.
capacity_exhausted_increments(_Config) ->
    aws_auth_validate_metrics:observe(<<"ldap">>, capacity_exhausted, 10),
    aws_auth_validate_metrics:observe(<<"ldap">>, capacity_exhausted, 20),
    aws_auth_validate_metrics:observe(<<"http">>, capacity_exhausted, 15),

    Counters = aws_auth_validate_metrics:counter_ref(),
    %% ldap + capacity_exhausted: method_index=0, category_index=11, slot = 0*15 + 11 + 1 = 12
    ?assertEqual(2, counters:get(Counters, 12)),
    %% http + capacity_exhausted: method_index=1, category_index=11, slot = 1*15 + 11 + 1 = 27
    ?assertEqual(1, counters:get(Counters, 27)),
    ok.

%% Verify that semaphore gauges reflect live state from the semaphore worker.
semaphore_gauges(_Config) ->
    %% Start a semaphore with max=3.
    {ok, Pid} = aws_auth_validate_semaphore:start_link(#{max => 3}),

    %% Acquire two slots.
    {ok, Ref1} = aws_auth_validate_semaphore:acquire(),
    {ok, _Ref2} = aws_auth_validate_semaphore:acquire(),

    %% Verify usage/0 returns current state.
    {2, 3} = aws_auth_validate_semaphore:usage(),

    %% Release one.
    ok = aws_auth_validate_semaphore:release(Ref1),
    {1, 3} = aws_auth_validate_semaphore:usage(),

    %% Clean up.
    gen_server:stop(Pid),
    ok.

%% Verify that an arbitrary (non-standard) exit from the semaphore does not
%% crash the metrics scrape -- it degrades gracefully to unavailable.
semaphore_exit_catch_all(_Config) ->
    meck:new(aws_auth_validate_semaphore, [passthrough]),
    meck:expect(aws_auth_validate_semaphore, usage, fun() -> exit({killed, test_reason}) end),

    %% collect_mf/2 should complete without crashing.
    Ref = make_ref(),
    Self = self(),
    Callback = fun(MF) -> Self ! {Ref, MF} end,
    ?assertEqual(ok, aws_auth_validate_metrics:collect_mf(default, Callback)),

    meck:unload(aws_auth_validate_semaphore),
    ok.

%% Verify that when metrics are not registered, no persistent_term entries exist.
no_state_when_disabled(_Config) ->
    ?assertEqual(undefined, persistent_term:get({aws_auth_validate_metrics, counters}, undefined)),
    ok.

%% Verify that calling observe/3 when not registered does not crash.
crash_safety(_Config) ->
    %% Should be a no-op, not a crash.
    ?assertEqual(ok, aws_auth_validate_metrics:observe(<<"ldap">>, success, 100)),
    ?assertEqual(ok, aws_auth_validate_metrics:observe(<<"http">>, auth_failed, 50)),
    ok.

%% Verify that observing an unknown method does not crash.
unknown_method_no_crash(_Config) ->
    ?assertEqual(ok, aws_auth_validate_metrics:observe(<<"unknown_method">>, success, 100)),
    ok.

%% Verify that observing an unknown category does not crash.
unknown_category_no_crash(_Config) ->
    ?assertEqual(ok, aws_auth_validate_metrics:observe(<<"ldap">>, some_unknown_category, 100)),
    ok.

%% Verify that calling register/0 a second time does not reset existing
%% counter values (get-or-create semantics).
register_preserves_counters(_Config) ->
    %% Observe something first.
    aws_auth_validate_metrics:observe(<<"ldap">>, success, 50),

    Counters1 = aws_auth_validate_metrics:counter_ref(),
    %% ldap + success: slot 1
    Val1 = counters:get(Counters1, 1),
    ?assertEqual(1, Val1),

    %% Re-register -- must NOT reset counters.
    aws_auth_validate_metrics:register(),

    Counters2 = aws_auth_validate_metrics:counter_ref(),
    ?assertEqual(Counters1, Counters2),
    Val2 = counters:get(Counters2, 1),
    ?assertEqual(1, Val2),
    ok.

%% A registered collector is removed from the registry and its counters erased,
%% symmetric with the boot-time registration.
deregister_collectors_removes_registered(_Config) ->
    ?assert(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)),
    ?assertEqual(ok, aws_sup:deregister_collectors()),
    ?assertNot(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)),
    ?assertEqual(undefined, aws_auth_validate_metrics:counter_ref()).

%% Teardown is driven by registration state, not the feature toggle: a
%% registered collector is torn down even when the toggle reads false, the
%% orphan case a toggle-gated teardown would miss.
deregister_collectors_ignores_feature_toggle(_Config) ->
    ?assert(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)),
    application:set_env(aws, auth_validation_enabled, false),
    try
        ?assertEqual(ok, aws_sup:deregister_collectors()),
        ?assertNot(prometheus_registry:collector_registeredp(aws_auth_validate_metrics))
    after
        application:unset_env(aws, auth_validation_enabled)
    end.

%% With no collector registered, teardown is a safe no-op: it returns ok without
%% raising and touches nothing (deregister/0 is never called, so no spurious
%% deregistration log is emitted on a feature-off shutdown).
deregister_collectors_not_registered_noop(_Config) ->
    ?assertNot(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)),
    ?assertEqual(ok, aws_sup:deregister_collectors()),
    ?assertNot(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)).

%% aws_app:stop/1 drives the teardown, so a registered collector does not
%% outlive the application.
app_stop_deregisters(_Config) ->
    ?assert(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)),
    ?assertEqual(ok, aws_app:stop(undefined)),
    ?assertNot(prometheus_registry:collector_registeredp(aws_auth_validate_metrics)),
    ?assertEqual(undefined, aws_auth_validate_metrics:counter_ref()).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Invoke collect_mf/2 and return the list of all metric families emitted.
collect_all_mf() ->
    Ref = make_ref(),
    Self = self(),
    Callback = fun(MF) -> Self ! {Ref, MF} end,
    aws_auth_validate_metrics:collect_mf(default, Callback),
    collect_all_mf_loop(Ref, []).

collect_all_mf_loop(Ref, Acc) ->
    receive
        {Ref, MF} -> collect_all_mf_loop(Ref, [MF | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.
