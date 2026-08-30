%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Single home for the node-health feature's configuration. Every
%% application:get_env read for the feature lives here, along with the
%% defaults, the runtime bounds, and the assembled worker settings, so the
%% supervisor and worker never touch the environment directly.
%%
%% Only the numeric knobs are operator-tunable (via the aws.node_health.*
%% schema mappings); the scoring thresholds come from aws_node_health and the
%% worker's runtime dependencies (local node, peer list, sampler) are supplied
%% by the worker itself.
-module(aws_node_health_config).

-export([
    enabled/0,
    interval_ms/0,
    window/0,
    stale_ticks/0,
    confirm_ticks/0,
    clear_ticks/0,
    worker_config/0
]).

-define(DEFAULT_INTERVAL_MS, 1000).
-define(MAX_INTERVAL_MS, 60000).
%% Lower bound on the sampling interval. A single sample may take up to the
%% worker's SAMPLE_TIMEOUT_MS (500 ms); an interval below that would drive the
%% sample/gossip cycle faster than one sample can complete, hammering aten_sink
%% (which sits on the real failure-detection hot path) and flooding gossip. An
%% out-of-range value falls back to the default rather than being honoured.
-define(MIN_INTERVAL_MS, 500).
-define(DEFAULT_WINDOW, 30).
%% Upper bound on the decision window. analyze/3 rescans the whole window for
%% every (node, peer) pair each tick (own_outbound_min/own_view_median), an
%% O(Nodes^2 * Window) recompute, so an oversized window makes every interval
%% expensive for no detection benefit. Cap it well above the default (30) but
%% far below the old 10000. An out-of-range value falls back to the default.
-define(MAX_WINDOW, 600).
-define(DEFAULT_STALE_TICKS, 5).
-define(MAX_STALE_TICKS, 10000).
-define(DEFAULT_CONFIRM_TICKS, 3).
-define(MAX_CONFIRM_TICKS, 10000).
-define(DEFAULT_CLEAR_TICKS, 3).
-define(MAX_CLEAR_TICKS, 10000).

%% Main toggle. Default off: with the feature disabled the supervisor starts
%% no worker and registers no metrics.
-spec enabled() -> boolean().
enabled() ->
    application:get_env(aws, node_health_enabled, false) =:= true.

%% Sampling and recompute period in milliseconds. Bounded below by
%% MIN_INTERVAL_MS so a too-small setting cannot hammer the failure detector.
-spec interval_ms() -> pos_integer().
interval_ms() ->
    get_int(node_health_interval_ms, ?DEFAULT_INTERVAL_MS, ?MIN_INTERVAL_MS, ?MAX_INTERVAL_MS).

%% Number of snapshots retained in the rolling decision window.
-spec window() -> pos_integer().
window() ->
    get_int(node_health_window, ?DEFAULT_WINDOW, ?MAX_WINDOW).

%% Drop a peer's row if it has not refreshed within this many ticks.
-spec stale_ticks() -> pos_integer().
stale_ticks() ->
    get_int(node_health_stale_ticks, ?DEFAULT_STALE_TICKS, ?MAX_STALE_TICKS).

%% Hysteresis: a peer must be the raw suspect for this many consecutive ticks
%% before its `suspected` flag is published (debounces transient verdict noise).
-spec confirm_ticks() -> pos_integer().
confirm_ticks() ->
    get_int(node_health_confirm_ticks, ?DEFAULT_CONFIRM_TICKS, ?MAX_CONFIRM_TICKS).

%% Hysteresis: a published suspect stays suspected until it is no longer the raw
%% suspect for this many consecutive ticks (debounces transient clears).
-spec clear_ticks() -> pos_integer().
clear_ticks() ->
    get_int(node_health_clear_ticks, ?DEFAULT_CLEAR_TICKS, ?MAX_CLEAR_TICKS).

%% The environment-derived settings the worker needs: the numeric knobs plus
%% the scoring thresholds. The worker layers its own runtime defaults (local
%% node, peer list, sampler) beneath this.
-spec worker_config() -> map().
worker_config() ->
    #{
        interval_ms => interval_ms(),
        window_max => window(),
        stale_ticks => stale_ticks(),
        confirm_ticks => confirm_ticks(),
        clear_ticks => clear_ticks(),
        analysis => aws_node_health:default_config()
    }.

%% Read a bounded positive integer from the `aws' application environment,
%% falling back to Default for an unset, non-integer, or out-of-range value.
%% Delegates to the shared aws_app_env helper so the bounds/fallback logic is
%% not duplicated with aws_sup.
-spec get_int(atom(), pos_integer(), pos_integer()) -> pos_integer().
get_int(Key, Default, MaxBound) ->
    aws_app_env:get_int_env(aws, Key, Default, MaxBound).

%% As get_int/3, with an explicit lower bound (currently only interval_ms needs
%% one; the other knobs are safe down to 1).
-spec get_int(atom(), pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
get_int(Key, Default, MinBound, MaxBound) ->
    aws_app_env:get_int_env(aws, Key, Default, MinBound, MaxBound).
