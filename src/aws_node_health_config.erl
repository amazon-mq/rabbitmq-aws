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

-export([enabled/0, interval_ms/0, window/0, stale_ticks/0, worker_config/0]).

-define(DEFAULT_INTERVAL_MS, 1000).
-define(MAX_INTERVAL_MS, 60000).
-define(DEFAULT_WINDOW, 30).
-define(MAX_WINDOW, 10000).
-define(DEFAULT_STALE_TICKS, 5).
-define(MAX_STALE_TICKS, 10000).

%% Master toggle. Default off: with the feature disabled the supervisor starts
%% no worker and registers no metrics.
-spec enabled() -> boolean().
enabled() ->
    application:get_env(aws, node_health_enabled, false) =:= true.

%% Sampling and recompute period in milliseconds.
-spec interval_ms() -> pos_integer().
interval_ms() ->
    get_int(node_health_interval_ms, ?DEFAULT_INTERVAL_MS, ?MAX_INTERVAL_MS).

%% Number of snapshots retained in the rolling decision window.
-spec window() -> pos_integer().
window() ->
    get_int(node_health_window, ?DEFAULT_WINDOW, ?MAX_WINDOW).

%% Drop a peer's row if it has not refreshed within this many ticks.
-spec stale_ticks() -> pos_integer().
stale_ticks() ->
    get_int(node_health_stale_ticks, ?DEFAULT_STALE_TICKS, ?MAX_STALE_TICKS).

%% The environment-derived settings the worker needs: the numeric knobs plus
%% the scoring thresholds. The worker layers its own runtime defaults (local
%% node, peer list, sampler) beneath this.
-spec worker_config() -> map().
worker_config() ->
    #{
        interval_ms => interval_ms(),
        window_max => window(),
        stale_ticks => stale_ticks(),
        analysis => aws_node_health:default_config()
    }.

%% Read a positive integer from the environment, falling back to Default for an
%% unset, non-integer, or out-of-range value. The schema reports a bad value to
%% the operator; this keeps the boot resilient.
-spec get_int(atom(), pos_integer(), pos_integer()) -> pos_integer().
get_int(Key, Default, MaxBound) ->
    case application:get_env(aws, Key) of
        {ok, N} when is_integer(N), N > 0, N =< MaxBound ->
            N;
        _ ->
            Default
    end.
