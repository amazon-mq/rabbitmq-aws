%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Dedicated sub-supervisor for the node-health feature.
%%
%% The feature's worker is placed under its own supervisor (rather than
%% directly under aws_sup with the auth_validation semaphore) so that a
%% crash-looping node_health worker consumes only its own restart budget and
%% cannot cascade a top-level restart that would also take down unrelated
%% features. If this sub-supervisor itself exceeds its intensity, only
%% node_health goes offline; the plugin's other features are unaffected.
-module(aws_node_health_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% one_for_one with the same intensity/period aws_sup used to run at, but
    %% scoped to this feature: at most 5 worker restarts in a 10-second window
    %% before we give up on node_health specifically.
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [worker_spec()],
    {ok, {SupFlags, ChildSpecs}}.

%% The worker reads its own settings from aws_node_health_config, so no config
%% is threaded through here.
worker_spec() ->
    #{
        id => aws_node_health_worker,
        start => {aws_node_health_worker, start_link, []},
        restart => permanent,
        shutdown => 5_000,
        type => worker,
        modules => [aws_node_health_worker]
    }.
