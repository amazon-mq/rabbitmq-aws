%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = auth_validation_children(),
    {ok, {SupFlags, ChildSpecs}}.

%%--------------------------------------------------------------------
%% Auth validation feature: workers are started only when the feature
%% toggle is on. With the toggle off, the supervisor remains empty and
%% the validation route returns 404, leaving the rest of the plugin
%% (ARN resolution) entirely undisturbed.
%%--------------------------------------------------------------------

auth_validation_children() ->
    case application:get_env(aws, auth_validation_enabled, false) of
        true -> [rate_limiter_spec(), semaphore_spec()];
        _ -> []
    end.

rate_limiter_spec() ->
    Config = rate_limiter_config(),
    #{
        id => aws_auth_validate_rate_limiter,
        start => {aws_auth_validate_rate_limiter, start_link, [Config]},
        restart => permanent,
        shutdown => 5_000,
        type => worker,
        modules => [aws_auth_validate_rate_limiter]
    }.

semaphore_spec() ->
    Config = semaphore_config(),
    #{
        id => aws_auth_validate_semaphore,
        start => {aws_auth_validate_semaphore, start_link, [Config]},
        restart => permanent,
        shutdown => 5_000,
        type => worker,
        modules => [aws_auth_validate_semaphore]
    }.

rate_limiter_config() ->
    WindowSecs = get_int_env(auth_validation_rate_limit_window_seconds, 60),
    Max = get_int_env(auth_validation_rate_limit_max_requests, 10),
    #{
        window_ms => WindowSecs * 1_000,
        max_per_window => Max,
        sweep_interval_ms => max(WindowSecs * 1_000 div 2, 1_000)
    }.

semaphore_config() ->
    #{max => get_int_env(auth_validation_max_concurrent, 5)}.

get_int_env(Key, Default) ->
    case application:get_env(aws, Key) of
        {ok, N} when is_integer(N), N > 0 -> N;
        _ -> Default
    end.
