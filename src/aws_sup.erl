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
    %% Tolerate a few transient worker crashes before giving up: a very low
    %% intensity would tear down the whole supervisor on a second crash in a
    %% short window. The validation worker is an independent gen_server, so
    %% allow several restarts in a slightly wider window before escalating.
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
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
        true ->
            %% Register the Prometheus metrics collector before starting the
            %% semaphore worker. The collector has no process (it is a callback
            %% module registered with prometheus_registry), so no child spec is
            %% needed. rabbitmq_prometheus is a declared dependency (which
            %% transitively pulls in prometheus), so a failure here is a real
            %% fault and must surface rather than be swallowed.
            aws_auth_validate_metrics:register(),
            [semaphore_spec()];
        _ ->
            []
    end.

%% The concurrency semaphore bounds simultaneous outbound LDAP connections;
%% it is the endpoint's primary, topology-independent backpressure. (ARN
%% resolution needs no serialization: each request threads its own
%% aws_state() through aws_lib, so there is no shared region/credential
%% singleton to clobber.)
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

semaphore_config() ->
    #{max => get_int_env(auth_validation_max_concurrent, 5, 100)}.

%% An out-of-range or non-integer value falls back to Default rather than
%% failing the boot; the schema is what reports a bad value to the operator.
-spec get_int_env(atom(), pos_integer(), pos_integer()) -> pos_integer().
get_int_env(Key, Default, MaxBound) ->
    case application:get_env(aws, Key) of
        {ok, N} when is_integer(N), N > 0, N =< MaxBound ->
            N;
        _ ->
            Default
    end.
