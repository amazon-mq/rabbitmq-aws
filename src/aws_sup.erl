%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1,
    deregister_collectors/0
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
    ChildSpecs = auth_validation_children() ++ node_health_children(),
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
            register_collectors(),
            [semaphore_spec()];
        _ ->
            []
    end.

%% Register the Prometheus collectors this plugin owns. A collector has no
%% process (it is a callback module registered with prometheus_registry), so no
%% child spec is needed. rabbitmq_prometheus is a declared dependency (which
%% transitively pulls in prometheus), so a failure here is a real fault and must
%% surface rather than be swallowed.
register_collectors() ->
    aws_auth_validate_metrics:register().

%% Tear down every collector register_collectors/0 owns so none outlives the
%% application: an orphan stays on the default registry and keeps running
%% collect_mf/2 on every scrape. Called from aws_app:stop/1.
deregister_collectors() ->
    lists:foreach(fun deregister_collector/1, [aws_auth_validate_metrics]).

%% Deregister one collector if it is currently registered. Driven by actual
%% registration state rather than the feature toggle, so a still-registered
%% collector is torn down even if the toggle no longer reads true; deregister/0
%% is idempotent. The catch tolerates prometheus not being started, in which
%% case nothing was ever registered.
deregister_collector(Mod) ->
    try prometheus_registry:collector_registeredp(Mod) of
        true -> Mod:deregister();
        false -> ok
    catch
        _:_ -> ok
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
    #{max => aws_app_env:get_int_env(aws, auth_validation_max_concurrent, 5, 100)}.

%%--------------------------------------------------------------------
%% Node-health feature: like auth validation, workers start only when the
%% toggle is on. When on, the Prometheus collector is registered and the
%% gossip worker is started; when off, the supervisor stays empty and no
%% peer-health metrics are emitted.
%%--------------------------------------------------------------------

node_health_children() ->
    case aws_node_health_config:enabled() of
        true ->
            %% Register the collector before the worker so a scrape between
            %% registration and the first sample simply reports the worker as
            %% unavailable rather than missing the collector entirely.
            aws_node_health_metrics:register(),
            [node_health_spec()];
        false ->
            []
    end.

%% The worker reads its own settings from aws_node_health_config, so no config
%% is threaded through here.
node_health_spec() ->
    #{
        id => aws_node_health_worker,
        start => {aws_node_health_worker, start_link, []},
        restart => permanent,
        shutdown => 5_000,
        type => worker,
        modules => [aws_node_health_worker]
    }.
