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
    %% Tolerate a few transient child crashes before giving up. Each feature
    %% is scoped so it consumes its own restart budget rather than the
    %% top-level's: the auth_validation semaphore runs directly under this
    %% supervisor, while node_health runs under its own aws_node_health_sup
    %% (see node_health_children/0). A crash-looping feature therefore takes
    %% only itself offline, not the plugin's other features.
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    %% Register the collectors before building the child specs so each is
    %% present before its worker starts (a scrape landing between registration
    %% and the first sample reports the worker as unavailable rather than the
    %% collector as missing).
    register_collectors(),
    ChildSpecs = auth_validation_children() ++ node_health_children(),
    {ok, {SupFlags, ChildSpecs}}.

%%--------------------------------------------------------------------
%% Auth validation feature: workers are started only when the feature
%% toggle is on. With the toggle off, the supervisor remains empty and
%% the validation route returns 404, leaving the rest of the plugin
%% (ARN resolution) entirely undisturbed.
%%--------------------------------------------------------------------

auth_validation_children() ->
    case auth_validation_enabled() of
        true ->
            [semaphore_spec()];
        _ ->
            []
    end.

auth_validation_enabled() ->
    application:get_env(aws, auth_validation_enabled, false) =:= true.

%% Register each feature's Prometheus collector when that feature is enabled,
%% symmetric with deregister_collectors/0. A collector has no process (it is a
%% callback module registered with prometheus_registry), so no child spec is
%% needed. rabbitmq_prometheus is a declared dependency (which transitively
%% pulls in prometheus), so a failure here is a real fault and must surface
%% rather than be swallowed.
register_collectors() ->
    maybe_register(auth_validation_enabled(), aws_auth_validate_metrics),
    maybe_register(aws_node_health_config:enabled(), aws_node_health_metrics).

maybe_register(true, Mod) -> Mod:register();
maybe_register(false, _Mod) -> ok.

%% Tear down every Prometheus collector register_collectors/0 registers, so
%% none outlives the application: an orphan stays on the default registry and
%% keeps running collect_mf/2 on every scrape. Each is deregistered only if
%% actually registered, so listing both regardless of which feature is enabled
%% is safe. Called from aws_app:stop/1.
deregister_collectors() ->
    deregister_collector(aws_auth_validate_metrics),
    deregister_collector(aws_node_health_metrics).

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
            [node_health_sup_spec()];
        false ->
            []
    end.

%% The worker lives under its own supervisor (aws_node_health_sup) so that a
%% crash-looping worker consumes only its own restart budget and cannot cascade
%% a top-level restart that would also take down auth_validation.
node_health_sup_spec() ->
    #{
        id => aws_node_health_sup,
        start => {aws_node_health_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [aws_node_health_sup]
    }.
