%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Prometheus collector for node-health attribution.
%%
%% Pull-based (implements prometheus_collector): Prometheus scrapes and invokes
%% collect_mf/2, which reads the current state from aws_node_health_worker at
%% scrape time and emits three gauges keyed by the `peer` label:
%%
%%   rabbitmq_peer_down_probability -- this node's own view of each peer's
%%       down-probability (the raw failure-detector row; needs no gossip).
%%   rabbitmq_peer_down_suspected   -- 1 if the cross-node decision attributes
%%       that peer as the single degraded node, else 0.
%%   rabbitmq_peer_down_confidence  -- confidence in [0,1] that the peer is the
%%       single degraded node.
%%
%% Reading the worker is crash-safe: if it is not running (feature disabled or
%% mid-restart) the scrape emits nothing rather than failing every other
%% collector registered on the same endpoint.
-module(aws_node_health_metrics).

-behaviour(prometheus_collector).

-export([register/0, deregister/0]).
-export([deregister_cleanup/1, collect_mf/2]).

-ifdef(TEST).
-export([probability_samples/1, suspected_samples/1, confidence_samples/1]).
-endif.

-import(prometheus_model_helpers, [create_mf/4]).

-include("aws.hrl").

-spec register() -> ok.
register() ->
    prometheus_registry:register_collector(?MODULE),
    ?AWS_LOG_INFO("node_health metrics: registered collector"),
    ok.

-spec deregister() -> ok.
deregister() ->
    prometheus_registry:deregister_collector(?MODULE),
    ?AWS_LOG_INFO("node_health metrics: deregistered collector"),
    ok.

deregister_cleanup(_Registry) ->
    ok.

collect_mf(_Registry, Callback) ->
    case read_worker() of
        unavailable ->
            ok;
        {OwnView, Scores} ->
            Callback(
                create_mf(
                    rabbitmq_peer_down_probability,
                    "This node's estimated probability that each peer node is down",
                    gauge,
                    probability_samples(OwnView)
                )
            ),
            Callback(
                create_mf(
                    rabbitmq_peer_down_suspected,
                    "Whether each peer node is attributed as the single degraded node (1) or not (0)",
                    gauge,
                    suspected_samples(Scores)
                )
            ),
            Callback(
                create_mf(
                    rabbitmq_peer_down_confidence,
                    "Confidence in [0,1] that each peer node is the single degraded node",
                    gauge,
                    confidence_samples(Scores)
                )
            ),
            ok
    end.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%% Read the worker's own view and scores in one place, tolerating a worker that
%% is not running. A crash here would fail the whole /metrics scrape.
read_worker() ->
    try
        OwnView = aws_node_health_worker:own_view(),
        #{scores := Scores} = aws_node_health_worker:latest(),
        {OwnView, Scores}
    catch
        exit:{noproc, _} -> unavailable;
        exit:{timeout, _} -> unavailable;
        _:_ -> unavailable
    end.

probability_samples(OwnView) ->
    [{[{peer, Peer}], Prob} || Peer := Prob <- OwnView].

suspected_samples(Scores) ->
    [{[{peer, Peer}], maps:get(suspected, Score)} || Peer := Score <- Scores].

confidence_samples(Scores) ->
    [{[{peer, Peer}], maps:get(confidence, Score)} || Peer := Score <- Scores].
