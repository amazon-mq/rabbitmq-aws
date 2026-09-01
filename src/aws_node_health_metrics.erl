%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Prometheus collector for node-health attribution.
%%
%% Pull-based (implements prometheus_collector): Prometheus scrapes and invokes
%% collect_mf/2, which reads the current state from aws_node_health_worker at
%% scrape time and emits three per-peer gauges keyed by the `peer` label plus one
%% unlabelled cluster-level gauge:
%%
%%   rabbitmq_aws_node_health_peer_down_probability -- this node's own view of each peer's
%%       down-probability (the raw failure-detector row; needs no gossip).
%%   rabbitmq_aws_node_health_peer_down_suspected   -- 1 if the cross-node decision attributes
%%       that peer as the single degraded node, else 0.
%%   rabbitmq_aws_node_health_peer_down_confidence  -- confidence in [0,1] that the peer is the
%%       single degraded node.
%%   rabbitmq_aws_node_health_cluster_congested     -- 1 if the degradation is symmetric across
%%       the cluster (not attributable to any single node), else 0. Cluster-level,
%%       so it carries no `peer` label.
%%
%% Reading the worker is crash-safe: if it is not running (feature disabled or
%% mid-restart) the scrape emits nothing rather than failing every other
%% collector registered on the same endpoint.
-module(aws_node_health_metrics).

-behaviour(prometheus_collector).

-export([register/0, deregister/0]).
-export([deregister_cleanup/1, collect_mf/2]).

-ifdef(TEST).
-export([probability_samples/1, suspected_samples/2, confidence_samples/2, congested_sample/1]).
-endif.

-import(prometheus_model_helpers, [create_mf/4]).

-include("aws.hrl").

-spec register() -> ok.
register() ->
    %% Idempotent: register/0 runs as a side effect of aws_sup:init/1, which can
    %% run again if the supervisor is restarted (e.g. the worker crash-loops past
    %% the supervisor intensity). Registering the same collector twice would emit
    %% duplicate metric families, so only register when not already present.
    case lists:member(?MODULE, prometheus_registry:collectors(default)) of
        true ->
            ok;
        false ->
            prometheus_registry:register_collector(?MODULE),
            ?AWS_LOG_INFO("node_health metrics: registered collector"),
            ok
    end.

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
        {OwnView, Verdict, Scores} ->
            Callback(
                create_mf(
                    rabbitmq_aws_node_health_peer_down_probability,
                    "This node's estimated probability that each peer node is down",
                    gauge,
                    probability_samples(OwnView)
                )
            ),
            Callback(
                create_mf(
                    rabbitmq_aws_node_health_peer_down_suspected,
                    "Whether each peer node is attributed as the single degraded node (1) or not (0)",
                    gauge,
                    suspected_samples(node(), Scores)
                )
            ),
            Callback(
                create_mf(
                    rabbitmq_aws_node_health_peer_down_confidence,
                    "Confidence in [0,1] that each peer node is the single degraded node",
                    gauge,
                    confidence_samples(node(), Scores)
                )
            ),
            Callback(
                create_mf(
                    rabbitmq_aws_node_health_cluster_congested,
                    "Whether cluster network degradation is symmetric across nodes (1), not attributable to any single node, or not (0)",
                    gauge,
                    congested_sample(Verdict)
                )
            ),
            ok
    end.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%% Read the worker's own view and scores in a single call, tolerating a worker
%% that is not running or is too busy to answer promptly. A crash or a long
%% block here would fail (or stall) the whole /metrics scrape.
read_worker() ->
    try
        {OwnView, #{verdict := Verdict, scores := Scores}} = aws_node_health_worker:report(),
        {OwnView, Verdict, Scores}
    catch
        %% Expected fast paths when the feature is disabled or the worker's
        %% mailbox is backed up: emit no families and stay silent.
        exit:{noproc, _} ->
            unavailable;
        exit:{timeout, _} ->
            unavailable;
        %% Anything else means the worker returned a shape we cannot pattern
        %% match (contract drift) or a sample builder raised. Log once at
        %% error level so a regression does not just present as "metrics
        %% silently absent" with nothing in the logs.
        Class:Reason:Stack ->
            ?AWS_LOG_ERROR(
                "node_health metrics: unexpected ~p:~p reading worker; emitting no families~n~p",
                [Class, Reason, Stack]
            ),
            unavailable
    end.

%% The scores map covers every node in the matrix, including this node itself
%% (it appears as a peer in the other nodes' rows). Exclude Self so the suspected
%% and confidence gauges share the same `peer` domain as probability (which comes
%% from aten's own view and never includes self), and so a node never reports
%% itself as a suspected-down peer on its own /metrics.
probability_samples(OwnView) ->
    [{[{peer, Peer}], Prob} || Peer := Prob <- OwnView].

suspected_samples(Self, Scores) ->
    [{[{peer, Peer}], maps:get(suspected, Score)} || Peer := Score <- Scores, Peer =/= Self].

confidence_samples(Self, Scores) ->
    [{[{peer, Peer}], maps:get(confidence, Score)} || Peer := Score <- Scores, Peer =/= Self].

%% A single unlabelled sample: 1 when the debounced verdict is cluster_wide
%% (symmetric congestion, not attributable to any one node), else 0. This is a
%% cluster-level signal, so unlike the peer gauges it carries no `peer` label.
-spec congested_sample(aws_node_health:verdict()) -> [{[], 0 | 1}].
congested_sample(cluster_wide) -> [{[], 1}];
congested_sample(_) -> [{[], 0}].
