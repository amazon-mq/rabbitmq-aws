%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

-export([boot_step/1]).

-rabbit_boot_step(
    {aws_arn_config, [
        {description, "Overrides plugin application environments to set values via AWS ARNs"},
        {mfa, {?MODULE, boot_step, [aws_arn_config]}},
        {enables, networking}
    ]}
).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    aws_sup:start_link().

stop(_State) ->
    ok.

boot_step(aws_arn_config) ->
    aws_arn_config:process_arns();
boot_step(Unknown) ->
    ?LOG_WARNING("unknown boot step: ~tp", [Unknown]),
    ok.
