%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_arn_config_http).

-export([run/4]).

run(ArnData, _KeyStr, ssl_options, ConfigSubKey) ->
    handle_content(ConfigSubKey, ArnData).

%%--------------------------------------------------------------------------------------------------
%% rabbitmq_auth_backend_http plugin
%%
handle_content(cacertfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_auth_backend_http,
        ssl_options,
        cacertfile,
        cacerts,
        aws_pem_util:decode_data(PemData)
    );
handle_content(certfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_auth_backend_http,
        ssl_options,
        certfile,
        certs_keys,
        aws_pem_util:decode_data(PemData)
    );
handle_content(keyfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_auth_backend_http,
        ssl_options,
        keyfile,
        certs_keys,
        aws_pem_util:decode_key_data(PemData)
    ).
