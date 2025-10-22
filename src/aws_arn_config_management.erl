%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_arn_config_management).

-export([run/3, run/4]).

run(ArnData, _KeyStr, oauth_client_secret = ConfigSubKey) ->
    handle_content(ConfigSubKey, ArnData).

run(ArnData, _KeyStr, ssl_options, ConfigSubKey) ->
    handle_content(ConfigSubKey, ArnData).

%%--------------------------------------------------------------------------------------------------
%% rabbitmq_management plugin
%%
handle_content(cacertfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_management,
        ssl_config,
        cacertfile,
        cacerts,
        aws_pem_util:decode_data(PemData)
    );
handle_content(certfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_management,
        ssl_config,
        certfile,
        certs_keys,
        aws_pem_util:decode_data(PemData)
    );
handle_content(keyfile, PemData) ->
    aws_arn_env:replace(
        rabbitmq_management,
        ssl_config,
        keyfile,
        certs_keys,
        aws_pem_util:decode_key_data(PemData)
    );
handle_content(oauth_client_secret, ClientSecret) ->
    aws_arn_env:replace(
        rabbitmq_management,
        oauth_client_secret,
        ClientSecret
    ).
