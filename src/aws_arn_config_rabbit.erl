%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

-module(aws_arn_config_rabbit).

-export([run/4]).

run(ArnData, _KeyStr, ssl_options, ConfigSubKey) ->
    handle_content(ConfigSubKey, ArnData).

%%--------------------------------------------------------------------------------------------------
%% rabbit application
%%
handle_content(cacertfile, PemData) ->
    aws_arn_env:replace(
        rabbit,
        ssl_options,
        cacertfile,
        cacerts,
        aws_pem_util:decode_data(PemData)
    );
handle_content(certfile, PemData) ->
    aws_arn_env:replace(
        rabbit,
        ssl_options,
        certfile,
        certs_keys,
        aws_pem_util:decode_data(PemData)
    );
handle_content(keyfile, PemData) ->
    aws_arn_env:replace(
        rabbit,
        ssl_options,
        keyfile,
        certs_keys,
        aws_pem_util:decode_key_data(PemData)
    ).
