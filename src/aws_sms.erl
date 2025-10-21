%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

-module(aws_sms).

-export([fetch_secret/2]).

% TODO: remove after we fix the rabbitmq_aws:api_post_request's return type
% the current return type is type result() but what it actuall
-dialyzer({no_match, make_request/2}).

-spec fetch_secret(string(), string()) -> {ok, binary()} | {error, term()}.
fetch_secret(Arn, Region) ->
    rabbitmq_aws:set_region(Region),
    RequestBody = rabbit_json:encode(#{
        <<"SecretId">> => rabbit_data_coercion:to_utf8_binary(Arn),
        <<"VersionStage">> => <<"AWSCURRENT">>
    }),
    Headers = [
        {"X-Amz-Target", "secretsmanager.GetSecretValue"},
        {"Content-Type", "application/x-amz-json-1.1"}
    ],
    make_request(RequestBody, Headers).
make_request(RequestBody, Headers) ->
    case rabbitmq_aws:api_post_request("secretsmanager", "/", RequestBody, Headers) of
        {ok, ResponseBody} ->
            case rabbit_json:decode(ResponseBody) of
                #{<<"SecretString">> := SecretValue} ->
                    {ok, SecretValue};
                #{<<"SecretBinary">> := SecretBinary} ->
                    {ok, base64:decode(SecretBinary)};
                _ ->
                    {error, no_secret_value}
            end;
        {error, _} = Error ->
            Error
    end.
