%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_acm_pca).

-export([fetch_certificate/2]).

% TODO: remove after we fix the rabbitmq_aws:api_post_request's return type
-dialyzer({no_match, make_request/2}).

-spec fetch_certificate(string(), string()) -> {ok, binary()} | {error, term()}.
fetch_certificate(CaArn, Region) ->
    rabbitmq_aws:set_region(Region),
    RequestBody = rabbit_json:encode(#{
        <<"CertificateAuthorityArn">> => rabbit_data_coercion:to_utf8_binary(CaArn)
    }),
    Headers = [
        {"X-Amz-Target", "ACMPrivateCA.GetCertificateAuthorityCertificate"},
        {"Content-Type", "application/x-amz-json-1.1"}
    ],
    make_request(RequestBody, Headers).

make_request(RequestBody, Headers) ->
    case rabbitmq_aws:api_post_request("acm-pca", "/", RequestBody, Headers) of
        {ok, ResponseBody} ->
            case rabbit_json:decode(ResponseBody) of
                #{<<"Certificate">> := Certificate} ->
                    {ok, Certificate};
                _ ->
                    {error, no_certificate}
            end;
        {error, _} = Error ->
            Error
    end.
