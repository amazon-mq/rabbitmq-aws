%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

-module(aws_s3).

-export([fetch_object/1]).

-spec fetch_object(string()) -> {ok, binary()} | {error, term()}.
fetch_object(Resource) ->
    %% Note: splits on the first / only
    %% https://www.erlang.org/doc/apps/stdlib/string.html#split/2
    [Bucket | Key] = string:split(Resource, "/"),
    Path = "/" ++ Bucket ++ "/" ++ Key,
    case rabbitmq_aws:api_get_request("s3", Path) of
        {ok, _} = Response ->
            Response;
        {error, _} = Error ->
            Error
    end.
