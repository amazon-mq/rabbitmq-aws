%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_iam).

-export([assume_role/1]).

% TODO: remove after we fix the rabbitmq_aws:api_post_request's return type
-dialyzer([
    {no_unused, parse_assume_role_response/1},
    {no_match, make_request/2}
]).

-spec assume_role(string() | binary()) -> ok | {error, term()}.
assume_role(RoleArn) when is_binary(RoleArn) ->
    assume_role(binary_to_list(RoleArn));
assume_role(RoleArn) ->
    SessionName = "rabbitmq-aws-" ++ integer_to_list(erlang:system_time(second)),
    Body =
        "Action=AssumeRole&RoleArn=" ++ uri_string:quote(RoleArn) ++
            "&RoleSessionName=" ++ uri_string:quote(SessionName) ++
            "&Version=2011-06-15",

    BaseHeaders = [
        {"content-type", "application/x-www-form-urlencoded"},
        {"accept", "application/json"}
    ],

    Headers = aws_sts:add_custom_headers(BaseHeaders),
    make_request(Body, Headers).

-spec parse_assume_role_response(any()) -> ok.
parse_assume_role_response(Body) ->
    [{"AssumeRoleResponse", ResponseData}] = Body,
    {"AssumeRoleResult", ResultData} = lists:keyfind("AssumeRoleResult", 1, ResponseData),
    {"Credentials", CredentialsData} = lists:keyfind("Credentials", 1, ResultData),
    {"AccessKeyId", AccessKey} = lists:keyfind("AccessKeyId", 1, CredentialsData),
    {"SecretAccessKey", SecretKey} = lists:keyfind("SecretAccessKey", 1, CredentialsData),
    {"SessionToken", SessionToken} = lists:keyfind("SessionToken", 1, CredentialsData),
    ok = rabbitmq_aws:set_credentials(AccessKey, SecretKey, SessionToken).

make_request(Body, Headers) ->
    case rabbitmq_aws:api_post_request("sts", "/", Body, Headers) of
        {ok, ResponseBody} ->
            parse_assume_role_response(ResponseBody);
        Error ->
            Error
    end.
