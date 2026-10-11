%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_iam).

-export([assume_role/2, assume_role/3, parse_assume_role_response/2]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("aws_lib.hrl").

-spec assume_role(string() | binary(), aws_lib:aws_state()) ->
    {ok, aws_lib:aws_state()} | {error, term()}.
%% @doc Assume a role using the given RoleArn, setting the resulting credentials
%% on the returned state. This is the legacy interface that sets credentials
%% directly on the state. For credential chain resolution, use assume_role/3
%% which returns the credentials without modifying state.
%% @end
assume_role(RoleArn, State) when is_binary(RoleArn) ->
    assume_role(binary_to_list(RoleArn), State);
assume_role(RoleArn, State) ->
    case assume_role(RoleArn, #{}, State) of
        {ok, Creds} ->
            {ok, State1} = aws_lib:set_credentials(
                Creds#aws_credentials.access_key,
                Creds#aws_credentials.secret_key,
                Creds#aws_credentials.security_token,
                State
            ),
            {ok, State1};
        {error, _} = Error ->
            Error
    end.

-spec assume_role(string() | binary(), map(), aws_lib:aws_state()) ->
    {ok, aws_credentials()} | {error, term()}.
%% @doc Assume a role and return the resulting credentials (with Expiration).
%%
%% Opts map keys (all optional):
%%   - external_id :: string() -- ExternalId for the STS request
%%   - role_session_name :: string() -- custom RoleSessionName (must be valid)
%%
%% This function is called by aws_lib_config during credential chain resolution.
%% It is ONLY reachable from the boot-time config file path -- not from any
%% validation request body (the auth-validation endpoint's allowed_fields/0
%% allowlists do NOT include role_arn/source_profile).
%% @end
assume_role(RoleArn, Opts, State) when is_binary(RoleArn) ->
    assume_role(binary_to_list(RoleArn), Opts, State);
assume_role(RoleArn, Opts, State) ->
    SessionName = maps:get(
        role_session_name,
        Opts,
        "rabbitmq-aws-" ++ integer_to_list(erlang:system_time(second))
    ),
    ExternalId = maps:get(external_id, Opts, undefined),
    Body = build_assume_role_body(RoleArn, SessionName, ExternalId),

    BaseHeaders = [
        {"content-type", "application/x-www-form-urlencoded"},
        {"accept", "application/json"}
    ],

    Headers = aws_sts:add_custom_headers(BaseHeaders),
    make_request_return_creds(Body, Headers, State).

%% @doc Build the URL-encoded form body for the AssumeRole POST request.
%% Includes ExternalId only when explicitly provided (never defaulted --
%% ExternalId is treated as sensitive and must not be logged per R6).
%% @end
build_assume_role_body(RoleArn, SessionName, undefined) ->
    "Action=AssumeRole&RoleArn=" ++ uri_string:quote(RoleArn) ++
        "&RoleSessionName=" ++ uri_string:quote(SessionName) ++
        "&Version=2011-06-15";
build_assume_role_body(RoleArn, SessionName, ExternalId) ->
    "Action=AssumeRole&RoleArn=" ++ uri_string:quote(RoleArn) ++
        "&RoleSessionName=" ++ uri_string:quote(SessionName) ++
        "&ExternalId=" ++ uri_string:quote(ExternalId) ++
        "&Version=2011-06-15".

-spec parse_assume_role_credentials(any()) -> {ok, aws_credentials()}.
%% @doc Parse the AssumeRole XML response into an #aws_credentials{} record
%% including the Expiration field. Called during credential chain resolution.
%% @end
parse_assume_role_credentials(Body) ->
    [{"AssumeRoleResponse", ResponseData}] = Body,
    {"AssumeRoleResult", ResultData} = lists:keyfind("AssumeRoleResult", 1, ResponseData),
    {"Credentials", CredentialsData} = lists:keyfind("Credentials", 1, ResultData),
    {"AccessKeyId", AccessKey} = lists:keyfind("AccessKeyId", 1, CredentialsData),
    {"SecretAccessKey", SecretKey} = lists:keyfind("SecretAccessKey", 1, CredentialsData),
    {"SessionToken", SessionToken} = lists:keyfind("SessionToken", 1, CredentialsData),
    Expiration =
        case lists:keyfind("Expiration", 1, CredentialsData) of
            {"Expiration", ExpirationStr} ->
                aws_lib_config:parse_iso8601_timestamp(ExpirationStr);
            false ->
                undefined
        end,
    Creds = #aws_credentials{
        access_key = AccessKey,
        secret_key = SecretKey,
        security_token = SessionToken,
        expiration = Expiration
    },
    {ok, Creds}.

make_request_return_creds(Body, Headers, State) ->
    case aws_lib:api_post_request("sts", "/", Body, Headers, State) of
        {ok, ResponseBody, _State1} ->
            parse_assume_role_credentials(ResponseBody);
        {error, Reason, _State1} ->
            {error, Reason}
    end.

-spec parse_assume_role_response(any(), aws_lib:aws_state()) ->
    {ok, aws_lib:aws_state()} | {error, term()}.
%% @doc Legacy interface: parse an AssumeRole XML response and set the resulting
%% credentials on the given state. Used by aws_arn_config for boot-time ARN
%% resolution.
%% @end
parse_assume_role_response(Body, State) ->
    {ok, Creds} = parse_assume_role_credentials(Body),
    aws_lib:set_credentials(
        Creds#aws_credentials.access_key,
        Creds#aws_credentials.secret_key,
        Creds#aws_credentials.security_token,
        State
    ).
