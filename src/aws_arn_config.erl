%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

-module(aws_arn_config).

-include("aws.hrl").

-export([process_arns/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

% TODO: remove after we fix the rabbitmq_aws:api_post_request's return type
% the current return type is type result() but what it actuall
-dialyzer({no_match, maybe_assume_role/1}).

-spec process_arns() -> ok.
%% @doc Fetch certificate files, secrets from Amazon S3 and Secret Manager and update application configuration to use them
%% @end
process_arns() ->
    try
        ok = application:ensure_started(rabbitmq_aws),
        case process_arn_config({handle_env_arn_config, application:get_env(aws, arn_config)}) of
            {ok, {iam_role_result, assumed}} ->
                ?AWS_LOG_INFO("success"),
                aws_util:reset_aws_credentials();
            {ok, {iam_role_result, not_assumed}} ->
                ?AWS_LOG_INFO("success");
            {error, credentials, _} = Error ->
                % Note: do NOT reset credentials in this error case
                ?AWS_LOG_ERROR("~tp", [Error]);
            {error, Error, {iam_role_result, assumed}} ->
                ?AWS_LOG_ERROR("~tp", [Error]),
                aws_util:reset_aws_credentials();
            {error, Error, {iam_role_result, not_assumed}} ->
                ?AWS_LOG_ERROR("~tp", [Error]);
            Unexpected ->
                ?AWS_LOG_ERROR("unexpected result: ~tp", [Unexpected])
        end
    catch
        Class:Reason:Stacktrace ->
            ?AWS_LOG_ERROR("~tp", [{Class, Reason}]),
            ?AWS_LOG_ERROR("~tp", [Stacktrace])
    end.

maybe_assume_role({arn_config, ArnConfig}) when is_list(ArnConfig) ->
    maybe_assume_role({assume_role_arn_value, proplists:get_value(assume_role_arn, ArnConfig)});
maybe_assume_role({assume_role_arn_value, undefined}) ->
    % No assume role configured, use existing credentials
    ?AWS_LOG_WARNING("aws.arns.assume_role_arn is not present in configuration"),
    {ok, not_assumed};
maybe_assume_role({assume_role_arn_value, RoleArn}) ->
    case aws_iam:assume_role(RoleArn) of
        {error, Error} ->
            {error, {assume_role_failed, Error}};
        ok ->
            {ok, assumed}
    end.

process_arn_config({handle_env_arn_config, undefined}) ->
    ?AWS_LOG_INFO("no ARNs to process"),
    {ok, {iam_role_result, not_assumed}};
process_arn_config({handle_env_arn_config, {ok, ArnConfig}}) ->
    process_arn_config({handle_env_arns, proplists:get_value(arns, ArnConfig), ArnConfig});
process_arn_config({handle_env_arns, undefined, _}) ->
    ?AWS_LOG_INFO("no ARNs to process"),
    {ok, {iam_role_result, not_assumed}};
process_arn_config({handle_env_arns, ArnList, ArnConfig}) ->
    {ok, Region} = rabbitmq_aws_config:region(),
    ok = rabbitmq_aws:set_region(Region),
    % Assume role once, then process all ARNs with those credentials
    process_arn_config({handle_assume_role, maybe_assume_role({arn_config, ArnConfig})}, ArnList).

process_arn_config({handle_assume_role, {ok, AssumeRoleResult}}, ArnList) ->
    handle_arn_handlers_result(run_arn_handlers(ArnList), AssumeRoleResult);
process_arn_config({handle_assume_role, {error, _}} = Error, _ArnList) ->
    {error, Error, {iam_role_result, not_assumed}}.

handle_arn_handlers_result(ok, AssumeRoleResult) ->
    {ok, {iam_role_result, AssumeRoleResult}};
handle_arn_handlers_result({error, Error}, AssumeRoleResult) ->
    {error, Error, {iam_role_result, AssumeRoleResult}}.

run_arn_handlers([]) ->
    ok;
run_arn_handlers([{Mod, undefined, _SchemaKey, Args} | Rest]) ->
    case erlang:apply(Mod, run, Args) of
        ok ->
            run_arn_handlers(Rest);
        Error ->
            Error
    end;
run_arn_handlers([{Mod, Arn, SchemaKey, Args} | Rest]) ->
    case aws_arn_util:resolve_arn(Arn) of
        {ok, ArnData} ->
            case erlang:apply(Mod, run, [ArnData | Args]) of
                ok ->
                    run_arn_handlers(Rest);
                Error ->
                    Error
            end;
        Error ->
            get_resolve_arn_error(Arn, SchemaKey, Error)
    end.

get_resolve_arn_error(Arn, SchemaKey, {error, E} = Error) ->
    ErrMsg0 = io_lib:format(
        "could not resolve ARN '~ts' for configuration '~ts', error: ~tp",
        [Arn, SchemaKey, E]
    ),
    ErrMsg1 = rabbit_data_coercion:to_utf8_binary(ErrMsg0),
    {error, {ErrMsg1, Error}}.
