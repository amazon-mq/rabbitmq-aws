%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_arn_mgmt).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

-export([
    init/2,
    content_types_accepted/2,
    allowed_methods/2,
    resource_exists/2,
    is_authorized/2,
    accept_content/2
]).

-include_lib("rabbitmq_web_dispatch/include/rabbitmq_web_dispatch_records.hrl").
-include("aws.hrl").

dispatcher() -> [{"/aws/arn/validate", ?MODULE, []}].

web_ui() -> [].

%%--------------------------------------------------------------------

init(Req, _Opts) ->
    {cowboy_rest, rabbit_mgmt_cors:set_headers(Req, ?MODULE), #context{}}.

content_types_accepted(ReqData, Context) ->
    {[{'*', accept_content}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"PUT">>, <<"OPTIONS">>], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized(ReqData, Context).

accept_content(Req0, Context) ->
    {ok, Region} = rabbitmq_aws_config:region(),
    ok = rabbitmq_aws:set_region(Region),
    F = fun(_Values, BodyMap, Req1) ->
        process_content(BodyMap, Req1, Context)
    end,
    rabbit_mgmt_util:with_decode([], Req0, Context, F).

process_content(BodyMap, Req, Context) ->
    handle_assume_role(maybe_assume_role(BodyMap), BodyMap, Req, Context).

handle_assume_role({ok, AssumeRoleResult}, BodyMap, Req0, Context) ->
    try
        {ok, Results} = maybe_process_arns({map, BodyMap}),
        Body = rabbit_json:encode(rabbit_mgmt_format:format_nulls(Results)),
        Headers = #{<<"content-type">> => <<"application/json">>},
        Req1 = cowboy_req:reply(200, Headers, Body, Req0),
        ok = maybe_reset_credentials(AssumeRoleResult),
        {stop, Req1, Context}
    catch
        throw:{unprocessable_entity, ErrMsg} ->
            aws_mgmt_util:unprocessable_entity(ErrMsg, Req0, Context);
        Class:Reason:Stacktrace ->
            ?AWS_LOG_ERROR("~tp", [{Class, Reason}]),
            ?AWS_LOG_ERROR("~tp", [Stacktrace]),
            rabbit_mgmt_util:bad_request({Class, Reason}, Req0, Context)
    after
        maybe_reset_credentials(AssumeRoleResult)
    end;
handle_assume_role({error, Error}, _BodyMap, Req, Context) ->
    rabbit_mgmt_util:bad_request(Error, Req, Context).

maybe_assume_role(BodyMap) when is_map(BodyMap) andalso is_map_key(assume_role_arn, BodyMap) ->
    AssumeRoleArn = maps:get(assume_role_arn, BodyMap),
    maybe_assume_role({assume_role_arn, AssumeRoleArn});
maybe_assume_role(BodyMap) when is_map(BodyMap) andalso not is_map_key(assume_role_arn, BodyMap) ->
    {ok, not_assumed};
maybe_assume_role({assume_role_arn, AssumeRoleArn}) ->
    maybe_assume_role({validate_arn_result, validate_arn(AssumeRoleArn)});
maybe_assume_role({validate_arn_result, {ok, AssumeRoleArn}}) ->
    case aws_iam:assume_role(AssumeRoleArn) of
        {error, _} = Error ->
            Error;
        ok ->
            {ok, assumed}
    end;
maybe_assume_role({validate_arn_result, {error, _} = Error}) ->
    Error.

maybe_reset_credentials(assumed) ->
    aws_util:reset_aws_credentials();
maybe_reset_credentials(_) ->
    ok.

maybe_process_arns({map, BodyMap}) when is_map_key(arns, BodyMap) ->
    Arns = maps:get(arns, BodyMap),
    maybe_process_arns({arns, Arns});
maybe_process_arns({arns, Arns}) when length(Arns) > 0 ->
    process_arns(Arns);
maybe_process_arns({arns, _Arns}) ->
    throw({unprocessable_entity, "one or more ARNs to resolve are required"});
maybe_process_arns(_) ->
    throw({bad_request, "unknown error processing ARNs"}).

process_arns(Arns) ->
    process_arns(Arns, []).

process_arns([], Acc) ->
    {ok, Acc};
process_arns([Arn | Rest], Acc0) ->
    case aws_arn_util:resolve_arn(Arn) of
        {ok, ArnValue} ->
            R = #{arn => Arn, value => ArnValue},
            Acc1 = [R | Acc0],
            process_arns(Rest, Acc1);
        {error, {Reason, Msg}} ->
            throw({unprocessable_entity, fmt(Reason, Msg)});
        {error, Error} ->
            throw({unprocessable_entity, Error});
        Unexpected ->
            throw({bad_request, Unexpected})
    end.

validate_arn(Arn) ->
    case aws_arn_util:parse_arn(Arn) of
        {ok, Map} when is_map(Map) ->
            {ok, Arn};
        Error ->
            Error
    end.

fmt(Reason, Msg) ->
    rabbit_data_coercion:to_utf8_binary(io_lib:format("~tp: ~ts", [Reason, Msg])).
