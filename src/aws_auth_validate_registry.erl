%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Maps a method path segment to a backend module, gates dispatch on
%% per-method enable flags, and filters the request body to the
%% intersection of the backend's allowed_fields/0 and any operator
%% override list.
-module(aws_auth_validate_registry).

-export([dispatch/2, lookup_backend/1, effective_allowed_fields/2]).

-spec dispatch(binary(), map()) ->
    aws_auth_validate_backend:result()
    | {error, unknown_method | method_disabled}.
dispatch(Method, Body) when is_binary(Method), is_map(Body) ->
    case lookup_backend(Method) of
        {error, _} = Err ->
            Err;
        {ok, Module} ->
            case is_method_enabled(Method) of
                false ->
                    {error, method_disabled};
                true ->
                    AllowedFields = effective_allowed_fields(Module, Method),
                    FilteredBody = maps:with(AllowedFields, Body),
                    Module:validate(FilteredBody)
            end
    end.

-spec lookup_backend(binary()) -> {ok, module()} | {error, unknown_method}.
lookup_backend(<<"ldap">>) ->
    {ok, aws_auth_validate_ldap};
lookup_backend(_) ->
    {error, unknown_method}.

-spec effective_allowed_fields(module(), binary()) -> [binary()].
effective_allowed_fields(Module, Method) ->
    BaseFields = Module:allowed_fields(),
    case application:get_env(aws, {auth_validation_allowed_fields_override, Method}) of
        {ok, Override} when is_list(Override) ->
            [F || F <- Override, lists:member(F, BaseFields)];
        _ ->
            BaseFields
    end.

%%--------------------------------------------------------------------

%% Per-method enable check. Defaults to enabled when no per-method
%% setting is provided so the operator only has to opt out, not in.
-spec is_method_enabled(binary()) -> boolean().
is_method_enabled(Method) ->
    case application:get_env(aws, auth_validation_enabled_methods) of
        {ok, Methods} when is_list(Methods) ->
            case lists:keyfind(Method, 1, Methods) of
                {_, Bool} when is_boolean(Bool) -> Bool;
                false -> true
            end;
        _ ->
            true
    end.
