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
lookup_backend(<<"http">>) ->
    {ok, aws_auth_validate_http};
lookup_backend(<<"oauth">>) ->
    {ok, aws_auth_validate_oauth};
lookup_backend(<<"tls">>) ->
    {ok, aws_auth_validate_tls};
lookup_backend(_) ->
    {error, unknown_method}.

%% The override narrows a method's allowed fields; it can never widen them,
%% because every entry is intersected with the backend's own list.
%%
%% Keyed the same way as auth_validation_enabled_methods below: a single atom
%% key holding a [{Method, Fields}] list. application:get_env/2 requires an
%% atom parameter name, so a per-method key cannot be a {atom(), binary()}
%% tuple.
-spec effective_allowed_fields(module(), binary()) -> [binary()].
effective_allowed_fields(Module, Method) ->
    BaseFields = Module:allowed_fields(),
    case application:get_env(aws, auth_validation_allowed_fields_override) of
        {ok, Overrides} when is_list(Overrides) ->
            case lists:keyfind(Method, 1, Overrides) of
                {_, Override} when is_list(Override) ->
                    [F || F <- Override, lists:member(F, BaseFields)];
                _ ->
                    BaseFields
            end;
        _ ->
            BaseFields
    end.

%%--------------------------------------------------------------------

%% Per-method enable check. EVERY method is opt-in: a method in ?OPT_IN_METHODS
%% defaults to DISABLED and must be turned on explicitly with
%% aws.auth_validation.enabled_methods.<method> = true. The master feature
%% toggle (aws.auth_validation.enabled) only starts the subsystem; it never
%% brings any individual method online on its own.
%%
%% All four methods make an operator-supplied outbound connection or resolve a
%% secret ARN, so none should activate implicitly:
%%   * ldap connects to a customer-supplied LDAP server (an SSRF surface, guarded
%%     by aws_auth_validate_ldap's stricter address policy).
%%   * http and oauth connect to a customer-supplied URL (SSRF surface; address
%%     policy enforced in aws_auth_validate_net).
%%   * tls makes no outbound connection, but resolving a cacertfile ARN under the
%%     assume_role is still a capability worth enabling explicitly.
%% Because every method is listed, is_method_enabled/1's Default is always false;
%% the list is retained so the opt-in set stays explicit and greppable.
-define(OPT_IN_METHODS, [<<"ldap">>, <<"http">>, <<"oauth">>, <<"tls">>]).

-spec is_method_enabled(binary()) -> boolean().
is_method_enabled(Method) ->
    Default = not lists:member(Method, ?OPT_IN_METHODS),
    case application:get_env(aws, auth_validation_enabled_methods) of
        {ok, Methods} when is_list(Methods) ->
            case lists:keyfind(Method, 1, Methods) of
                {_, Bool} when is_boolean(Bool) -> Bool;
                false -> Default
            end;
        _ ->
            Default
    end.
