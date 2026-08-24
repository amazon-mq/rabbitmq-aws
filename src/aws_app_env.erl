%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_app_env).

-export([update/4, delete/3, get_int_env/4]).

%% Read a bounded positive integer from an application's environment. An unset,
%% non-integer, or out-of-range value falls back to Default rather than failing
%% the caller; a schema (where present) is what reports a bad value to the
%% operator. Shared by aws_sup and aws_node_health_config so the bounds/fallback
%% logic lives in one place.
-spec get_int_env(atom(), atom(), pos_integer(), pos_integer()) -> pos_integer().
get_int_env(App, Key, Default, MaxBound) ->
    case application:get_env(App, Key) of
        {ok, N} when is_integer(N), N > 0, N =< MaxBound ->
            N;
        _ ->
            Default
    end.

-spec update(
    App :: atom(),
    ConfigKey :: atom(),
    Key :: atom(),
    Value :: any()
) -> ok.
update(App, ConfigKey, Key, Value) ->
    Config =
        case application:get_env(App, ConfigKey) of
            {ok, ExistingConfig} -> ExistingConfig;
            undefined -> []
        end,
    NewConfig = lists:keystore(Key, 1, Config, {Key, Value}),
    ok = application:set_env(App, ConfigKey, NewConfig).

-spec delete(
    App :: atom(),
    ConfigKey :: atom(),
    KeyToDelete :: atom()
) -> {'ok', 'false' | tuple()}.
delete(App, ConfigKey, KeyToDelete) ->
    ConfigValue =
        case application:get_env(App, ConfigKey) of
            {ok, Val} -> Val;
            undefined -> []
        end,
    OrigConfigValue = lists:keyfind(KeyToDelete, 1, ConfigValue),
    NewConfig = lists:keydelete(KeyToDelete, 1, ConfigValue),
    ok = application:set_env(App, ConfigKey, NewConfig),
    {ok, OrigConfigValue}.
