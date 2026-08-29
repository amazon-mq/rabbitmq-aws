%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_app_env).

-include("aws.hrl").

-export([update/4, delete/3, get_int_env/4, get_int_env/5]).

%% Read a bounded positive integer from an application's environment, accepting
%% [1, MaxBound]. Equivalent to get_int_env/5 with a lower bound of 1.
-spec get_int_env(atom(), atom(), pos_integer(), pos_integer()) -> pos_integer().
get_int_env(App, Key, Default, MaxBound) ->
    get_int_env(App, Key, Default, 1, MaxBound).

%% Read a bounded positive integer from an application's environment, accepting
%% [MinBound, MaxBound]. An unset, non-integer, or out-of-range value (below
%% MinBound or above MaxBound) falls back to Default rather than failing the
%% caller. A schema (where present) is the primary line of defense against bad
%% values, but the schema does not always declare a range, so an out-of-range
%% value can still reach here; log a warning in that case so the silent fallback
%% does not leave the operator wondering why their setting appears to have no
%% effect.
-spec get_int_env(atom(), atom(), pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
get_int_env(App, Key, Default, MinBound, MaxBound) ->
    case application:get_env(App, Key) of
        undefined ->
            Default;
        {ok, N} when is_integer(N), N >= MinBound, N =< MaxBound ->
            N;
        {ok, Bad} ->
            ?AWS_LOG_WARNING(
                "~p env key ~p rejected (value ~p; expected integer in [~b, ~b]); using default ~p",
                [App, Key, Bad, MinBound, MaxBound, Default]
            ),
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
