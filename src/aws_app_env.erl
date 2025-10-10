%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

-module(aws_app_env).

-export([update/4, delete/3]).

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
