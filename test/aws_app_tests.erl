%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_app_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {foreach,
        fun() ->
            meck:new(aws_sup, [passthrough])
        end,
        fun(_) ->
            meck:unload(aws_sup)
        end,
        [
            {"supervisor initialized", fun() ->
                meck:expect(aws_sup, start_link, fun() -> {ok, test_result} end),
                ?assertMatch({ok, test_result}, aws_app:start(normal, [])),
                meck:validate(aws_sup)
            end}
        ]}.

stop_test() ->
    ?assertMatch(ok, aws_app:stop({})).
