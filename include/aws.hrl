%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-include_lib("kernel/include/logger.hrl").

-define(AWS_LOG_DEBUG(Arg),
    ?LOG_DEBUG(?MODULE_STRING ": ~tp", [Arg])
).

-define(AWS_LOG_DEBUG(Fmt, Args),
    ?LOG_DEBUG(?MODULE_STRING ": " ++ Fmt, Args)
).

-define(AWS_LOG_WARNING(Arg),
    ?LOG_WARNING(?MODULE_STRING ": ~ts", [Arg])
).

-define(AWS_LOG_WARNING(Fmt, Args),
    ?LOG_WARNING(?MODULE_STRING ": " ++ Fmt, Args)
).

-define(AWS_LOG_ERROR(Arg),
    ?LOG_ERROR(?MODULE_STRING ": ~tp", [Arg])
).

-define(AWS_LOG_ERROR(Fmt, Args),
    ?LOG_ERROR(?MODULE_STRING ": " ++ Fmt, Args)
).

-define(AWS_LOG_INFO(Arg),
    ?LOG_INFO(?MODULE_STRING ": ~ts", [Arg])
).

-define(AWS_LOG_INFO(Fmt, Args),
    ?LOG_INFO(?MODULE_STRING ": " ++ Fmt, Args)
).
