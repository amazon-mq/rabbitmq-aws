%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Behaviour every auth validation backend must implement. The registry
%% dispatches a request to a module that exports these three callbacks.
-module(aws_auth_validate_backend).

-export_type([error_category/0, result/0]).

-type error_category() ::
    input_invalid
    | connection_failed
    | tls_failed
    | auth_failed
    | config_conflict
    | query_invalid
    | authz_unverified.

-type result() :: ok | {error, error_category(), Reason :: binary()}.

-callback method_name() -> binary().
-callback validate(Body :: map()) -> result().
-callback allowed_fields() -> [binary()].
