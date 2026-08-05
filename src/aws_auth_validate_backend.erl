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
    | authz_unverified
    %% Customer-supplied access-token verification outcomes (oauth backend).
    %% Split out of auth_failed so an operator can distinguish a transient,
    %% non-config problem (token_expired -- just re-mint and retry) from a real
    %% config mismatch (token_invalid -- the JWKS the broker fetches will reject
    %% live tokens). Safe to be granular here: unlike the reachability
    %% categories, these describe the caller's own token, not the broker's infra
    %% or an SSRF target, so they disclose nothing the fixed-category rule guards
    %% (that rule prevents echoing raw errors, hostnames, or network details that
    %% would enable SSRF reconnaissance).
    | token_expired
    | token_invalid.

-type result() :: ok | {error, error_category(), Reason :: binary()}.

-callback method_name() -> binary().
-callback validate(Body :: map()) -> result().
-callback allowed_fields() -> [binary()].
