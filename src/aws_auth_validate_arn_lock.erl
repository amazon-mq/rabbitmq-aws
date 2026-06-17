%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Serializes ARN resolution for the auth-validation endpoint.
%%
%% ARN resolution goes through the shared `rabbitmq_aws' singleton, and
%% `aws_sms'/`aws_acm_pca' call `rabbitmq_aws:set_region/1' (a global write)
%% derived from the request's ARN before issuing the HTTP call. The
%% concurrency semaphore admits up to `max_concurrent' validations at once,
%% so without serialization two concurrent requests for ARNs in different
%% regions can interleave: request A sets region R1, request B then sets R2,
%% and A signs/sends to the wrong region. This lock makes the
%% set_region-then-resolve section mutually exclusive across validation
%% requests so the region cannot be clobbered mid-resolution.
%%
%% Scope note: the broker's own boot-time ARN resolution (aws_arn_config) is
%% a rabbit_boot_step that runs before networking, hence before the endpoint
%% is reachable, so it never races validation traffic. Only
%% validation-vs-validation needs guarding, which is what this lock does.
%%
%% Implementation: a node-local `global:trans/4' lock (lock id scoped to
%% `node()'), not a dedicated gen_server. `global:trans/4' runs the closure
%% IN THE CALLER (so the resolved password never leaves this process -- it is
%% not copied into a server's mailbox, preserving the R6 no-leak property)
%% and wraps it in `try Fun() after del_lock', so the lock is always released
%% and any exception propagates to the caller unchanged. There is therefore
%% no separate server process to start or supervise.
%%
%% Requires a distributed (alive) node: `global' only serializes correctly
%% once `net_kernel' is up. The broker always runs as a distributed
%% `rabbit@host' node, so this holds in production.
-module(aws_auth_validate_arn_lock).

-export([with_lock/1]).

%% Lock id for global:trans/4. The {ResourceId, LockRequesterId} shape is
%% global's convention; scoping the resource to node() keeps the lock
%% node-local and distinct from any other global lock.
-define(LOCK_ID(Self), {{?MODULE, node()}, Self}).

%% global:trans/4 retries acquisition this many times before returning
%% `aborted' rather than blocking forever. Each retry waits for the current
%% holder to release, so this bounds how long a caller queues behind an
%% in-flight resolution. ARN resolution is a bounded HTTP call, so a finite
%% retry budget is enough; `infinity' could wedge a caller behind a stuck
%% holder with no escape.
-define(LOCK_RETRIES, 600).

%% Run Fun serialized against all other with_lock/1 callers on this node.
%% Returns Fun's value. Any exception Fun raises propagates to the caller
%% unchanged (global:trans/4 releases the lock via try/after first), matching
%% the behaviour of an unlocked call.
-spec with_lock(fun(() -> Result)) -> Result when Result :: term().
with_lock(Fun) when is_function(Fun, 0) ->
    case global:trans(?LOCK_ID(self()), Fun, [node()], ?LOCK_RETRIES) of
        aborted -> error(arn_lock_aborted);
        Result -> Result
    end.
