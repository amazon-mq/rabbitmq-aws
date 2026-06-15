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
%% The closure runs INSIDE this gen_server, so resolutions are inherently
%% serialized. A crashing closure is caught here (the lock server survives)
%% and its exception is re-raised in the caller, preserving the existing
%% try/catch behaviour in aws_auth_validate_ldap:validate/1.
-module(aws_auth_validate_arn_lock).

-behaviour(gen_server).

-export([start_link/0, start_link/1, with_lock/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Upper bound on a single serialized resolution. Must exceed the AWS
%% client's own retry/timeout budget so a slow-but-progressing resolve is
%% not aborted, while still bounding how long one stuck resolve can block
%% the queue. rabbitmq_aws does linear-backoff retries with finite per-call
%% timeouts, so 60s is comfortably above a normal worst case.
-define(DEFAULT_CALL_TIMEOUT_MS, 60_000).

-record(state, {}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% Run Fun under the lock, serialized against all other with_lock/1 callers.
%% Returns Fun's value, or re-raises in the caller whatever exception Fun
%% raised (so callers see the same error class/reason as an unlocked call).
-spec with_lock(fun(() -> Result)) -> Result.
with_lock(Fun) when is_function(Fun, 0) ->
    case gen_server:call(?MODULE, {run, Fun}, ?DEFAULT_CALL_TIMEOUT_MS) of
        {ok, Result} -> Result;
        {raised, Class, Reason, Stacktrace} -> erlang:raise(Class, Reason, Stacktrace)
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(_Config) ->
    {ok, #state{}}.

handle_call({run, Fun}, _From, State) ->
    %% Run the closure here so it is serialized with every other request.
    %% Catch any exception so a failed resolution never crashes the lock
    %% server; re-raise it in the caller to preserve unlocked semantics.
    Reply =
        try Fun() of
            Result -> {ok, Result}
        catch
            Class:Reason:Stacktrace -> {raised, Class, Reason, Stacktrace}
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
