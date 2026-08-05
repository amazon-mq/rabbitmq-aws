%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Shared ephemeral-httpc-profile pool for the http and oauth validation
%% backends. (The ldap backend uses eldap, not httpc, so it does not use this.)
%%
%% This machinery exists ONLY because httpc pools TLS sessions on a global,
%% named-profile basis: the shared default profile would let a prior request's
%% authenticated (e.g. mTLS) session be reused by a later request -- a false
%% success and a cross-request connection leak (violating the zero-side-effects
%% invariant that each validation uses only ephemeral connections). Each in-flight
%% probe therefore OWNS a distinct started profile drawn from a FIXED pool of
%% ?PROFILE_POOL_SIZE interned atoms (a fresh atom per request would leak the
%% atom table). A slot is claimed only by successfully starting it, so two
%% simultaneous calls never share a profile and none is ever stolen from an
%% actively-probing peer. The pool is sized strictly above the concurrency cap
%% (auth_validation_max_concurrent = 100) so the scan always finds a free slot.
%%
%% Both backends carried a byte-identical copy of this before; the only
%% difference was the profile-name prefix (kept per-backend so the two pools
%% never collide), which is now a parameter.
-module(aws_auth_validate_httpc).

-export([
    claim_probe_profile/1,
    set_probe_profile_opts/1,
    stop_probe_profile/1
]).

-define(PROFILE_POOL_SIZE, 128).

profile_atom(Prefix, Slot) ->
    list_to_atom(Prefix ++ integer_to_list(Slot)).

%% Claim a profile by scanning the fixed pool for a FREE slot: start at a
%% per-request hashed slot and try to inets:start it. Starting succeeds only if
%% no concurrent validation already owns that slot, so success means we own it.
%% On {already_started,_} (slot running) or already_present (slot mid-teardown)
%% advance to the next slot -- never stop a slot in use by a peer, that would
%% tear down its in-flight request. Bounded to one full pass over the pool; if
%% every slot is busy, fail closed. Returns {ok, Profile} | none.
%%
%% Prefix disambiguates the http and oauth pools so their atoms never collide.
-spec claim_probe_profile(string()) -> {ok, atom()} | none.
claim_probe_profile(Prefix) ->
    Start = erlang:phash2(make_ref(), ?PROFILE_POOL_SIZE),
    claim_probe_profile(Prefix, Start, ?PROFILE_POOL_SIZE).

claim_probe_profile(_Prefix, _Slot, 0) ->
    %% Every slot busy: unreachable while the semaphore caps concurrency below
    %% ?PROFILE_POOL_SIZE, but fail closed rather than reuse a shared profile.
    none;
claim_probe_profile(Prefix, Slot, Remaining) ->
    Profile = profile_atom(Prefix, Slot),
    case inets:start(httpc, [{profile, Profile}]) of
        {ok, _Pid} ->
            %% We started (own) this profile; disable reuse and take it.
            set_probe_profile_opts(Profile),
            {ok, Profile};
        {error, {already_started, _}} ->
            %% Slot in use by a concurrent validation -- do NOT steal it; advance.
            claim_probe_profile(Prefix, (Slot + 1) rem ?PROFILE_POOL_SIZE, Remaining - 1);
        {error, already_present} ->
            %% Slot mid-teardown by a concurrent validation. inets:stop/2 runs
            %% terminate_child THEN delete_child as two separate supervisor
            %% calls; a permanent child (the httpc profile) keeps its spec with
            %% pid=undefined between them, so a start landing in that window sees
            %% already_present rather than already_started. Not cleanly free --
            %% advance like the already_started case rather than fail this
            %% request closed. The pool size exceeds the concurrency cap, so the
            %% scan still lands on a genuinely free slot within one pass.
            claim_probe_profile(Prefix, (Slot + 1) rem ?PROFILE_POOL_SIZE, Remaining - 1);
        _ ->
            %% Any other start error: fail closed rather than risk the shared
            %% default profile's session-reuse hazard.
            none
    end.

set_probe_profile_opts(Profile) ->
    _ = httpc:set_options(
        [{max_sessions, 0}, {max_keep_alive_length, 0}, {keep_alive_timeout, 0}],
        Profile
    ),
    ok.

stop_probe_profile(Profile) ->
    _ = inets:stop(httpc, Profile),
    ok.
