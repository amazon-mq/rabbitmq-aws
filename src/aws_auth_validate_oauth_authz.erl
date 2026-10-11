%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Optional OAuth authorization-evaluation layer, wired as an opt-in step in
%% aws_auth_validate_oauth (activates only when a request carries an authz_check
%% block; a no-op otherwise).
%%
%% Purpose: given a *already-verified* customer-supplied access token (signature
%% + exp + aud already checked by aws_auth_validate_oauth), answer "would the
%% broker grant this principal <permission> on <vhost>/<resource>?" -- i.e. reach
%% an authorization decision, not just an authentication one. This is a
%% troubleshooting/triage aid: it localizes WHICH layer of an OAuth config is
%% wrong (reachability vs. authentication vs. authorization) and, for authz,
%% names the failing STAGE (see the reason macros), deflecting the "my token is
%% broken (it isn't)" support ticket without a broker restart.
%%
%% Design: RUNTIME SOFT DEPENDENCY on rabbitmq_auth_backend_oauth2.
%%   * We do NOT add it to the plugin's DEPS (it is not force-loaded onto
%%     aws-plugin users who never run OAuth).
%%   * We CALL its exported, pure decision functions when-and-only-when the
%%     module is already loaded on the broker, guarded by
%%     erlang:function_exported/3. When it is not loaded, authz evaluation is
%%     unavailable and we say so with a fixed category -- we never fall back to a
%%     home-grown scope matcher (that would reintroduce the drift risk this
%%     approach exists to avoid).
%%   * Because we EXECUTE the broker's own SCOPE-MATCHING code rather than mirror
%%     it, the matching logic itself cannot drift. The parity guarantee is scoped
%%     to that: it covers the fields we overlay onto the #resource_server{} record
%%     below -- scope_prefix, scope_aliases, additional_scopes_key -- plus the
%%     scope_pattern_syntax we thread through. It does NOT cover
%%     resource-server config the broker also consumes but that this endpoint does
%%     not model: resource_server_type (the rich-authorization-request path in
%%     normalize_token_scope), extra_scopes_source, preferred_username_claims, and
%%     per-`resource_servers' map entries. Those stay at new_resource_server/1
%%     defaults here. For an operator relying on them, this endpoint can under-
%%     report effective scopes and thus FAIL an authz_check the live broker would
%%     grant (the safe direction -- it never over-grants -- but still a parity gap
%%     to document, not a "drift is impossible" claim). Extending the overlay to
%%     those fields is a documented follow-up.
%%
%% The broker functions used (all verified pure -- no app-env / ETS / mnesia on
%% this path; they read all config from the #resource_server{} record we build):
%%   * rabbit_oauth2_resource_server:new_resource_server/1  -- record constructor
%%   * rabbit_oauth2_schema:tokenize_additional_scopes_key/1 -- (when present) splits
%%       the caller's `additional_scopes_key' the SAME way rabbitmq.conf does, so an
%%       escaped `\.' stays a literal dot in a flat claim name (rabbitmq/rabbitmq-server
%%       #16947). Absent on pre-fix series -> we pass the raw binary through and the
%%       backend's own split_path/1 applies (see tokenize_additional_scopes_key/1).
%%   * rabbit_auth_backend_oauth2:normalize_token_scope/2   -- alias / additional-
%%       scopes-key / prefix-filter expansion (the IAM `scope_aliases' path)
%%   * rabbit_auth_backend_oauth2:get_expanded_scopes/3     -- {vhost} placeholder
%%       expansion
%%   * rabbit_oauth2_scope:{vhost,resource,topic}_access/*  -- the final match
%%
%% Zero side effects: every function above is side-effect-free; the config
%% comes from the record argument, so this reads nothing from broker state and
%% mutates nothing. We construct the #resource_server{} from CUSTOMER-SUPPLIED
%% fields, so we are validating the customer's config, not the broker's live one.
%% (Verified: no ETS / mnesia / persistent_term / application:get_env on this
%% call path -- the only side effect reachable is rabbit_oauth2_scope's own
%% ?LOG_WARNING on a rejected/invalid pattern, which carries no token or claim
%% content. The impure resource-server *resolution* functions that DO read
%% app-env are bypassed by constructing the record via new_resource_server/1.)
%%
%% scope_pattern_syntax: both `wildcard' (the default, used by IAM and the
%% documented OAuth recipe) and `regex' are accepted. The regex path carries NO
%% new ReDoS surface: rabbit_oauth2_scope runs the SAME bounded matcher the live
%% broker runs on every OAuth-authenticated connection -- a 2048-byte pattern
%% cap, a rejected-construct denylist (inline modifiers / callouts / comments /
%% control verbs), and hard PCRE bounds (match_limit=10000,
%% match_limit_recursion=1000) that cap backtracking regardless of pattern. A
%% crafted pattern cannot exceed what the broker already permits at auth time,
%% and the concurrency semaphore bounds how many run at once. Allowing regex
%% keeps validation parity with a broker configured for regex scopes.
-module(aws_auth_validate_oauth_authz).

-export([maybe_check/2, available/0, backend_loaded/0, evaluator_compiled_in/0]).

%% The evaluation path builds the broker's #resource_server{} record, so it needs
%% that record's shape (from oauth2.hrl). But rabbitmq_auth_backend_oauth2 is a
%% RUNTIME soft dependency, not a build DEPS: the plugin is compiled against a
%% range of broker series, and the oauth2 backend's shape is NOT stable across
%% them -- older series (e.g. 3.13.x) ship no oauth2.hrl at all, and newer ones
%% add record fields (scope_pattern_syntax). Hard-including the header would make
%% the whole plugin fail to COMPILE on any series that lacks it, defeating the
%% soft-dependency design (the feature is meant to be merely UNAVAILABLE there,
%% via available/0, not a build break).
%%
%% So the header include and the record-using evaluation code are guarded by
%% -ifdef(HAVE_OAUTH2_RESOURCE_SERVER), a macro the Makefile defines only when
%% $(DEPS_DIR)/rabbitmq_auth_backend_oauth2/include/oauth2.hrl actually exists at
%% build time. When it is absent, the module still compiles; available/0 returns
%% false and maybe_check/2 reports config_conflict (authz unavailable). When it
%% is present, the record-typed evaluator is compiled in. We do NOT reference
%% fields that vary across supported series (scope_pattern_syntax): the syntax is
%% threaded as a plain argument to the scope functions instead (see evaluate/3).
-include_lib("rabbit_common/include/resource.hrl").
-ifdef(HAVE_OAUTH2_RESOURCE_SERVER).
-include_lib("rabbitmq_auth_backend_oauth2/include/oauth2.hrl").
-endif.

-define(OAUTH2_MOD, rabbit_auth_backend_oauth2).
-define(SCOPE_MOD, rabbit_oauth2_scope).
-define(RS_MOD, rabbit_oauth2_resource_server).
-define(SCHEMA_MOD, rabbit_oauth2_schema).

%% Fixed reasons. NOTE on granularity: the fixed-category rule guards against
%% leaking BROKER INFRASTRUCTURE or an SSRF target (hostnames, IPs, raw
%% network/LDAP errors) that an attacker does not already know. The authz
%% failure reasons below are categorically different: they describe only the
%% CALLER'S OWN token and the CALLER'S OWN supplied authorization config (both
%% arrived in this same request), never broker infra or a network target -- the
%% same basis on which token_expired/token_invalid were already split out. So we
%% keep a single error CATEGORY (authz_unverified) for the audit-log/response
%% contract but differentiate the human-readable message, which is what actually
%% lets a customer self-diagnose the "my token is broken (it isn't)" case. The
%% messages carry NO scope values or claim content -- they name the failing
%% STAGE, not the data -- so no token material is echoed, and the message is not
%% audit-logged regardless.
-define(REASON_AUTHZ_UNAVAILABLE, <<
    "authorization evaluation requires the rabbitmq_auth_backend_oauth2 plugin "
    "to be loaded on this broker; it is not"
>>).
%% Authentic token, but nothing survived scope_prefix / resource_server_id
%% filtering -- the #1 cause of a spurious "my token is broken" ticket (a
%% scope_prefix or scope_aliases typo). Naming this stage points the customer
%% straight at their prefix/alias config.
-define(REASON_AUTHZ_NO_EFFECTIVE_SCOPES, <<
    "the access_token carries no scopes for this resource_server after "
    "scope_prefix / resource_server_id filtering; check scope_prefix, "
    "resource_server_id, additional_scopes_key, and scope_aliases"
>>).
%% Token has effective scopes, but none grant the requested permission on the
%% requested resource -- the genuine authorization mismatch.
-define(REASON_AUTHZ_NO_MATCH, <<
    "the access_token has scopes for this resource_server, but none grant the "
    "requested permission on the requested vhost/resource"
>>).
%% normalize_token_scope/2's only explicit error: the token exceeded the
%% broker's maximum scope count. Previously swallowed as a generic denial, which
%% wrongly told the customer to fix their permission mapping.
-define(REASON_AUTHZ_TOO_MANY_SCOPES, <<
    "the access_token carries more scopes than the broker will process "
    "(auth_oauth2 maximum); the live broker would also reject it"
>>).
%% Catch-all for any future normalize_token_scope/2 error we do not yet model,
%% so an upstream change cannot crash or be misclassified.
-define(REASON_AUTHZ_UNPROCESSABLE, <<
    "the access_token's scopes could not be processed under the supplied "
    "authorization config"
>>).
-define(REASON_AUTHZ_BAD_INPUT, <<
    "authz_check must be an object with permission and resource fields"
>>).

%% True when the broker has the oauth2 decision functions available on the code
%% path. Used both to gate the request and (in aws_auth_validate_oauth) to decide
%% whether to offer authz mode at all.
%%
%% available/0 is the strong property: the oauth2 backend is loaded AND exposes
%% the exact arity-4 scope API this layer calls. backend_loaded/0 is the weaker
%% property: rabbit_auth_backend_oauth2 is loaded at all, regardless of its API
%% version. The two differ precisely on the portability regression Luke flagged:
%% a broker series that ships the oauth2 backend but predates resource_access/4.
%% Tests use the pair to tell a legitimate skip (backend absent entirely) from a
%% hard failure (backend present but API too old -- must not pass silently).
%%
%% We use code:ensure_loaded/1 rather than a bare erlang:function_exported/3:
%% the latter returns false for a module whose beam is on the path but has not
%% yet been loaded into the VM, which would make availability depend on load
%% order (a request could see the oauth2 backend as "absent" simply because
%% nothing had called it yet this boot). ensure_loaded triggers the load if the
%% beam is present, so this reflects "is the backend installed" -- the property
%% we actually want -- and stays false only when the backend genuinely is not
%% deployed on this broker.
%%
%% The probe (3x code:ensure_loaded + 4x function_exported) runs on every request.
%% It is deliberately not memoized: once the modules are loaded each check is a
%% cheap in-VM hash lookup against the active code-index tables -- code:ensure_loaded
%% short-circuits on erlang:module_loaded (no code-server round-trip), and
%% function_exported/3 looks up the export table -- the endpoint is a rarely-hit,
%% semaphore-bounded triage aid, and a stateless probe stays correct in both
%% directions: it picks up a backend enabled after boot and, equally, reports a
%% backend disabled at runtime (whose modules rabbit_plugins purges), where a
%% cached `true' would leave available/0 stale and misreport the request.
-spec available() -> boolean().
-ifdef(HAVE_OAUTH2_RESOURCE_SERVER).
available() ->
    module_ready(?SCOPE_MOD) andalso
        module_ready(?OAUTH2_MOD) andalso
        module_ready(?RS_MOD) andalso
        erlang:function_exported(?SCOPE_MOD, resource_access, 4) andalso
        erlang:function_exported(?OAUTH2_MOD, get_expanded_scopes, 3) andalso
        erlang:function_exported(?OAUTH2_MOD, normalize_token_scope, 2) andalso
        erlang:function_exported(?RS_MOD, new_resource_server, 1).

module_ready(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} -> true;
        _ -> false
    end.

%% Weaker probe: is the oauth2 backend module loaded at all, independent of its
%% API version? Used only by tests to distinguish "backend genuinely absent"
%% (legitimate skip) from "backend present but too old" (hard failure). Not
%% memoized -- it is off the request path.
-spec backend_loaded() -> boolean().
backend_loaded() ->
    module_ready(?OAUTH2_MOD).

%% True when the record-typed evaluator was compiled into this build (i.e. the
%% Makefile saw the arity-4 scope API in oauth2.hrl and defined the macro). This
%% is the COMPILE-TIME property; available/0 is the RUNTIME one. Tests use this
%% to scope the "present-but-unusable" hard failure: only when the evaluator was
%% compiled in but available/0 is still false at runtime is there a genuine
%% portability regression to fail on. When the evaluator was NOT compiled in
%% (a pre-floor broker series), a supplied authz_check is legitimately
%% unavailable, so tests skip rather than fail.
-spec evaluator_compiled_in() -> boolean().
evaluator_compiled_in() ->
    true.
-else.
%% Built against a broker series with no oauth2 resource_server record: the
%% evaluator cannot be compiled in, so authz is unconditionally unavailable.
available() ->
    false.

%% Even without the record at build time, the runtime backend module may exist;
%% report its load state so tests can still make the absent-vs-present-but-old
%% distinction on this build branch.
-spec backend_loaded() -> boolean().
backend_loaded() ->
    case code:ensure_loaded(?OAUTH2_MOD) of
        {module, ?OAUTH2_MOD} -> true;
        _ -> false
    end.

%% Evaluator was not compiled in on this build branch (pre-floor series).
-spec evaluator_compiled_in() -> boolean().
evaluator_compiled_in() ->
    false.
-endif.

%% Optional layer: when the request carried an `authz_check' block, evaluate it
%% against the verified token's claims. Params is the parsed request accumulator
%% from aws_auth_validate_oauth; Claims is the decoded, already-signature-and-
%% expiry-verified JWT claims map.
%%
%%   * no authz_check supplied            -> ok (layer is a no-op)
%%   * authz_check supplied, oauth2 absent -> {error, config_conflict, ...}
%%   * granted                            -> ok
%%   * denied / unprocessable             -> {error, authz_unverified, Msg}
%%       where Msg distinguishes: no effective scopes after prefix/alias
%%       filtering, scopes present but none match, or too-many-scopes. The
%%       CATEGORY stays authz_unverified in every failing case; only the message
%%       differs (see the reason macros above for the granularity rationale).
-spec maybe_check(map(), map()) -> aws_auth_validate_backend:result().
maybe_check(#{authz_check := none}, _Claims) ->
    ok;
maybe_check(#{authz_check := Check} = Params, Claims) when is_map(Check) ->
    maybe_check_available(Params, Check, Claims);
maybe_check(_Params, _Claims) ->
    %% No authz_check slot at all (older parse path) -> no-op.
    ok.

%%--------------------------------------------------------------------
%% Evaluation (compiled in only when the oauth2 resource_server record is
%% available at build time; see HAVE_OAUTH2_RESOURCE_SERVER at the top).
%%--------------------------------------------------------------------

-ifdef(HAVE_OAUTH2_RESOURCE_SERVER).

maybe_check_available(Params, Check, Claims) ->
    case available() of
        false ->
            {error, config_conflict, ?REASON_AUTHZ_UNAVAILABLE};
        true ->
            evaluate(Params, Check, Claims)
    end.

%% COUPLING NOTE (see the parity-scope discussion in the module header): this
%% function replays the broker's authorization decision as an explicit chain of
%% its internal stages -- new_resource_server/1 -> normalize_token_scope/2 ->
%% get_expanded_scopes/3 -> resource_access/4. The matcher itself cannot drift
%% (it IS the broker's code), but this SEQUENCING can: if a future oauth2 series
%% inserts a normalization stage between these calls, or moves work into a
%% higher-level entry point, this endpoint would under-report effective scopes
%% (the safe direction -- it never over-grants -- but a parity gap). If the
%% backend ever exposes a single pure "would this token be granted X" entry
%% point, prefer collapsing this chain onto it. Until then, any new stage the
%% backend adds must be mirrored here.
evaluate(Params, Check, Claims) ->
    %% Build the broker's #resource_server{} from CUSTOMER-SUPPLIED config so the
    %% alias / additional-scopes-key / prefix logic matches what their broker
    %% would do. new_resource_server/1 seeds the defaults; we overlay the
    %% supplied fields.
    %% resource_server_id is guaranteed present and a non-empty binary here:
    %% aws_auth_validate_oauth:parse_authz_config rejects an authz_check request
    %% that lacks it (input_invalid) in the pure phase, so this layer never
    %% builds new_resource_server(undefined) (which would crash on the
    %% iolist_to_binary([undefined, <<".">>]) scope_prefix seed and be misreported
    %% as a connection failure). We read it with no default so a future caller that
    %% breaks that invariant fails loudly rather than silently guessing an id.
    ResourceServerId = maps:get(resource_server_id, Params),
    RS0 = ?RS_MOD:new_resource_server(ResourceServerId),
    %% Only overlay fields that exist in the #resource_server{} record across all
    %% supported broker series. `scope_pattern_syntax' is deliberately NOT set on
    %% the record: it is absent from older oauth2 backends' record definition, so
    %% a compile-time field reference here would fail to build against those
    %% series (this module is compiled against whatever oauth2.hrl the broker
    %% ships). It is not needed on the record anyway -- normalize_token_scope/2
    %% does not read it (the scope_prefix filter is a literal prefix match, not a
    %% pattern), and the ONLY consumers of the syntax (get_expanded_scopes/3 and
    %% rabbit_oauth2_scope:resource_access/4) take it as an explicit argument,
    %% which we thread through as `Syntax' below. So regex parity is preserved on
    %% brokers that support it without a record-shape dependency.
    RS = RS0#resource_server{
        scope_aliases = maps:get(scope_aliases, Params, undefined),
        additional_scopes_key = tokenize_additional_scopes_key(
            maps:get(additional_scopes_key, Params, undefined)
        ),
        scope_prefix = maps:get(scope_prefix, Params, RS0#resource_server.scope_prefix)
    },
    Syntax = maps:get(scope_pattern_syntax, Params, wildcard),
    VHost = maps:get(vhost, Check, <<"/">>),
    Name = maps:get(resource, Check, undefined),
    Permission = maps:get(permission, Check, undefined),
    case {Name, Permission} of
        {N, P} when is_binary(N), is_binary(P) ->
            %% 1. normalize_token_scope applies aliases + additional_scopes_key +
            %%    prefix filter (the IAM `scope_aliases' expansion), storing the
            %%    result in the token's `scope' field. Its errors are no longer
            %%    swallowed as a generic denial (which wrongly implied a
            %%    permission-mapping problem): they are mapped to their own
            %%    message so the customer sees the real cause.
            case ?OAUTH2_MOD:normalize_token_scope(RS, Claims) of
                {ok, Normalized} ->
                    Resource = #resource{
                        virtual_host = VHost,
                        kind = queue,
                        name = Name
                    },
                    %% 2. expand {vhost} etc. placeholders, then 3. match.
                    Scopes = ?OAUTH2_MOD:get_expanded_scopes(Normalized, Resource, Syntax),
                    decide(Scopes, Resource, to_permission_atom(P), Syntax);
                {error, too_many_scopes} ->
                    {error, authz_unverified, ?REASON_AUTHZ_TOO_MANY_SCOPES};
                {error, _Other} ->
                    %% Any future normalize error we do not yet model: fixed,
                    %% generic message (never the raw reason) so an upstream
                    %% change cannot crash or misclassify.
                    {error, authz_unverified, ?REASON_AUTHZ_UNPROCESSABLE}
            end;
        _ ->
            {error, input_invalid, ?REASON_AUTHZ_BAD_INPUT}
    end.

%% Distinguish "authentic token, but no scopes survived prefix/alias filtering"
%% (the prefix/alias-typo footgun) from "has scopes, but none grant this
%% permission" (a genuine mapping mismatch). Both stay in the authz_unverified
%% category; only the message differs. No scope value is echoed -- the messages
%% name the failing stage, not the data.
decide([], _Resource, _PermAtom, _Syntax) ->
    {error, authz_unverified, ?REASON_AUTHZ_NO_EFFECTIVE_SCOPES};
decide(Scopes, Resource, PermAtom, Syntax) ->
    case ?SCOPE_MOD:resource_access(Resource, PermAtom, Scopes, Syntax) of
        true -> ok;
        false -> {error, authz_unverified, ?REASON_AUTHZ_NO_MATCH}
    end.

%% Map the supplied permission string to the broker's permission atom. The
%% permission is already allowlisted to exactly these three verbs in the pure
%% phase (aws_auth_validate_oauth:parse_authz_check/3), so this clause set is
%% total over every value that can reach it. It has no catch-all: an unexpected
%% value must fail loudly here rather than be silently mapped to a non-matching
%% atom that resource_access/4 would report as a spurious "none grant".
to_permission_atom(<<"configure">>) -> configure;
to_permission_atom(<<"write">>) -> write;
to_permission_atom(<<"read">>) -> read.

%% Interpret the customer-supplied `additional_scopes_key' exactly as the broker's
%% own config path does. The caller sends a plain string (the request contract is
%% unchanged); its dots are ambiguous: an UNESCAPED `.' is a nesting separator,
%% while an ESCAPED `\.' is a literal dot in a flat claim name -- the case that
%% matters for STS/OIDC keys like `https://sts\.amazonaws\.com/.request_tags.scope'
%% (rabbitmq/rabbitmq-server #16947).
%%
%% When the broker exposes rabbit_oauth2_schema:tokenize_additional_scopes_key/1
%% (the post-#16947 series), we call it so the endpoint splits the key IDENTICALLY
%% to `auth_oauth2.additional_scopes_key' in rabbitmq.conf: it returns [[binary()]]
%% (a list of paths, each a list of segments) with `\.' kept literal, and the
%% record's list-form `additional_scopes_key' clause consumes it WITHOUT re-splitting.
%% Parity is preserved in both directions -- an unescaped key still tokenizes to the
%% same nested path split_path/1 produced, so it still fails to resolve a flat dotted
%% claim (the pinned #16947 behavior), and an escaped key now resolves.
%%
%% Fallback: on a pre-#16947 broker series the tokenizer is absent. We pass the raw
%% binary through unchanged, so normalize_token_scope/2 takes its own binary clause
%% (split_path/1) -- the endpoint behaves exactly as it did before this change. The
%% feature stays available on old brokers; it simply lacks escaped-dot support there,
%% matching what those brokers themselves can do.
tokenize_additional_scopes_key(undefined) ->
    undefined;
tokenize_additional_scopes_key(Key) when is_binary(Key) ->
    %% module_ready/1 (code:ensure_loaded) before function_exported/3: a bare
    %% function_exported returns false for a beam that is on the path but not yet
    %% loaded, which would make escaped-dot support depend on load order (the same
    %% trap available/0 documents). ensure_loaded triggers the load if the beam is
    %% present, so this reflects "is the tokenizer installed", not "has it been
    %% called yet this boot".
    case
        module_ready(?SCHEMA_MOD) andalso
            erlang:function_exported(?SCHEMA_MOD, tokenize_additional_scopes_key, 1)
    of
        true -> ?SCHEMA_MOD:tokenize_additional_scopes_key(Key);
        false -> Key
    end.

-else.

%% No oauth2 resource_server record at build time: authz cannot be evaluated, so
%% a supplied authz_check is a config conflict (the feature is unavailable on
%% this broker series). available/0 already returns false in this branch.
maybe_check_available(_Params, _Check, _Claims) ->
    {error, config_conflict, ?REASON_AUTHZ_UNAVAILABLE}.

-endif.
