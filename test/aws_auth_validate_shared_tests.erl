%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Cross-backend equivalence tests for the shared validation modules
%% (aws_auth_validate_ssl / aws_auth_validate_net / aws_auth_validate_httpc).
%%
%% These lock in the property that the http and oauth backends behave
%% IDENTICALLY where they share code -- the whole point of the extraction. If a
%% future change makes one backend diverge from the shared helper (the drift
%% these modules exist to prevent), one of these fails.
-module(aws_auth_validate_shared_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% aws_auth_validate_ssl: value translators (backend-independent)
%%--------------------------------------------------------------------

to_verify_test() ->
    ?assertEqual(verify_peer, aws_auth_validate_ssl:to_verify(<<"verify_peer">>)),
    ?assertEqual(verify_none, aws_auth_validate_ssl:to_verify(<<"verify_none">>)).

to_versions_test() ->
    ?assertEqual(
        ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', tlsv1],
        aws_auth_validate_ssl:to_versions([
            <<"tlsv1.3">>, <<"tlsv1.2">>, <<"tlsv1.1">>, <<"tlsv1">>
        ])
    ).

%% decode_pem_cacerts/1 must return raw DER (or skip), never pem_entry_decode
%% records -- the cacerts-DER contract the three backends depend on.
decode_pem_cacerts_skip_on_non_pem_test() ->
    ?assertEqual(skip, aws_auth_validate_ssl:decode_pem_cacerts(<<"not-a-pem">>)).

%% resolve_arn/2 fails closed on the `none' credential sentinel: it must NOT call
%% out to AWS. (aws_arn_util is not even loaded here; a call would error.)
resolve_arn_fails_closed_on_none_test() ->
    ?assertEqual(
        {error, no_credentials_state}, aws_auth_validate_ssl:resolve_arn(<<"arn:x">>, none)
    ).

%% connection_timeout_ms/1 caps at the supplied max and floors invalid values to
%% the default -- shared by all three backends.
connection_timeout_bounds_test() ->
    application:unset_env(aws, auth_validation_connection_timeout_ms),
    ?assertEqual(
        5000, aws_auth_validate_ssl:connection_timeout_ms(#{default => 5000, max => 60000})
    ),
    application:set_env(aws, auth_validation_connection_timeout_ms, 7000),
    ?assertEqual(
        7000, aws_auth_validate_ssl:connection_timeout_ms(#{default => 5000, max => 60000})
    ),
    %% Over the cap -> default.
    application:set_env(aws, auth_validation_connection_timeout_ms, 999999),
    ?assertEqual(
        5000, aws_auth_validate_ssl:connection_timeout_ms(#{default => 5000, max => 60000})
    ),
    application:unset_env(aws, auth_validation_connection_timeout_ms).

%%--------------------------------------------------------------------
%% aws_auth_validate_ssl: hostname-check on verify_peer paths
%%--------------------------------------------------------------------
%%
%% Regression guard: every verify_peer path must carry the https match_fun so a
%% multi-label host under a single-label wildcard cert (e.g. Cognito's
%% foo.auth.<region>.amazoncognito.com vs *.auth.<region>.amazoncognito.com)
%% verifies the way curl/openssl/browsers do. Without it the probe reported a
%% spurious tls_failed for valid IdP endpoints. Applies to http + oauth (both
%% call apply_verify_default with a cacerts anchor present).

%% A cacerts anchor is present, so a bare verify_peer must gain the https
%% customize_hostname_check match_fun.
apply_verify_default_adds_hostname_check_test() ->
    Opts = [{verify, verify_peer}, {cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, true),
    ?assert(has_https_match_fun(Out)).

%% When verify is ABSENT and an anchor exists, we default to verify_peer AND add
%% the match_fun (the http/oauth default probe path).
apply_verify_default_absent_verify_adds_hostname_check_test() ->
    Opts = [{cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, false),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Out)),
    ?assert(has_https_match_fun(Out)).

%% An explicit verify_none is left untouched -- no hostname check injected.
apply_verify_default_verify_none_untouched_test() ->
    Opts = [{verify, verify_none}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, true),
    ?assertNot(lists:keymember(customize_hostname_check, 1, Out)).

%% A caller-supplied customize_hostname_check is not overridden.
apply_verify_default_respects_caller_hostname_check_test() ->
    Custom = [{match_fun, fun(_, _) -> true end}],
    Opts = [{verify, verify_peer}, {cacerts, [<<"der">>]}, {customize_hostname_check, Custom}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, true),
    ?assertEqual(Custom, proplists:get_value(customize_hostname_check, Out)).

has_https_match_fun(Opts) ->
    case lists:keyfind(customize_hostname_check, 1, Opts) of
        {customize_hostname_check, Cfg} -> is_function(proplists:get_value(match_fun, Cfg));
        false -> false
    end.

%%--------------------------------------------------------------------
%% aws_auth_validate_ssl: apply_verify_default/3 hostname-check mode
%%--------------------------------------------------------------------
%%
%% The /3 form lets a backend model the broker's hostname_verification setting.
%% `wildcard' applies the RFC 6125 https match fun (http/oauth default, and the
%% /2 form); `strict' leaves OTP's exact matching in place -- the LDAP default,
%% mirroring rabbit_auth_backend_ldap which only applies the https fun when
%% auth_ldap.ssl_options.hostname_verification = wildcard.

%% /2 delegates to /3 with wildcard: a bare verify_peer gains the match fun.
apply_verify_default_2_defaults_to_wildcard_test() ->
    Opts = [{verify, verify_peer}, {cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, true),
    ?assert(has_https_match_fun(Out)).

%% /3 wildcard mode matches the /2 behaviour.
apply_verify_default_3_wildcard_adds_hostname_check_test() ->
    Opts = [{verify, verify_peer}, {cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, true, wildcard),
    ?assert(has_https_match_fun(Out)).

%% /3 strict mode MUST NOT inject the https match fun on an explicit verify_peer:
%% OTP's default (exact) matching stays, matching the broker's default LDAP path.
apply_verify_default_3_strict_no_hostname_check_test() ->
    Opts = [{verify, verify_peer}, {cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, true, strict),
    ?assertNot(lists:keymember(customize_hostname_check, 1, Out)),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Out)).

%% /3 strict mode on the ABSENT-verify path: defaults to verify_peer (anchor
%% present) but still adds no match fun.
apply_verify_default_3_strict_absent_verify_no_hostname_check_test() ->
    Opts = [{cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts, false, strict),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Out)),
    ?assertNot(lists:keymember(customize_hostname_check, 1, Out)).

%% strict mode does not alter the no-anchor policy: an EXPLICIT verify_peer with
%% no anchor still fails loud (the hostname-check mode is orthogonal to it).
apply_verify_default_3_strict_explicit_no_anchor_still_fails_test_() ->
    {setup,
        fun() ->
            ok = meck:new(public_key, [unstick, passthrough]),
            meck:expect(public_key, cacerts_get, fun() -> [] end),
            ok
        end,
        fun(_) -> meck:unload(public_key) end, fun() ->
            Result = aws_auth_validate_ssl:apply_verify_default(
                [{verify, verify_peer}], true, strict
            ),
            ?assertMatch({error, tls_failed, _}, Result)
        end}.

%%--------------------------------------------------------------------
%% aws_auth_validate_ssl: hostname_check_mode/1. Shared by http/oauth/ldap to
%% resolve the modeled `hostname_verification' ssl_options input into the /3
%% mode. Unset (or the broker's `none') = strict; only `wildcard' opts into the
%% RFC 6125 https match fun -- mirroring the broker's strict-by-default gating
%% across all three backends.
%%--------------------------------------------------------------------

hostname_check_mode_unset_is_strict_test() ->
    ?assertEqual(strict, aws_auth_validate_ssl:hostname_check_mode(#{})).

hostname_check_mode_none_is_strict_test() ->
    ?assertEqual(
        strict,
        aws_auth_validate_ssl:hostname_check_mode(#{<<"hostname_verification">> => <<"none">>})
    ).

hostname_check_mode_wildcard_test() ->
    ?assertEqual(
        wildcard,
        aws_auth_validate_ssl:hostname_check_mode(#{<<"hostname_verification">> => <<"wildcard">>})
    ).

%%--------------------------------------------------------------------
%% aws_auth_validate_net: embedded_v4/1 shared unwrapper. classify_ip/2 (shared
%% by all three backends) uses this helper, so its decoding must cover every
%% v4-carrying v6 notation and leave native v6 (::, ::1) unwrapped.
%%--------------------------------------------------------------------

embedded_v4_ipv4_mapped_test() ->
    ?assertEqual(
        {ok, {169, 254, 169, 254}},
        aws_auth_validate_net:embedded_v4({0, 0, 0, 0, 0, 16#ffff, 16#a9fe, 16#a9fe})
    ).

embedded_v4_ipv4_compatible_test() ->
    ?assertEqual(
        {ok, {8, 8, 8, 8}},
        aws_auth_validate_net:embedded_v4({0, 0, 0, 0, 0, 0, 16#0808, 16#0808})
    ).

embedded_v4_nat64_test() ->
    ?assertEqual(
        {ok, {169, 254, 169, 254}},
        aws_auth_validate_net:embedded_v4({16#64, 16#ff9b, 0, 0, 0, 0, 16#a9fe, 16#a9fe})
    ).

embedded_v4_6to4_test() ->
    ?assertEqual(
        {ok, {169, 254, 169, 254}},
        aws_auth_validate_net:embedded_v4({16#2002, 16#a9fe, 16#a9fe, 0, 0, 0, 0, 0})
    ).

%% :: and ::1 are NOT unwrapped -- they must classify as v6 (::1 is v6 loopback,
%% not v4 0.0.0.1), so the unwrapper returns `none' for them.
embedded_v4_unspecified_and_loopback_not_unwrapped_test() ->
    ?assertEqual(none, aws_auth_validate_net:embedded_v4({0, 0, 0, 0, 0, 0, 0, 0})),
    ?assertEqual(none, aws_auth_validate_net:embedded_v4({0, 0, 0, 0, 0, 0, 0, 1})).

%% A plain global-unicast v6 carries no embedded v4.
embedded_v4_plain_v6_none_test() ->
    ?assertEqual(none, aws_auth_validate_net:embedded_v4({2600, 16#1f18, 0, 0, 0, 0, 0, 1})).

%%--------------------------------------------------------------------
%% aws_auth_validate_net: the SSRF classifier is identical for the http and
%% oauth policies (same infra denylist). Assert both backends' TEST wrappers
%% agree with each other and with the shared module for the full v4/v6 matrix.
%%--------------------------------------------------------------------

%% The infra addresses both backends must DENY, and the public address both must
%% ALLOW. classify_ip/1 on each backend delegates to the shared net module with
%% that backend's policy; the denylists are identical, so the verdicts must match.
ssrf_classify_ip_parity_test() ->
    application:unset_env(aws, auth_validation_allow_private_networks),
    Denied = [
        {127, 0, 0, 1},
        {169, 254, 169, 254},
        {0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 1},
        {16#fe80, 0, 0, 0, 0, 0, 0, 1},
        {16#fd00, 16#0ec2, 0, 0, 0, 0, 0, 16#0254},
        %% v6-encoded IMDS (v4-mapped) must also be denied.
        {0, 0, 0, 0, 0, 16#ffff, 16#a9fe, 16#a9fe}
    ],
    Allowed = [
        {8, 8, 8, 8},
        {10, 0, 0, 5},
        {172, 16, 0, 1},
        {192, 168, 1, 1},
        {2600, 16#1f18, 0, 0, 0, 0, 0, 1}
    ],
    [
        ?assertEqual(
            deny,
            aws_auth_validate_http:classify_ip(IP),
            {http_should_deny, IP}
        )
     || IP <- Denied
    ],
    [
        ?assertEqual(
            deny,
            aws_auth_validate_oauth:classify_ip(IP),
            {oauth_should_deny, IP}
        )
     || IP <- Denied
    ],
    [
        ?assertEqual(
            allow,
            aws_auth_validate_http:classify_ip(IP),
            {http_should_allow, IP}
        )
     || IP <- Allowed
    ],
    [
        ?assertEqual(
            allow,
            aws_auth_validate_oauth:classify_ip(IP),
            {oauth_should_allow, IP}
        )
     || IP <- Allowed
    ],
    %% Explicit http-vs-oauth agreement on every case.
    [
        ?assertEqual(
            aws_auth_validate_http:classify_ip(IP),
            aws_auth_validate_oauth:classify_ip(IP),
            {parity_mismatch, IP}
        )
     || IP <- Denied ++ Allowed
    ].

%% The loopback-relax flag must behave identically for both backends: loopback
%% flips allow, IMDS stays denied.
ssrf_loopback_flag_parity_test() ->
    application:set_env(aws, auth_validation_allow_private_networks, true),
    try
        ?assertEqual(allow, aws_auth_validate_http:classify_ip({127, 0, 0, 1})),
        ?assertEqual(allow, aws_auth_validate_oauth:classify_ip({127, 0, 0, 1})),
        %% IMDS is NOT relaxed for either.
        ?assertEqual(deny, aws_auth_validate_http:classify_ip({169, 254, 169, 254})),
        ?assertEqual(deny, aws_auth_validate_oauth:classify_ip({169, 254, 169, 254}))
    after
        application:unset_env(aws, auth_validation_allow_private_networks)
    end.

%% CIDR membership math is backend-independent; sanity-check via both wrappers.
in_cidr_parity_test() ->
    Cidr = {{169, 254, 0, 0}, 16},
    ?assertEqual(
        aws_auth_validate_http:in_cidr({169, 254, 169, 254}, Cidr),
        aws_auth_validate_oauth:in_cidr({169, 254, 169, 254}, Cidr)
    ),
    ?assert(aws_auth_validate_http:in_cidr({169, 254, 169, 254}, Cidr)),
    ?assertNot(aws_auth_validate_http:in_cidr({8, 8, 8, 8}, Cidr)).

%%--------------------------------------------------------------------
%% Scheme-allowlist divergence is INTENTIONAL: oauth is https-only, http allows
%% both. Lock that difference in so a refactor cannot accidentally unify it.
%%--------------------------------------------------------------------

scheme_policy_divergence_test() ->
    HttpUrl = #{scheme => "http", host => "example.com"},
    %% http backend allows http://
    ?assertEqual(ok, aws_auth_validate_http:url_allowed(HttpUrl)),
    %% oauth backend rejects http:// (https-only)
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:url_allowed(HttpUrl)),
    %% both allow https:// to a public host
    HttpsUrl = #{scheme => "https", host => "example.com"},
    ?assertEqual(ok, aws_auth_validate_http:url_allowed(HttpsUrl)),
    ?assertEqual(ok, aws_auth_validate_oauth:url_allowed(HttpsUrl)).

%%--------------------------------------------------------------------
%% aws_auth_validate_ssl:apply_verify_default/2: verify_peer is never SILENTLY
%% downgraded to verify_none. An explicitly requested verify_peer with no trust
%% anchor must FAIL rather than pass, because silently downgrading would report a
%% config as certificate-verified when it is not. These pin every branch of the
%% policy directly, deterministically, without needing a live TLS server (the
%% http/oauth/tls SUITEs cover it end to end but skip when no server is present).
%%
%% Branch matrix (VerifyExplicit x anchor-present):
%%   explicit verify_peer + no anchor   -> {error, tls_failed, _}   (FAIL, never downgrade)
%%   explicit verify_peer + anchor      -> {ok, _} unchanged
%%   defaulted verify_peer + no anchor  -> {ok, _} downgraded to verify_none
%%   explicit verify_none               -> {ok, _} untouched
%%   absent verify + anchor             -> {ok, _} verify_peer added
%%   absent verify + no anchor          -> {ok, _} verify left unset
%%--------------------------------------------------------------------

%% Run Fun/0 (an eunit instantiator returning a test list) with the OS trust
%% store forced empty, so the "no anchor" branches are deterministic regardless
%% of the host's CA store. os_cacerts/0 wraps public_key:cacerts_get/0 in a
%% try/catch, so returning [] here means "no trust anchor available".
with_empty_os_store(Fun) ->
    {setup,
        fun() ->
            ok = meck:new(public_key, [unstick, passthrough]),
            meck:expect(public_key, cacerts_get, fun() -> [] end),
            ok
        end,
        fun(_) -> meck:unload(public_key) end, Fun}.

%% LOAD-BEARING assertion: an EXPLICIT verify_peer with no trust anchor MUST fail
%% closed with tls_failed -- it must never be silently downgraded to verify_none.
apply_verify_default_explicit_verify_peer_without_anchor_fails_test_() ->
    with_empty_os_store(fun() ->
        Result = aws_auth_validate_ssl:apply_verify_default([{verify, verify_peer}], true),
        [?_assertMatch({error, tls_failed, _}, Result)]
    end).

%% A DEFAULTED verify_peer (not explicitly requested) with no anchor is the ONLY
%% case allowed to downgrade -- to verify_none -- so a plain reachability probe
%% still works without a CA store. Explicitness (previous test) is what forbids
%% the downgrade.
apply_verify_default_defaulted_verify_peer_without_anchor_downgrades_test_() ->
    with_empty_os_store(fun() ->
        {ok, Opts} = aws_auth_validate_ssl:apply_verify_default([{verify, verify_peer}], false),
        [?_assertEqual({verify, verify_none}, lists:keyfind(verify, 1, Opts))]
    end).

%% Absent verify + no anchor: leave verify unset (do not fabricate verify_peer
%% with nothing to verify against).
apply_verify_default_absent_verify_without_anchor_stays_unset_test_() ->
    with_empty_os_store(fun() ->
        {ok, Opts} = aws_auth_validate_ssl:apply_verify_default([], false),
        [?_assertNot(lists:keymember(verify, 1, Opts))]
    end).

%% Explicit verify_peer WITH a supplied trust anchor (cacerts) is accepted --
%% the presence of cacerts short-circuits before the OS store, so no mock is
%% needed. The verify and cacerts opts are preserved and the https match_fun is
%% added (every verify_peer path carries it -- see has_https_match_fun and the
%% wildcard-cert regression guards above).
apply_verify_default_explicit_verify_peer_with_cacerts_is_ok_test() ->
    Opts0 = [{verify, verify_peer}, {cacerts, [<<"der">>]}],
    {ok, Out} = aws_auth_validate_ssl:apply_verify_default(Opts0, true),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Out)),
    ?assertEqual({cacerts, [<<"der">>]}, lists:keyfind(cacerts, 1, Out)),
    ?assert(has_https_match_fun(Out)).

%% An explicit verify_none is honoured as-is (the operator opted out of
%% verification); apply_verify_default never touches it.
apply_verify_default_explicit_verify_none_untouched_test() ->
    Opts0 = [{verify, verify_none}],
    ?assertEqual({ok, Opts0}, aws_auth_validate_ssl:apply_verify_default(Opts0, false)).

%% Absent verify WITH a trust anchor present: default up to verify_peer (secure
%% by default when we have something to verify against).
apply_verify_default_absent_verify_with_cacerts_adds_verify_peer_test() ->
    Opts0 = [{cacerts, [<<"der">>]}],
    {ok, Opts1} = aws_auth_validate_ssl:apply_verify_default(Opts0, false),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Opts1)),
    ?assert(lists:keymember(cacerts, 1, Opts1)).
