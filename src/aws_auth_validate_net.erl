%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Shared SSRF / DNS-rebinding network guard for the http and oauth validation
%% backends. The ldap backend shares the address classifier (classify_ip/2) and
%% URL parser (parse_url/2) here, but passes its OWN stricter denylist -- it
%% denies ALL private/RFC1918/CGNAT ranges, not just broker infra -- and adds a
%% post-connect peer re-check for eldap's re-resolve. See aws_auth_validate_ldap's
%% is_denied_ip/1.
%%
%% The validator issues outbound requests to customer-supplied URLs from the
%% broker's network position -- a textbook SSRF / confused-deputy surface.
%% Defense is two-phase and IDENTICAL for http and oauth; only the scheme
%% allowlist (oauth is https-only; http allows http+https) and the fixed
%% reason strings differ, so those are parameters (a policy() map):
%%
%%   * PURE phase (url_allowed/2): scheme allowlist + literal-IP classification
%%     against the always-denied infra ranges. Deterministic, no DNS. Catches
%%     https://169.254.169.254/... and https://[::1]/... up front. A hostname
%%     passes here (cannot classify without DNS) and is re-checked below.
%%
%%   * NETWORK phase (resolve_and_pin/2): resolve the host to ALL A/AAAA, deny
%%     if ANY is blocked infra, then connect to the PINNED resolved IP (the
%%     caller preserves Host header + SNI) so httpc cannot re-resolve to a
%%     different address (DNS-rebinding / TOCTOU defense).
%%
%% Policy: the http/oauth denylist denies the broker's own infra only (IMDS,
%% loopback, link-local, unspecified). RFC1918 / unique-local are NOT denied there
%% -- a customer auth server or IdP normally lives in their VPC. (The ldap backend
%% passes classify_ip/2 its OWN stricter denylist that DOES deny RFC1918/CGNAT;
%% the denylist is a per-backend parameter, not a property of this module.) The
%% test-only auth_validation_allow_private_networks flag relaxes ONLY loopback so
%% an integration suite can reach a local stub; IMDS/link-local stay denied.
-module(aws_auth_validate_net).

-export([
    url_allowed/2,
    classify_address/2,
    classify_ip/2,
    embedded_v4/1,
    resolve_and_pin/2,
    pin_url/2,
    parse_url/2,
    in_cidr/2,
    in_any_cidr/2
]).

%% policy() carries the per-backend surface: the infra denylists, the scheme
%% allowlist, and the fixed reason strings for a disallowed target / an
%% unreachable host.
-type policy() :: #{
    denied_v4 := [{tuple(), non_neg_integer()}],
    denied_v6 := [{tuple(), non_neg_integer()}],
    allowed_schemes := [string()],
    reason_not_allowed := binary(),
    reason_connection := binary()
}.

%% The subset classify_ip/2 actually reads: just the infra denylists. A full
%% policy() satisfies it, and a backend that only classifies addresses (ldap)
%% can pass a denylist-only map without dummy scheme/reason keys.
-type cidr_policy() :: #{
    denied_v4 := [{tuple(), non_neg_integer()}],
    denied_v6 := [{tuple(), non_neg_integer()}],
    _ => _
}.

%% Options for parse_url/2: the scheme allowlist (same key name as policy())
%% plus whether a pre-existing query string or #fragment is rejected. Defaults
%% reject both.
-type url_opts() :: #{
    allowed_schemes := [string()],
    query => allow | reject,
    fragment => allow | reject
}.

-export_type([policy/0, cidr_policy/0, url_opts/0]).

%%--------------------------------------------------------------------
%% Pure phase: scheme allowlist + literal-IP classification
%%--------------------------------------------------------------------

%% Pure per-URL policy check: scheme allowlist + literal-IP classification.
%% Hostnames pass here (allow) and are re-checked after DNS in resolve_and_pin/2.
-spec url_allowed(map(), policy()) ->
    ok | {error, input_invalid, binary()}.
url_allowed(#{scheme := Scheme} = Url, Policy) ->
    case lists:member(Scheme, maps:get(allowed_schemes, Policy)) of
        false ->
            {error, input_invalid, maps:get(reason_not_allowed, Policy)};
        true ->
            case classify_address(Url, Policy) of
                allow -> ok;
                deny -> {error, input_invalid, maps:get(reason_not_allowed, Policy)}
            end
    end.

%% Classify a URL's host in the pure phase. A literal IP is classified directly
%% against the infra denylist; a hostname cannot be classified without DNS, so
%% it is allowed here and resolved+classified in the network phase.
-spec classify_address(map(), policy()) -> allow | deny.
classify_address(#{host := Host}, Policy) ->
    case inet:parse_address(Host) of
        {ok, IP} -> classify_ip(IP, Policy);
        {error, _} -> allow
    end.

%% Parse and minimally validate a URL against Opts, returning a normalized map
%% the probe and the SSRF guard share. Always rejects a non-allowlisted scheme,
%% an empty host, userinfo (embedded credentials httpc would turn into an
%% Authorization header), and an out-of-range port (else httpc crashes at
%% request time). A pre-existing query string or #fragment is rejected unless
%% Opts opts in (default reject), because a caller that appends its own path or
%% query would otherwise produce an ambiguous or mangled URL.
-spec parse_url(binary(), url_opts()) -> {ok, map()} | {error, bad_url}.
parse_url(Bin, Opts) when is_binary(Bin) ->
    Str = binary_to_list(Bin),
    case uri_string:parse(Str) of
        #{scheme := Scheme, host := Host} = Parsed when Host =/= [] ->
            case lists:member(Scheme, maps:get(allowed_schemes, Opts)) of
                false ->
                    {error, bad_url};
                true ->
                    Port = maps:get(port, Parsed, undefined),
                    QueryRejected =
                        maps:get(query, Opts, reject) =:= reject andalso
                            maps:is_key(query, Parsed) andalso
                            maps:get(query, Parsed) =/= [],
                    FragmentRejected =
                        maps:get(fragment, Opts, reject) =:= reject andalso
                            maps:is_key(fragment, Parsed) andalso
                            maps:get(fragment, Parsed) =/= [],
                    case Port of
                        P when is_integer(P), (P < 1 orelse P > 65535) ->
                            {error, bad_url};
                        _ when QueryRejected orelse FragmentRejected ->
                            {error, bad_url};
                        _ ->
                            case maps:is_key(userinfo, Parsed) of
                                true -> {error, bad_url};
                                false -> {ok, Parsed#{url_string => Str}}
                            end
                    end
            end;
        _ ->
            {error, bad_url}
    end.

%% classify_ip/2: allow | deny for a parsed IP tuple. Denies the broker-infra
%% ranges only. A v6 address that embeds a v4 address is unwrapped via the shared
%% embedded_v4/1 and re-classified as v4, otherwise the v6 encoding smuggles a
%% denied v4 address (e.g. 169.254.169.254) past the v4 denylist.
-spec classify_ip(tuple(), cidr_policy()) -> allow | deny.
classify_ip({_, _, _, _, _, _, _, _} = V6, Policy) ->
    case embedded_v4(V6) of
        {ok, V4} ->
            classify_ip(V4, Policy);
        none ->
            case in_any_cidr(V6, maps:get(denied_v6, Policy)) of
                true -> classify_denied(V6);
                false -> allow
            end
    end;
classify_ip({_, _, _, _} = V4, Policy) ->
    case in_any_cidr(V4, maps:get(denied_v4, Policy)) of
        true -> classify_denied(V4);
        false -> allow
    end.

%% embedded_v4/1: for the IPv6 notations that carry a v4 address, return
%% {ok, V4Tuple}; otherwise `none'. Policy-independent unwrapping used by
%% classify_ip/2 (which all three backends share) so every classifier decodes
%% the same encodings from one place. Covered: IPv4-mapped
%% ::ffff:a.b.c.d, IPv4-compatible ::a.b.c.d, NAT64 64:ff9b::/96, 6to4 2002::/16.
%% The unspecified :: and loopback ::1 are deliberately NOT unwrapped -- they
%% must be classified as v6 (::1 is v6 loopback, not v4 0.0.0.1).
-spec embedded_v4(tuple()) -> {ok, {byte(), byte(), byte(), byte()}} | none.
embedded_v4({0, 0, 0, 0, 0, 16#ffff, W1, W2}) ->
    {ok, v4_from_words(W1, W2)};
embedded_v4({0, 0, 0, 0, 0, 0, W1, W2}) when {W1, W2} =/= {0, 0}, {W1, W2} =/= {0, 1} ->
    {ok, v4_from_words(W1, W2)};
embedded_v4({16#64, 16#ff9b, 0, 0, 0, 0, W1, W2}) ->
    {ok, v4_from_words(W1, W2)};
embedded_v4({16#2002, W1, W2, _, _, _, _, _}) ->
    {ok, v4_from_words(W1, W2)};
embedded_v4(_) ->
    none.

%% A denied address is normally `deny'. The ONE exception is loopback when
%% auth_validation_allow_private_networks is set (test-only, default false): that
%% lets an integration suite probe a local stub on 127.0.0.1/::1. It relaxes ONLY
%% loopback -- IMDS, link-local, and unspecified stay denied even with the flag
%% on, so it cannot be used to reach instance metadata.
classify_denied(IP) ->
    case is_loopback(IP) andalso allow_private_networks() of
        true -> allow;
        false -> deny
    end.

is_loopback({127, _, _, _}) -> true;
is_loopback({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_loopback(_) -> false.

allow_private_networks() ->
    application:get_env(aws, auth_validation_allow_private_networks, false) =:= true.

%% Two 16-bit words -> a v4 4-tuple {a,b,c,d}.
v4_from_words(W1, W2) ->
    {W1 bsr 8, W1 band 16#ff, W2 bsr 8, W2 band 16#ff}.

in_any_cidr(IP, Cidrs) ->
    lists:any(fun(Cidr) -> in_cidr(IP, Cidr) end, Cidrs).

%% in_cidr/2: is IP within {Network, PrefixBits}? Same address family only.
in_cidr({_, _, _, _} = IP, {{_, _, _, _} = Net, Prefix}) when
    Prefix >= 0, Prefix =< 32
->
    same_prefix(ip_to_int(IP), ip_to_int(Net), 32, Prefix);
in_cidr({_, _, _, _, _, _, _, _} = IP, {{_, _, _, _, _, _, _, _} = Net, Prefix}) when
    Prefix >= 0, Prefix =< 128
->
    same_prefix(ip_to_int(IP), ip_to_int(Net), 128, Prefix);
in_cidr(_IP, _Cidr) ->
    %% Cross-family (e.g. v4 IP vs v6 CIDR): never matches.
    false.

same_prefix(IpInt, NetInt, TotalBits, Prefix) ->
    HostBits = TotalBits - Prefix,
    (IpInt bsr HostBits) =:= (NetInt bsr HostBits).

ip_to_int(Tuple) ->
    BitsPerElem =
        case tuple_size(Tuple) of
            4 -> 8;
            8 -> 16
        end,
    lists:foldl(
        fun(Elem, Acc) -> (Acc bsl BitsPerElem) bor Elem end,
        0,
        tuple_to_list(Tuple)
    ).

%%--------------------------------------------------------------------
%% Network phase: DNS resolve-and-pin (DNS-rebinding / TOCTOU defense)
%%--------------------------------------------------------------------

%% Resolve the URL's host to all addresses, deny if ANY is a blocked infra
%% address, and return the URL rewritten to connect to a single pinned IP plus
%% the original host (for the Host header / SNI). A literal-IP host is pinned to
%% itself (re-classified here defensively). A resolution failure is a
%% reachability fact -> connection_failed.
-spec resolve_and_pin(map(), policy()) ->
    {ok, map(), string()} | {error, input_invalid | connection_failed, binary()}.
resolve_and_pin(#{host := Host} = Url, Policy) ->
    case inet:parse_address(Host) of
        {ok, IP} ->
            case classify_ip(IP, Policy) of
                deny -> {error, input_invalid, maps:get(reason_not_allowed, Policy)};
                allow -> {ok, pin_url(Url, Host), Host}
            end;
        {error, _} ->
            resolve_hostname_and_pin(Url, Host, Policy)
    end.

resolve_hostname_and_pin(Url, Host, Policy) ->
    case resolve_all(Host) of
        [] ->
            {error, connection_failed, maps:get(reason_connection, Policy)};
        Addrs ->
            case lists:any(fun(IP) -> classify_ip(IP, Policy) =:= deny end, Addrs) of
                true ->
                    %% At least one resolved address is blocked infra: deny the
                    %% whole request rather than connect to a host that resolves
                    %% (even partly) to IMDS/loopback/link-local.
                    {error, input_invalid, maps:get(reason_not_allowed, Policy)};
                false ->
                    PinIP = inet:ntoa(hd(Addrs)),
                    {ok, pin_url(Url, PinIP), Host}
            end
    end.

%% Resolve a hostname to all IPv4 + IPv6 addresses (best-effort per family).
resolve_all(Host) ->
    V4 =
        case inet:getaddrs(Host, inet) of
            {ok, A4} -> A4;
            {error, _} -> []
        end,
    V6 =
        case inet:getaddrs(Host, inet6) of
            {ok, A6} -> A6;
            {error, _} -> []
        end,
    V4 ++ V6.

%% Rewrite the URL to connect to the pinned IP, keeping scheme/port/path/query.
%% The original hostname is carried separately by the caller (Host header + SNI)
%% so vhost routing and cert hostname verification still target the real name.
pin_url(Url, PinHost) ->
    Rebuilt = uri_string:recompose(maps:without([url_string], Url#{host => PinHost})),
    Url#{url_string => Rebuilt}.
