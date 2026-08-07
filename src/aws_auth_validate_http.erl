%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% HTTP auth-backend reachability validation.
%%
%% Validates a `rabbit_auth_backend_http' configuration (the auth_http.*
%% settings a customer would put in rabbitmq.conf) without a broker restart.
%% Mirrors the LDAP backend's shape: pure input validation runs first, secret
%% ARNs are resolved only after, and every outcome collapses to a fixed
%% category so the response leaks no target URL, response body, or raw httpc
%% error.
%%
%% HTTP client: this backend uses httpc/inets, NOT gun (which aws_lib uses for
%% AWS API calls). That is deliberate. The point of a config validator is to
%% reproduce the REAL code path, and rabbit_auth_backend_http -- the backend
%% this endpoint validates -- issues its auth requests via httpc:request/4
%% (rabbit_auth_backend_http:do_http_req/2, in deps/rabbitmq_auth_backend_http).
%% Probing with the same client the broker uses means a green result here
%% reflects how the broker will actually behave: the same TLS option handling,
%% redirect policy, header shaping, and timeout semantics. A different client
%% (e.g. gun) could pass here yet fail in production, or vice versa. This
%% mirrors the LDAP backend, which uses eldap because rabbit_auth_backend_ldap
%% does. The cost of httpc is its global-profile model (see the profile-pool
%% section below); that complexity is the price of fidelity, not an arbitrary
%% choice.
%%
%% Two modes:
%%   * Reachability-only (default, no credentials supplied): confirms each
%%     configured *_path is reachable over (m)TLS and answers like an auth
%%     server -- a usable HTTP status (200/201) AND a body matching
%%     rabbit_auth_backend_http's allow/deny grammar. A deny for the synthetic
%%     probe principal is a success, since a well-formed deny still proves the
%%     endpoint speaks the auth protocol.
%%   * Credentialed-probe (username + password_arn supplied): resolves the ARN,
%%     probes user_path with the real credentials, and asserts the response is
%%     allow. A deny becomes auth_failed (422). Other paths remain
%%     reachability-only.
%%
%% Category mapping (reuses the existing aws_auth_validate_backend categories;
%% no new category is introduced -- the fixed set is kept small so responses
%% cannot be used for SSRF reconnaissance or information disclosure):
%%   * connection_failed (400) -- host unreachable / DNS / connection refused.
%%   * tls_failed        (400) -- TLS handshake / cert verification failure.
%%   * auth_failed       (422) -- reached the server but its response is not
%%                                auth-server-shaped (no usable HTTP status).
%%     NOTE: auth_failed is borrowed for "endpoint did not behave like an auth
%%     server". If reachability-only proves too coarse, introduce a dedicated
%%     `endpoint_unverified' category (analogous to authz_unverified) -- that
%%     requires extending aws_auth_validate_backend:error_category/0 and the
%%     handler's status_for_category/1.
-module(aws_auth_validate_http).

-behaviour(aws_auth_validate_backend).

-export([method_name/0, validate/1, allowed_fields/0]).

-ifdef(TEST).
%% Exposed for unit tests: the SSRF address policy (classify_ip/1 and the
%% CIDR primitives), the URL parser, the per-URL pure guard, the network-phase
%% resolve+pin, the TLS-option builder, and the httpc error classifier. All are
%% otherwise internal. Mirrors aws_auth_validate_ldap's TEST export block.
-export([
    parse_url/1,
    parse_paths/2,
    url_allowed/1,
    classify_address/1,
    classify_ip/1,
    in_cidr/2,
    in_any_cidr/2,
    resolve_and_pin/1,
    pin_url/2,
    build_client_ssl_opts/1,
    classify_http_error/1,
    is_tls_error/1,
    classify_response/2,
    query_for/1,
    params_for/1,
    credentialed_query_for/1,
    classify_credentialed_response/1,
    parse_credentials/2
]).
-endif.

%% Default per-request timeout (ms) when auth_validation_connection_timeout_ms
%% is unset. Matches the LDAP backend's default.
-define(DEFAULT_TIMEOUT_MS, 5_000).

%% Ephemeral httpc profile-name prefix for this backend's pool (shared pool
%% machinery in aws_auth_validate_httpc). Disjoint from the oauth prefix so the
%% two backends' pools never collide.
-define(PROFILE_PREFIX, "aws_auth_validate_http_").

%% The four request paths a rabbit_auth_backend_http config can define. Each is
%% an absolute URL. user_path is required (the broker always issues a user
%% check); the other three are optional and validated/probed only when present.
-define(PATH_KEYS, [
    <<"user_path">>,
    <<"vhost_path">>,
    <<"resource_path">>,
    <<"topic_path">>
]).
-define(REQUIRED_PATH_KEY, <<"user_path">>).

%% Accepted values for http_method (mirrors rabbit_auth_backend_http's
%% auth_http.http_method).
-define(HTTP_METHOD_VALUES, [<<"get">>, <<"post">>]).

%% ssl_options surface, identical to the LDAP backend's (auth_http.ssl_options.*
%% has the same shape as auth_ldap.ssl_options.*).
%% Accepted ssl_options keys, named to match what a customer writes under
%% auth_http.ssl_options.* in rabbitmq.conf so a config can be pasted as-is.
%% In particular the key is `sni' (the broker's config key), NOT the internal
%% `server_name_indication' atom it maps to -- the probe translates sni ->
%% server_name_indication when it builds the httpc ssl options. cacertfile_arn
%% mirrors the tutorial's aws.arns.auth_http.ssl_options.cacertfile ARN line.
%% certfile_arn / keyfile_arn carry the CLIENT certificate + private key for
%% mutual TLS (the broker's auth_http.ssl_options.certfile / .keyfile). The
%% cert is typically an S3-hosted PEM; the key a Secrets Manager PEM. Both are
%% resolved like cacertfile_arn and decoded into in-memory ssl {cert,_}/{key,_}
%% options so an mTLS auth server can be validated.
-define(SSL_OPTION_KEYS, [
    <<"cacertfile_arn">>,
    <<"certfile_arn">>,
    <<"keyfile_arn">>,
    <<"verify">>,
    <<"depth">>,
    <<"versions">>,
    <<"sni">>,
    <<"hostname_verification">>
]).
%% Fixed, hardcoded reason strings -- never echo a URL, host, or raw error,
%% so the endpoint cannot be used for SSRF reconnaissance.
-define(REASON_BAD_PATHS, <<"at least user_path must be a non-empty URL string">>).
-define(REASON_BAD_PATH_VALUE, <<"each path must be a non-empty URL string">>).
-define(REASON_BAD_URL, <<"a configured path is not a valid http(s) URL">>).
-define(REASON_URL_NOT_ALLOWED, <<"a configured path targets a disallowed address">>).
-define(REASON_BAD_HTTP_METHOD, <<"http_method must be get or post">>).
-define(REASON_BAD_SSL_OPTIONS, <<"ssl_options must be an object">>).
-define(REASON_UNKNOWN_SSL_OPTION, <<
    "ssl_options contains an unknown key; allowed keys are cacertfile_arn, "
    "certfile_arn, keyfile_arn, verify, depth, versions, sni, hostname_verification"
>>).
-define(REASON_BAD_SSL_VERIFY, <<"ssl_options.verify must be verify_peer or verify_none">>).
-define(REASON_BAD_SSL_DEPTH, <<"ssl_options.depth must be a non-negative integer">>).
-define(REASON_BAD_SSL_VERSIONS, <<"ssl_options.versions must be a list of known TLS versions">>).
-define(REASON_BAD_SSL_SNI, <<"ssl_options.sni must be a string">>).
-define(REASON_BAD_SSL_HOSTNAME_VERIFICATION,
    <<"ssl_options.hostname_verification must be wildcard or none">>
).
-define(REASON_BAD_SSL_CACERT_ARN, <<"ssl_options.cacertfile_arn must be a non-empty string">>).
-define(REASON_BAD_SSL_CERT_ARN, <<"ssl_options.certfile_arn must be a non-empty string">>).
-define(REASON_BAD_SSL_KEY_ARN, <<"ssl_options.keyfile_arn must be a non-empty string">>).
-define(REASON_CLIENT_CERT_INCOMPLETE,
    <<"ssl_options.certfile_arn and keyfile_arn must be supplied together">>
).
-define(REASON_CONNECTION, <<"could not connect to HTTP auth server">>).
-define(REASON_TLS_HANDSHAKE, <<"TLS handshake failed">>).
-define(REASON_ENDPOINT, <<"HTTP auth server did not return a usable response">>).
-define(REASON_ASSUME_ROLE, <<"failed to assume the configured role">>).
-define(REASON_NO_ASSUME_ROLE, <<
    "auth validation requires an assume_role to be configured; "
    "set aws.arns.assume_role_arn"
>>).
-define(REASON_CREDENTIAL_INCOMPLETE,
    <<"username and password_arn must be supplied together">>
).
-define(REASON_AUTH_DENIED, <<"HTTP auth server denied the supplied credentials">>).

%% HTTP status codes rabbit_auth_backend_http treats as a usable auth response
%% (it accepts 200 and 201). We accept the same set as "the endpoint answered
%% like an auth server"; any other well-formed status still proves reachability
%% but is reported as endpoint-unverified (the server is up but not responding
%% the way the auth backend expects).
-define(AUTH_RESPONSE_CODES, [200, 201]).

%% SSRF guard toggles (see the SSRF guard section below for the full rationale).
%% The address policy (classify_ip/1 + the probe-layer resolve+pin) is live, so
%% ?ENFORCE_ADDRESS_POLICY is TRUE. It is retained as a defined marker of that
%% state; enforcement itself runs unconditionally via url_allowed/1 and the
%% probe-layer resolve+classify. The method is opt-in
%% (aws_auth_validate_registry ?OPT_IN_METHODS), so it is not live until an
%% operator explicitly enables it.
-define(ALLOWED_SCHEMES, ["https", "http"]).
-define(ENFORCE_ADDRESS_POLICY, true).

%% Always-denied address ranges -- the broker's own privileged infrastructure.
%% We deliberately do NOT block RFC1918 / unique-local: a customer's HTTP auth
%% server normally lives in their VPC, so blocking private ranges would make the
%% endpoint useless. The threat is the broker reaching its OWN infra (IMDS =
%% instance-role credential theft, loopback = broker-local services), not
%% reaching private addresses in general.
-define(DENIED_V4_CIDRS, [
    %% loopback 127.0.0.0/8
    {{127, 0, 0, 0}, 8},
    %% link-local 169.254.0.0/16 (this range CONTAINS the IMDS 169.254.169.254)
    {{169, 254, 0, 0}, 16},
    %% unspecified / "this host" 0.0.0.0/8
    {{0, 0, 0, 0}, 8}
]).
-define(DENIED_V6_CIDRS, [
    %% loopback ::1/128
    {{0, 0, 0, 0, 0, 0, 0, 1}, 128},
    %% link-local fe80::/10 (IPv6 IMDS link-local lives here)
    {{16#fe80, 0, 0, 0, 0, 0, 0, 0}, 10},
    %% AWS IMDSv6 fd00:ec2::254/128
    {{16#fd00, 16#0ec2, 0, 0, 0, 0, 0, 16#0254}, 128},
    %% unspecified ::/128
    {{0, 0, 0, 0, 0, 0, 0, 0}, 128}
]).

%% Per-backend surface passed to the shared aws_auth_validate_ssl helpers: which
%% ssl_options keys reference an ARN, the full allowed-key set, the customer SNI
%% key spelling, whether the mTLS client-cert pair is accepted, and this
%% backend's fixed reason strings (kept here so wording/tests are unchanged).
ssl_opts() ->
    #{
        arn_keys => [<<"cacertfile_arn">>, <<"certfile_arn">>, <<"keyfile_arn">>],
        ssl_option_keys => ?SSL_OPTION_KEYS,
        sni_key => <<"sni">>,
        client_cert => true,
        reasons => #{
            no_assume_role => ?REASON_NO_ASSUME_ROLE,
            assume_role => ?REASON_ASSUME_ROLE,
            unknown_ssl_option => ?REASON_UNKNOWN_SSL_OPTION,
            client_cert_incomplete => ?REASON_CLIENT_CERT_INCOMPLETE,
            bad_ssl_options => ?REASON_BAD_SSL_OPTIONS,
            bad_ssl_verify => ?REASON_BAD_SSL_VERIFY,
            bad_ssl_depth => ?REASON_BAD_SSL_DEPTH,
            bad_ssl_versions => ?REASON_BAD_SSL_VERSIONS,
            bad_ssl_sni => ?REASON_BAD_SSL_SNI,
            bad_ssl_hostname_verification => ?REASON_BAD_SSL_HOSTNAME_VERIFICATION,
            bad_ssl_cacert_arn => ?REASON_BAD_SSL_CACERT_ARN,
            bad_ssl_cert_arn => ?REASON_BAD_SSL_CERT_ARN,
            bad_ssl_key_arn => ?REASON_BAD_SSL_KEY_ARN
        }
    }.

%% SSRF policy passed to the shared aws_auth_validate_net guard: this backend's
%% infra denylists, its scheme allowlist (http+https), and the fixed reason
%% strings for a disallowed target / an unreachable host.
net_policy() ->
    #{
        denied_v4 => ?DENIED_V4_CIDRS,
        denied_v6 => ?DENIED_V6_CIDRS,
        allowed_schemes => ?ALLOWED_SCHEMES,
        reason_not_allowed => ?REASON_URL_NOT_ALLOWED,
        reason_connection => ?REASON_CONNECTION
    }.

%%--------------------------------------------------------------------
%% Behaviour callbacks
%%--------------------------------------------------------------------

method_name() ->
    <<"http">>.

allowed_fields() ->
    [
        <<"user_path">>,
        <<"vhost_path">>,
        <<"resource_path">>,
        <<"topic_path">>,
        <<"http_method">>,
        <<"ssl_options">>,
        <<"username">>,
        <<"password_arn">>
    ].

-spec validate(map()) -> aws_auth_validate_backend:result().
validate(Body) when is_map(Body) ->
    %% Whether the method may run is gated by the opt-in registry
    %% (aws_auth_validate_registry ?OPT_IN_METHODS), not a compile-time switch:
    %% the SSRF address policy is live, so no fail-closed guard is needed here.
    %% Same ARN-first ordering as the LDAP backend: all pure, network-free
    %% validation (URL shape, method, ssl_options values, and the SSRF guard)
    %% runs before any cacertfile_arn is resolved or any request is made.
    case parse_input(Body) of
        {error, _, _} = Err ->
            Err;
        {ok, Params} ->
            case resolve_request_state(Params) of
                {error, _, _} = Err ->
                    Err;
                {ok, Params1} ->
                    case resolve_credential(Params1) of
                        {error, _, _} = Err -> Err;
                        {ok, Params2} -> do_http_validate(Params2)
                    end
            end
    end.

%% Build the per-request aws_lib state used for every ARN fetch in this request
%% (cacertfile_arn / certfile_arn / keyfile_arn / password_arn), and thread it
%% into Params. Runs after all pure input validation has passed, so a malformed
%% request never triggers an STS AssumeRole call (ARN-first ordering).
%%
%% Delegates to the shared aws_auth_validate_ssl:resolve_request_state/2 for both
%% modes. In credentialed mode (username + password_arn) the password_arn will be
%% resolved under the assumed role, so an assume_role_arn is MANDATORY regardless
%% of whether TLS material ARNs are also referenced -- the `force_assume_role'
%% flag in the opts map achieves this by skipping the request_references_arn check
%% and always requiring the configured role.
%%
%% For reachability-only mode the behaviour is unchanged: the shared helper checks
%% whether any ARN key is present in ssl_options before requiring a role. The
%% no-ARN branch stores the `none' credential sentinel (which resolve_arn/2
%% refuses), matching the fail-closed contract PR #116 introduced -- a customer
%% request can never resolve an ARN under the broker's ambient EC2 instance role.
resolve_request_state(#{credential_mode := credentialed} = Params) ->
    %% Credentialed mode resolves password_arn, so assume_role is mandatory
    %% regardless of whether TLS material ARNs are also referenced. The shared
    %% helper's force_assume_role flag skips the request_references_arn check and
    %% always requires/assumes the configured role.
    Opts = (ssl_opts())#{force_assume_role => true},
    aws_auth_validate_ssl:resolve_request_state(Params, Opts);
resolve_request_state(Params) ->
    aws_auth_validate_ssl:resolve_request_state(Params, ssl_opts()).

%% Resolve the password ARN for credentialed mode. Runs after
%% resolve_request_state/1 (which established the aws_state under the
%% configured assume_role), so the secret is fetched under the operator's
%% role -- not the broker's ambient instance credentials. The resolved
%% password is stored in the params map and never logged or returned.
resolve_credential(#{credential_mode := reachability} = Params) ->
    {ok, Params};
resolve_credential(
    #{credential_mode := credentialed, password_arn := Arn, aws_state := State} = Params
) ->
    case aws_auth_validate_ssl:resolve_arn(Arn, State) of
        {ok, Password} ->
            {ok, Params#{password => Password}};
        {error, _} ->
            {error, input_invalid, aws_auth_validate_ssl:arn_resolve_reason([<<"password_arn">>])}
    end.

%%--------------------------------------------------------------------
%% Input parsing (pure, no network)
%%--------------------------------------------------------------------

parse_input(Body) ->
    Steps = [
        fun parse_paths/2,
        fun parse_credentials/2,
        fun parse_http_method/2,
        fun parse_ssl_options/2,
        %% SSRF guard runs in the pure phase so a disallowed target is rejected
        %% before any ARN resolution or outbound request. See guard_urls/2.
        fun guard_urls/2
    ],
    parse_input(Steps, Body, #{timeout => connection_timeout_ms()}).

parse_input([], _Body, Acc) ->
    {ok, Acc};
parse_input([Step | Rest], Body, Acc0) ->
    case Step(Body, Acc0) of
        {ok, Acc1} -> parse_input(Rest, Body, Acc1);
        {error, _, _} = Err -> Err
    end.

%% Collect the configured paths. user_path is mandatory; the others are
%% optional. Each present value must be a non-empty string and parse as an
%% http(s) URL. Stored as a list of {Key, UrlString} preserving which path is
%% which (so a probe failure could be attributed internally, though the
%% response stays category-only).
parse_paths(Body, Acc) ->
    case maps:get(?REQUIRED_PATH_KEY, Body, undefined) of
        V when not (is_binary(V) andalso byte_size(V) > 0) ->
            {error, input_invalid, ?REASON_BAD_PATHS};
        _ ->
            collect_paths(?PATH_KEYS, Body, Acc, [])
    end.

collect_paths([], _Body, Acc, Paths) ->
    {ok, Acc#{paths => lists:reverse(Paths)}};
collect_paths([Key | Rest], Body, Acc, Paths) ->
    case maps:get(Key, Body, undefined) of
        undefined ->
            collect_paths(Rest, Body, Acc, Paths);
        V when is_binary(V), byte_size(V) > 0 ->
            case parse_url(V) of
                {ok, Url} -> collect_paths(Rest, Body, Acc, [{Key, Url} | Paths]);
                {error, _} -> {error, input_invalid, ?REASON_BAD_URL}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_PATH_VALUE}
    end.

%% Credential-mode parsing: determines whether this request is a reachability-
%% only probe (the default) or a credentialed probe (username + password_arn).
%% Both-or-neither enforcement: supplying only one of the pair is input_invalid.
parse_credentials(Body, Acc) ->
    Username = maps:get(<<"username">>, Body, undefined),
    PasswordArn = maps:get(<<"password_arn">>, Body, undefined),
    HasUser = aws_auth_validate_ssl:is_nonempty_binary(Username),
    HasArn = aws_auth_validate_ssl:is_nonempty_binary(PasswordArn),
    case {HasUser, HasArn} of
        {false, false} ->
            %% Neither supplied: reachability-only mode (the default path).
            {ok, Acc#{credential_mode => reachability}};
        {true, true} ->
            %% Both supplied: credentialed-probe mode.
            {ok, Acc#{
                credential_mode => credentialed, username => Username, password_arn => PasswordArn
            }};
        _ ->
            %% Only one of the pair is present (or one is present but not a
            %% non-empty binary): reject.
            {error, input_invalid, ?REASON_CREDENTIAL_INCOMPLETE}
    end.

%% Parse + minimally validate an http(s) URL via the shared
%% aws_auth_validate_net:parse_url/2. The broker's auth_http.*_path config is
%% query- and fragment-less and the probe appends its own ?username= params, so
%% a pre-existing query (double-? mangling) or #fragment (probe's query dropped
%% after the fragment) is rejected -- both are the parser's default. userinfo and
%% out-of-range ports are always rejected.
parse_url(Bin) when is_binary(Bin) ->
    aws_auth_validate_net:parse_url(Bin, #{allowed_schemes => ?ALLOWED_SCHEMES}).

parse_http_method(Body, Acc) ->
    case maps:get(<<"http_method">>, Body, undefined) of
        undefined ->
            %% Match rabbit_auth_backend_http's own default of `get' (set in
            %% its app env) when the caller omits http_method, so the probe
            %% uses the same method the broker would. For a reachability check
            %% the method barely matters (both yield a response), so we default
            %% rather than reject.
            {ok, Acc#{http_method => get}};
        V when is_binary(V) ->
            case lists:member(V, ?HTTP_METHOD_VALUES) of
                true -> {ok, Acc#{http_method => binary_to_existing_atom(V, utf8)}};
                false -> {error, input_invalid, ?REASON_BAD_HTTP_METHOD}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_HTTP_METHOD}
    end.

%% ssl_options parsing (keys + values, mTLS pairing) is shared across backends;
%% delegate to aws_auth_validate_ssl with this backend's surface (ssl_opts/0).
parse_ssl_options(Body, Acc) ->
    aws_auth_validate_ssl:parse_ssl_options(
        maps:get(<<"ssl_options">>, Body, undefined), Acc, ssl_opts()
    ).

%%--------------------------------------------------------------------
%% SSRF guard
%%--------------------------------------------------------------------
%%
%% The validator issues outbound requests to customer-supplied URLs using the
%% broker's network position -- a textbook SSRF / confused-deputy surface.
%% Defense is two-phase:
%%
%%   * PURE phase (guard_urls/2 here, via classify_address/1): enforce the
%%     scheme allowlist, and when the host is a LITERAL IP, classify it against
%%     the always-denied infra ranges immediately -- no DNS, deterministic. This
%%     catches http://169.254.169.254/... and https://[::1]/... up front.
%%
%%   * NETWORK phase (resolve_and_pin/1 in the probe): a HOSTNAME is only
%%     resolved at connect time, so the pure check cannot see its address. The
%%     probe resolves the host to ALL A/AAAA records, denies if ANY is a
%%     blocked infra address, then connects to the PINNED resolved IP (Host
%%     header + TLS SNI preserved) so httpc cannot re-resolve to a different
%%     address (DNS-rebinding / TOCTOU defense).
%%
%% Redirects are not followed (autoredirect=false in probe_one/4), so a 3xx
%% cannot bounce the probe to a blocked address.
%%
%% Policy: deny the broker's own infra (IMDS, loopback, link-local, unspecified
%% -- see ?DENIED_V4_CIDRS / ?DENIED_V6_CIDRS). RFC1918 / unique-local are NOT
%% denied: a customer auth server normally lives in their VPC.

guard_urls(_Body, #{paths := Paths} = Acc) ->
    case check_urls(Paths) of
        ok -> {ok, Acc};
        {error, _, _} = Err -> Err
    end.

check_urls([]) ->
    ok;
check_urls([{_Key, Url} | Rest]) ->
    case aws_auth_validate_net:url_allowed(Url, net_policy()) of
        ok -> check_urls(Rest);
        {error, _, _} = Err -> Err
    end.

-ifdef(TEST).
%% TEST-only wrappers exposing the shared SSRF guard under this backend's policy,
%% so the module's -ifdef(TEST) export contract stays stable. Production code
%% calls aws_auth_validate_net directly.
url_allowed(Url) -> aws_auth_validate_net:url_allowed(Url, net_policy()).
classify_address(Url) -> aws_auth_validate_net:classify_address(Url, net_policy()).
classify_ip(IP) -> aws_auth_validate_net:classify_ip(IP, net_policy()).
in_cidr(IP, Cidr) -> aws_auth_validate_net:in_cidr(IP, Cidr).
in_any_cidr(IP, Cidrs) -> aws_auth_validate_net:in_any_cidr(IP, Cidrs).
resolve_and_pin(Url) -> aws_auth_validate_net:resolve_and_pin(Url, net_policy()).
pin_url(Url, Host) -> aws_auth_validate_net:pin_url(Url, Host).
-endif.

%%--------------------------------------------------------------------
%% Reachability probe (network)
%%--------------------------------------------------------------------
%%
%% Reachability-only: confirm each configured path is reachable over (m)TLS and
%% answers like an auth server. We send a representative request with a clearly
%% SYNTHETIC username (never a real credential) and treat:
%%   * a 2xx the real backend accepts (200/201) AND a body matching the
%%       allow/deny grammar                       -> reachable + auth-shaped (ok)
%%   * a 2xx whose body is neither allow nor deny  -> reached a server, but it is
%%       not an auth backend (auth_failed) -- catches a path pointing at a health
%%       check, HTML page, or proxy that returns 200 for everything
%%   * any other well-formed HTTP status          -> reachable but the path is
%%       likely wrong (auth_failed) -- catches user_path pointing at the wrong
%%       endpoint, the most common config mistake after TLS/connectivity
%%   * connection refused / DNS / timeout         -> connection_failed
%%   * TLS handshake / cert verification failure  -> tls_failed
%% We check the SHAPE of the response, not its verdict: allow and deny are both
%% successes (a deny for the synthetic user still proves the endpoint speaks the
%% auth protocol). Only a body matching NEITHER is a failure.
%%
%% The whole probe runs inside a try/catch that collapses any raise to
%% connection_failed, so a resolved secret can never reach a crash report.
%%
%% All probe requests for THIS validation run on a dedicated, ephemeral httpc
%% profile that is started here and stopped in the `after' clause. This isolates
%% each validation's TLS sessions/connections: the shared default profile pools
%% TLS sessions, so a prior request's authenticated (e.g. mTLS) session could be
%% reused by a later request -- producing a false success for a config that
%% would not connect on its own (and a leaked connection across requests, which
%% violates the zero-side-effects invariant: each validation must use only
%% ephemeral connections and never mutate shared state). A fresh profile has no
%% sessions to reuse, and stopping it tears down anything opened, so each
%% validation is hermetic.
do_http_validate(Params) ->
    case aws_auth_validate_httpc:claim_probe_profile(?PROFILE_PREFIX) of
        none ->
            %% Could not obtain an isolated profile; fail safe rather than fall
            %% back to the shared default profile (which would reintroduce the
            %% session-reuse hazard). Unreachable in practice -- the semaphore
            %% caps concurrency below the pool size -- but must be handled.
            {error, connection_failed, ?REASON_CONNECTION};
        {ok, Profile} ->
            try
                case build_client_ssl_opts(Params) of
                    {error, _, _} = Err ->
                        Err;
                    {ok, SslOpts} ->
                        probe_paths(maps:get(paths, Params), Params, SslOpts, Profile)
                end
            catch
                _Class:_Reason:_Stack ->
                    {error, connection_failed, ?REASON_CONNECTION}
            after
                %% Only ever stop the profile THIS request started (claimed).
                aws_auth_validate_httpc:stop_probe_profile(Profile)
            end
    end.

%% Probe each configured path in turn; the first non-ok outcome wins (mirrors
%% the LDAP backend's check_dns/2 short-circuit).
probe_paths([], _Params, _SslOpts, _Profile) ->
    ok;
probe_paths([{Key, Url} | Rest], Params, SslOpts, Profile) ->
    case probe_one(Key, Url, Params, SslOpts, Profile) of
        ok -> probe_paths(Rest, Params, SslOpts, Profile);
        {error, _, _} = Err -> Err
    end.

probe_one(Key, Url, Params, SslOpts, Profile) ->
    #{http_method := Method, timeout := Timeout} = Params,
    %% Resolve the host and classify every resolved address against the infra
    %% denylist BEFORE connecting, then connect to the pinned IP so httpc cannot
    %% re-resolve to a different (possibly denied) address (DNS-rebinding /
    %% TOCTOU defense). For a literal-IP host this is a no-op pin to that IP.
    case aws_auth_validate_net:resolve_and_pin(Url, net_policy()) of
        {error, _, _} = Err ->
            Err;
        {ok, PinnedUrl, Host} ->
            UrlStr = maps:get(url_string, PinnedUrl),
            {Query, Classifier} = probe_query_and_classifier(Key, Params),
            %% Force POST for credentialed probes: a GET would append the
            %% resolved password to the URL query string, leaking it into auth
            %% server access logs and any intermediate proxy/LB logs. POST puts
            %% credentials in the request body where they are not logged by
            %% standard infrastructure. Reachability probes use whatever method
            %% the operator configured (default GET) -- they carry no secret.
            EffectiveMethod =
                case maps:get(credential_mode, Params, reachability) of
                    credentialed -> post;
                    _ -> Method
                end,
            Request = build_request(EffectiveMethod, UrlStr, Query, Host),
            HttpOpts =
                [
                    {timeout, Timeout},
                    {connect_timeout, Timeout},
                    %% Do not auto-follow redirects: a redirect could send the
                    %% probe to an address the SSRF guard would reject (and we
                    %% do not re-vet the Location target).
                    {autoredirect, false}
                ] ++ ssl_http_opt(Url, Host, SslOpts),
            case
                httpc:request(EffectiveMethod, Request, HttpOpts, [{body_format, binary}], Profile)
            of
                {ok, {{_Vsn, Code, _Phrase}, _Headers, Body}} ->
                    case lists:member(Code, ?AUTH_RESPONSE_CODES) of
                        %% A usable status is necessary but not sufficient: the
                        %% body must also match rabbit_auth_backend_http's
                        %% allow/deny grammar, or any 200-returning service
                        %% (health check, HTML page, proxy) would pass as an
                        %% auth backend. See classify_response/2.
                        true -> Classifier(Body);
                        false -> {error, auth_failed, ?REASON_ENDPOINT}
                    end;
                {error, Reason} ->
                    classify_http_error(Reason)
            end
    end.

%% Choose the query parameters and the response classifier for a given path.
%% In credentialed mode on user_path, use the real credentials and require
%% allow; in all other cases use the existing reachability query and contract
%% check. Separating this decision from the probe keeps probe_one/5 generic.
probe_query_and_classifier(<<"user_path">>, #{credential_mode := credentialed} = Params) ->
    Query = credentialed_query_for(Params),
    Classifier = fun classify_credentialed_response/1,
    {Query, Classifier};
probe_query_and_classifier(Key, _Params) ->
    Query = query_for(Key),
    Classifier = fun(Body) -> classify_response(Key, Body) end,
    {Query, Classifier}.

%% Build the query string for a credentialed user_path probe, matching the
%% fields rabbit_auth_backend_http sends for a user check (username + password).
%% Uses rabbit_http_util:quote_plus/1 -- the same encoder the real backend uses
%% in rabbit_auth_backend_http:q/1 -- to ensure byte-for-byte encoding parity.
%% uri_string:compose_query would re-encode non-ASCII bytes as UTF-8 codepoints
%% (double-encoding), producing different percent-sequences for multi-byte chars.
credentialed_query_for(#{username := Username, password := Password}) ->
    "username=" ++ rabbit_http_util:quote_plus(Username) ++
        "&password=" ++ rabbit_http_util:quote_plus(Password).

%% Map an httpc transport error to a fixed category (shared classifier). A
%% TLS/cert failure -> tls_failed; everything else -> connection_failed. The raw
%% reason is never echoed -- responses carry only fixed categories and hardcoded
%% strings to prevent information disclosure.
classify_http_error(Reason) ->
    aws_auth_validate_ssl:classify_http_error(
        Reason, ?REASON_TLS_HANDSHAKE, ?REASON_CONNECTION
    ).

-ifdef(TEST).
%% TEST-only re-export delegating to the shared TLS-error scanner (the module's
%% -ifdef(TEST) export block exposes it; production code calls the shared module
%% directly via classify_http_error/1).
is_tls_error(Term) ->
    aws_auth_validate_ssl:is_tls_error(Term).
-endif.

%% Response-contract check: the status code proved the server answered, this
%% proves it answered like rabbit_auth_backend_http. A `deny' (or a `deny'-
%% shaped response) is a SUCCESS here -- the probe uses a synthetic principal
%% the server is expected to reject, and a well-formed deny still proves the
%% endpoint speaks the auth protocol. We only fail when the body matches
%% NEITHER allow nor deny, which means the configured path points at something
%% that is not an auth backend (the false-pass this guard closes). We never
%% echo the body: a mismatch returns the fixed ?REASON_ENDPOINT.
%%
%% Grammar (mirrors rabbit_auth_backend_http's own parsing in
%% user_login_authentication/2 (authn) and req/2 (authz)). Both match a raw
%% `"deny " ++ Reason' body before lowercasing the trimmed body and matching
%% the bare verdicts, so a well-formed deny may carry a human-readable reason:
%%   * user_path (authn): `deny' (optionally followed by whitespace and a
%%     reason), or `allow' (optionally followed by space-separated tags).
%%   * vhost/resource/topic_path (authz): exactly `allow', or `deny' (optionally
%%     followed by whitespace and a reason).
classify_response(Key, Body) ->
    case response_matches_contract(Key, normalize_resp(Body)) of
        true -> ok;
        false -> {error, auth_failed, ?REASON_ENDPOINT}
    end.

%% In credentialed mode, only `allow' is success. A well-formed `deny' means
%% the credential was rejected -- the operator expected it to work, so this is
%% auth_failed with REASON_AUTH_DENIED. A body matching NEITHER allow NOR deny
%% means the endpoint is not an auth server at all -- report REASON_ENDPOINT (the
%% same distinction the reachability classifier makes), so the operator is not
%% misled into thinking their credentials are wrong when the path simply points at
%% the wrong endpoint.
classify_credentialed_response(Body) ->
    Normalized = normalize_resp(Body),
    case is_keyword_prefix(<<"allow">>, Normalized) of
        true ->
            ok;
        false ->
            case is_keyword_prefix(<<"deny">>, Normalized) of
                true -> {error, auth_failed, ?REASON_AUTH_DENIED};
                false -> {error, auth_failed, ?REASON_ENDPOINT}
            end
    end.

%% Normalize exactly as rabbit_auth_backend_http does: strip leading/trailing
%% whitespace, then lowercase. Tolerates a non-binary/oversized body defensively
%% (an auth response is tiny; anything else is not auth-shaped).
normalize_resp(Body) when is_binary(Body) ->
    string:lowercase(string:trim(Body));
normalize_resp(_) ->
    <<>>.

%% user_path authenticates, so it accepts allow-with-tags, the same as the real
%% backend's `"allow" ++ Rest' clause. Both authn and authz accept a `deny' that
%% carries a reason, mirroring the backend's `"deny " ++ Reason' clause. The
%% authz paths never return allow-with-tags, so allow must be bare there.
response_matches_contract(<<"user_path">>, Resp) ->
    is_keyword_prefix(<<"deny">>, Resp) orelse is_keyword_prefix(<<"allow">>, Resp);
response_matches_contract(_AuthzPath, Resp) ->
    Resp =:= <<"allow">> orelse is_keyword_prefix(<<"deny">>, Resp).

%% True when Resp is exactly Keyword, or Keyword followed by a whitespace-
%% delimited remainder (an `allow' tag list, or a `deny' reason). A near-miss
%% like `allowxyz' or `denied' is rejected: the real backend tolerates a bare
%% `"allow" ++ Rest', but for a config-validation signal we require the
%% canonical separator so a near-miss body is flagged rather than waved through.
is_keyword_prefix(Keyword, Resp) ->
    KwSize = byte_size(Keyword),
    case Resp of
        Keyword ->
            true;
        <<Keyword:KwSize/binary, Sep, _/binary>> ->
            Sep =:= $\s orelse Sep =:= $\t;
        _ ->
            false
    end.

%% Build the httpc Request tuple for the configured method. For GET the query
%% goes in the URL; for POST it is a form-encoded body, matching
%% rabbit_auth_backend_http's request shape so the endpoint's decision matches
%% the live broker's for the same inputs. The Host header carries the ORIGINAL
%% hostname (the URL host is the pinned IP), so the auth server sees the name it
%% expects -- needed for name-based vhosts.
build_request(get, UrlStr, Query, Host) ->
    Sep =
        case Query of
            "" -> "";
            _ -> "?"
        end,
    {UrlStr ++ Sep ++ Query, [{"host", Host}]};
build_request(post, UrlStr, Query, Host) ->
    {UrlStr, [{"host", Host}], "application/x-www-form-urlencoded", Query}.

%% Only attach ssl options for https targets; httpc ignores them for http.
%% server_name_indication is forced to the ORIGINAL hostname (not the pinned
%% IP) so SNI and certificate hostname verification validate against the name
%% the customer configured -- unless the caller set sni explicitly, which
%% translate_ssl_opts already honoured.
ssl_http_opt(#{scheme := "https"}, Host, SslOpts) ->
    [{ssl, with_default_sni(SslOpts, Host)}];
ssl_http_opt(_Url, _Host, _SslOpts) ->
    [].

with_default_sni(SslOpts, Host) ->
    case lists:keymember(server_name_indication, 1, SslOpts) of
        true -> SslOpts;
        false -> [{server_name_indication, Host} | SslOpts]
    end.

%% Representative, clearly-synthetic query params per path type, mirroring the
%% fields rabbit_auth_backend_http sends so a conformant server returns a 2xx
%% (allow or deny) rather than a 400 for missing params. The username is an
%% obvious placeholder so an operator reading their auth-server logs can see it
%% was a validation probe, not a real login attempt. The authz paths carry a
%% `tags' field (empty for the tagless probe user), matching what upstream's
%% check_vhost_access/check_resource_access/check_topic_access always send via
%% join_tags/1; a server that requires the field must still see it.
%%
%% NOTE: the parity test feeds params_for/1 to both sides, so it proves the two
%% ENCODERS agree but cannot catch this field set drifting from upstream's; keep
%% these lists in step with the fields those upstream functions send by hand.
query_for(Key) ->
    uri_string:compose_query(params_for(Key)).

-define(PROBE_USER, "__rabbitmq_config_validation_probe__").
-define(PROBE_VHOST, "/").

params_for(<<"user_path">>) ->
    [{"username", ?PROBE_USER}];
params_for(<<"vhost_path">>) ->
    [
        {"username", ?PROBE_USER},
        {"vhost", ?PROBE_VHOST},
        {"ip", "127.0.0.1"},
        {"tags", ""}
    ];
params_for(<<"resource_path">>) ->
    [
        {"username", ?PROBE_USER},
        {"vhost", ?PROBE_VHOST},
        {"resource", "queue"},
        {"name", "validation-probe"},
        {"permission", "read"},
        {"tags", ""}
    ];
params_for(<<"topic_path">>) ->
    [
        {"username", ?PROBE_USER},
        {"vhost", ?PROBE_VHOST},
        {"resource", "topic"},
        {"name", "validation-probe"},
        {"permission", "read"},
        {"tags", ""},
        {"routing_key", "#"}
    ].

%%--------------------------------------------------------------------
%% TLS option shaping (ports the LDAP backend's resolution, wrapped for httpc)
%%--------------------------------------------------------------------

%% Resolve the ARN-backed TLS material (CA bundle, and the client cert+key for
%% mTLS) and translate the validated ssl_options map into an Erlang ssl
%% proplist for httpc. ARN resolution happens here, in the network phase, after
%% all pure validation has passed (ARN-first ordering). Any resolution failure
%% maps to input_invalid, matching the LDAP backend.
%% Resolve the ARN-backed TLS material (CA bundle + optional mTLS client cert)
%% and translate the validated ssl_options into an ssl proplist for httpc. The
%% resolution, decoding, and verify-default policy are shared
%% (aws_auth_validate_ssl); only the SNI key spelling (<<"sni">>) is
%% backend-specific.
build_client_ssl_opts(#{ssl_options := Map} = Params) ->
    %% State is used for every ARN fetch in this function. A request that
    %% references no ARN carries the `none' sentinel, which the shared
    %% resolve_arn/2 refuses -- so even if an ARN fetch were reached on that
    %% path it fails closed instead of falling back to the instance role.
    %% Default to `none' (never a usable state) if the key is somehow absent,
    %% preserving the fail-closed contract.
    State = maps:get(aws_state, Params, none),
    %% Resolves the CA bundle and the mTLS pair together so a response can name
    %% every ARN field that failed, not just the first.
    case aws_auth_validate_ssl:resolve_ssl_material(Map, State) of
        {error, _, _} = Err ->
            Err;
        {ok, MaterialOpts} ->
            Opts = MaterialOpts ++ aws_auth_validate_ssl:translate_ssl_opts(Map, <<"sni">>),
            %% Whether the caller set verify explicitly governs the
            %% no-trust-anchor policy (fail vs silent default).
            VerifyExplicit = maps:is_key(<<"verify">>, Map),
            %% Mirror rabbit_auth_backend_http: the RFC 6125 https match
            %% fun is applied ONLY when ssl_hostname_verification =
            %% wildcard; unset is strict OTP matching. Defaulting to
            %% wildcard here would pass a cert the live broker rejects.
            HostnameCheck = aws_auth_validate_ssl:hostname_check_mode(Map),
            aws_auth_validate_ssl:apply_verify_default(Opts, VerifyExplicit, HostnameCheck)
    end.

connection_timeout_ms() ->
    aws_auth_validate_ssl:connection_timeout_ms(#{default => ?DEFAULT_TIMEOUT_MS, max => 60_000}).
