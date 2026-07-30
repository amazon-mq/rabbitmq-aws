%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Validates broker-side TLS/mTLS material without a broker restart.
%%
%% The SSL and mTLS setups configure a CA bundle by ARN
%% (aws.arns.ssl_options.cacertfile / aws.arns.management.ssl.cacertfile). A
%% wrong ARN or a malformed/expired CA otherwise only shows up at boot. This
%% backend checks that material up front.
%%
%% It validates the material only, not a live handshake. The other backends
%% probe an outbound auth server; here the config is an inbound listener, so
%% there is nothing for the broker to connect to. Checks:
%%   1. the cacertfile ARN resolves,
%%   2. the resolved PEM holds at least one well-formed CA certificate,
%%   3. none of those certificates is expired or not yet valid,
%%   4. verify / fail_if_no_peer_cert / depth / versions are well-shaped.
%%
%% Optionally, when `client_cert' and `cert_login' are supplied, the backend
%% also validates:
%%   5. the client certificate chains to the supplied CA bundle (Layer 1),
%%   6. a username can be extracted from the leaf cert using the configured
%%      cert_login strategy -- DN / CN / SAN (Layer 2),
%%   7. the extracted username resolves to an internal broker user (Layer 3).
%%
%% A pass means the material is usable and (when layers 5-7 are exercised) the
%% cert-login pipeline produces a resolvable user, not that the broker is
%% actually running this config.
%%
%% Result categories (shared with the other backends):
%%   * input_invalid (400) -- bad target/ssl_options, missing cacertfile_arn,
%%     ARN resolve failure, or a PEM with no parseable CA certificate.
%%   * tls_failed (400) -- a CA certificate is expired or not yet valid.
%%   * auth_failed (422) -- client certificate chain validation failed, no
%%     username could be extracted, or the extracted user does not exist.
%%   * config_conflict (422) -- a cacertfile_arn is given but no
%%     aws.arns.assume_role_arn is configured; or cert_login references
%%     unavailable broker modules.
-module(aws_auth_validate_tls).

-behaviour(aws_auth_validate_backend).

-export([method_name/0, validate/1, allowed_fields/0]).

-ifdef(TEST).
%% Exposed for the unit tests: the pure input parser, the certificate-validity
%% helpers (classify_validity/3 lets the expired/not-yet-valid branches be tested
%% without generating an actually-expired cert), and the client-cert-login layers.
-export([
    parse_input/1,
    check_cert_validity/1,
    cert_validity_seconds/1,
    classify_validity/3,
    validate_chain/3,
    extract_username/2,
    resolve_user/1,
    sanitize_username/1
]).
-endif.

-include_lib("public_key/include/public_key.hrl").

%% The listener a request targets. The material check is the same for both; a
%% request must name one, there is no default.
-define(TARGET_VALUES, [<<"listener">>, <<"management">>]).

%% Accepted ssl_options keys, named as in rabbitmq.conf (ssl_options.* /
%% management.ssl.*) so a config can be pasted as-is. cacertfile_arn is the only
%% material these setups supply -- the server cert is AWS-managed -- so there is
%% no certfile_arn/keyfile_arn and no sni.
-define(SSL_OPTION_KEYS, [
    <<"cacertfile_arn">>,
    <<"verify">>,
    <<"fail_if_no_peer_cert">>,
    <<"depth">>,
    <<"versions">>
]).

%% Fixed reason strings: the response never echoes the ARN, cert details, or a
%% raw decode error.
-define(REASON_BAD_TARGET, <<"target must be \"listener\" or \"management\"">>).
-define(REASON_MISSING_CACERT, <<"ssl_options.cacertfile_arn is required">>).
-define(REASON_BAD_SSL_OPTIONS, <<"ssl_options must be an object">>).
-define(REASON_UNKNOWN_SSL_OPTION, <<
    "ssl_options contains an unknown key; allowed keys are cacertfile_arn, "
    "verify, fail_if_no_peer_cert, depth, versions"
>>).
-define(REASON_BAD_SSL_VERIFY, <<"ssl_options.verify must be verify_peer or verify_none">>).
-define(REASON_BAD_SSL_DEPTH, <<"ssl_options.depth must be a non-negative integer">>).
-define(REASON_BAD_SSL_VERSIONS, <<"ssl_options.versions must be a list of known TLS versions">>).
-define(REASON_BAD_SSL_FAIL_IF_NO_PEER_CERT,
    <<"ssl_options.fail_if_no_peer_cert must be true or false">>
).
-define(REASON_BAD_SSL_CACERT_ARN, <<"ssl_options.cacertfile_arn must be a non-empty string">>).
-define(REASON_ARN_RESOLVE, <<"failed to resolve ARN">>).
-define(REASON_NO_CERTS, <<"cacertfile ARN did not resolve to any CA certificates">>).
-define(REASON_BAD_CERT, <<"a certificate in the CA bundle could not be parsed">>).
-define(REASON_CERT_EXPIRED, <<"the CA bundle contains an expired certificate">>).
-define(REASON_CERT_NOT_YET_VALID,
    <<"the CA bundle contains a certificate that is not yet valid">>
).
-define(REASON_ASSUME_ROLE, <<"failed to assume the configured role">>).
-define(REASON_NO_ASSUME_ROLE, <<
    "auth validation requires an assume_role to be configured; "
    "set aws.arns.assume_role_arn"
>>).

%% client_cert / cert_login reason strings (Layer 1-3 validation).
-define(REASON_BAD_CLIENT_CERT,
    <<"client_cert must be a non-empty PEM-encoded certificate chain (leaf certificate first)">>
).
-define(REASON_CLIENT_CERT_PRIVATE_KEY,
    <<"client_cert must not contain private key material; send only certificate entries">>
).
-define(REASON_CLIENT_CERT_UNEXPECTED_ENTRY,
    <<"client_cert contains a non-certificate PEM entry; send only certificate entries">>
).
-define(REASON_BAD_CERT_LOGIN, <<"cert_login must be an object">>).
-define(REASON_BAD_CERT_LOGIN_FROM, <<
    "cert_login.from must be distinguished_name, common_name, "
    "subject_alternative_name, or subject_alt_name"
>>).
-define(REASON_SAN_TYPE_REQUIRES_SAN_FROM,
    <<"cert_login.san_type is only valid when from is subject_alternative_name">>
).
-define(REASON_SAN_INDEX_REQUIRES_SAN_FROM,
    <<"cert_login.san_index is only valid when from is subject_alternative_name">>
).
-define(REASON_BAD_SAN_TYPE,
    <<"cert_login.san_type must be dns, ip, email, uri, or other_name">>
).
-define(REASON_BAD_SAN_INDEX,
    <<"cert_login.san_index must be a non-negative integer">>
).
-define(REASON_UNKNOWN_CERT_LOGIN_KEY,
    <<"cert_login contains an unknown key; allowed keys are from, san_type, san_index">>
).
-define(REASON_CERT_LOGIN_REQUIRES_CLIENT_CERT,
    <<"cert_login requires client_cert to be present">>
).
-define(REASON_CERT_LOGIN_REQUIRES_VERIFY_PEER,
    <<"cert-based login requires ssl_options.verify = verify_peer">>
).
-define(REASON_CHAIN_FAILED,
    <<"the client certificate does not chain to the supplied CA bundle">>
).
-define(REASON_CHAIN_DEPTH_EXCEEDED,
    <<"the client certificate chain exceeds the configured path length (depth)">>
).
-define(REASON_CHAIN_INVALID_SIGNATURE,
    <<"a certificate in the client chain has an invalid signature">>
).
-define(REASON_CHAIN_INVALID_ISSUER,
    <<"a certificate in the client chain has an invalid issuer">>
).
-define(REASON_CHAIN_BAD_KEY_USAGE,
    <<"a certificate in the client chain has invalid key usage">>
).
-define(REASON_CHAIN_MISSING_BASIC_CONSTRAINT,
    <<"a certificate in the client chain is missing a required basic constraint">>
).
-define(REASON_CHAIN_EXPIRED,
    <<"the client certificate chain contains an expired or not-yet-valid certificate">>
).
-define(REASON_CHAIN_MALFORMED,
    <<"a certificate in the client chain could not be parsed">>
).
-define(REASON_EXTRACT_FAILED,
    <<"no username could be extracted from the client certificate with the supplied cert_login settings">>
).
-define(REASON_USER_NOT_FOUND(Name),
    iolist_to_binary([
        <<"no internal user named ">>, sanitize_username(Name), <<" exists">>
    ])
).
-define(REASON_USER_LOOKUP_UNAVAILABLE,
    <<"user-resolution check unavailable on this broker series">>
).
-define(REASON_USER_LOOKUP_INCONCLUSIVE(Name),
    iolist_to_binary([
        <<"user ">>,
        sanitize_username(Name),
        <<
            " not found in the internal backend; other auth backends are configured"
            " and the user may exist there (cannot verify without network I/O)"
        >>
    ])
).

-define(INTERNAL_BACKEND, rabbit_auth_backend_internal).
-define(MAX_USERNAME_LEN, 256).

%% Known private-key atom tags. Used for message selection only -- the actual
%% rejection is allowlist-based (only 'Certificate' entries pass). When the
%% offending entry carries one of these types or a tuple tag like
%% {no_asn1, new_openssh}, the operator gets the specific "never send key
%% material" reason; other unrecognized entry types get a generic rejection.
%% EncryptedPrivateKeyInfo is listed for documentation: OTP rewrites it to
%% PrivateKeyInfo internally before a caller sees it, but keeping it here is
%% harmless and documents intent.
-define(PRIVATE_KEY_TYPES, [
    'RSAPrivateKey',
    'DSAPrivateKey',
    'ECPrivateKey',
    'PrivateKeyInfo',
    'EncryptedPrivateKeyInfo'
]).

%% Surface passed to the shared aws_auth_validate_ssl helpers: the ARN-bearing
%% keys, the allowed-key set, and this backend's reason strings. client_cert is
%% false (no client pair) and sni_key is unused here; both are required by the
%% shared opts() type.
ssl_opts() ->
    #{
        arn_keys => [<<"cacertfile_arn">>],
        ssl_option_keys => ?SSL_OPTION_KEYS,
        sni_key => <<"sni">>,
        client_cert => false,
        reasons => #{
            no_assume_role => ?REASON_NO_ASSUME_ROLE,
            assume_role => ?REASON_ASSUME_ROLE,
            unknown_ssl_option => ?REASON_UNKNOWN_SSL_OPTION,
            bad_ssl_options => ?REASON_BAD_SSL_OPTIONS,
            bad_ssl_verify => ?REASON_BAD_SSL_VERIFY,
            bad_ssl_depth => ?REASON_BAD_SSL_DEPTH,
            bad_ssl_versions => ?REASON_BAD_SSL_VERSIONS,
            bad_ssl_fail_if_no_peer_cert => ?REASON_BAD_SSL_FAIL_IF_NO_PEER_CERT,
            bad_ssl_cacert_arn => ?REASON_BAD_SSL_CACERT_ARN
        }
    }.

%%--------------------------------------------------------------------
%% Behaviour callbacks
%%--------------------------------------------------------------------

method_name() ->
    <<"tls">>.

allowed_fields() ->
    [<<"target">>, <<"ssl_options">>, <<"client_cert">>, <<"cert_login">>].

-spec validate(map()) -> aws_auth_validate_backend:result().
validate(Body) when is_map(Body) ->
    %% Validate the whole request before touching the network, so a malformed
    %% request never triggers an AssumeRole or an ARN fetch.
    case parse_input(Body) of
        {error, _, _} = Err ->
            Err;
        {ok, Params} ->
            case aws_auth_validate_ssl:resolve_request_state(Params, ssl_opts()) of
                {error, _, _} = Err -> Err;
                {ok, Params1} -> do_tls_validate(Params1)
            end
    end.

%%--------------------------------------------------------------------
%% Input parsing (pure, no network)
%%--------------------------------------------------------------------

parse_input(Body) ->
    Steps = [
        fun parse_target/2,
        fun parse_ssl_options/2,
        fun require_cacert/2,
        fun parse_client_cert/2,
        fun parse_cert_login/2,
        fun cross_field_checks/2
    ],
    run_steps(Steps, Body, #{}).

run_steps([], _Body, Acc) ->
    {ok, Acc};
run_steps([Step | Rest], Body, Acc0) ->
    case Step(Body, Acc0) of
        {ok, Acc1} -> run_steps(Rest, Body, Acc1);
        {error, _, _} = Err -> Err
    end.

%% target is mandatory and must name a known listener.
parse_target(Body, Acc) ->
    case maps:get(<<"target">>, Body, undefined) of
        T when is_binary(T) ->
            case lists:member(T, ?TARGET_VALUES) of
                true -> {ok, Acc#{target => T}};
                false -> {error, input_invalid, ?REASON_BAD_TARGET}
            end;
        _ ->
            {error, input_invalid, ?REASON_BAD_TARGET}
    end.

%% Key and value-shape checks are shared; delegate with this backend's surface.
%% An absent ssl_options yields an empty map, which require_cacert/2 rejects.
parse_ssl_options(Body, Acc) ->
    aws_auth_validate_ssl:parse_ssl_options(
        maps:get(<<"ssl_options">>, Body, undefined), Acc, ssl_opts()
    ).

%% cacertfile_arn is mandatory. Checked after parse_ssl_options so an ill-shaped
%% value reports its own error first.
require_cacert(_Body, #{ssl_options := Map} = Acc) ->
    case maps:is_key(<<"cacertfile_arn">>, Map) of
        true -> {ok, Acc};
        false -> {error, input_invalid, ?REASON_MISSING_CACERT}
    end.

%%--------------------------------------------------------------------
%% Material validation (resolve the ARN, then check the certificates)
%%--------------------------------------------------------------------

%% Resolve the cacertfile ARN, then decode and check the CA bundle. The only
%% network call is the ARN fetch; nothing connects to a listener. When client
%% certificate layers are present, continue into chain/extract/resolve after
%% the CA bundle passes.
do_tls_validate(#{ssl_options := Map} = Params) ->
    %% A request that referenced no ARN carries the `none' sentinel, which
    %% resolve_arn/2 refuses. cacertfile_arn is required, so a valid request
    %% always has a real state; `none' just keeps the failure closed.
    State = maps:get(aws_state, Params, none),
    Arn = maps:get(<<"cacertfile_arn">>, Map),
    case aws_auth_validate_ssl:resolve_arn(Arn, State) of
        {error, _} ->
            {error, input_invalid, ?REASON_ARN_RESOLVE};
        {ok, Pem} ->
            case decode_and_check(Pem) of
                {error, _, _} = Err -> Err;
                {ok, CaDers} -> maybe_chain_validate(Params, CaDers)
            end
    end.

%% Decode the CA PEM and check each certificate. Returns {ok, CaDers} on
%% success so the caller can thread the already-decoded DER list into chain
%% validation without a redundant second decode. The decode is wrapped because
%% public_key:pem_decode/1 raises (rather than returning `skip') on a
%% cert-framed PEM with a malformed base64 body -- one of the misconfigurations
%% this catches -- so it must map to input_invalid, not crash.
decode_and_check(Pem) ->
    Decoded =
        try
            aws_auth_validate_ssl:decode_pem_cacerts(Pem)
        catch
            _Class:_Reason -> error
        end,
    case Decoded of
        error ->
            {error, input_invalid, ?REASON_NO_CERTS};
        skip ->
            {error, input_invalid, ?REASON_NO_CERTS};
        Ders ->
            case check_cert_validity(Ders) of
                ok -> {ok, Ders};
                {error, _, _} = Err -> Err
            end
    end.

%% Check every certificate's [notBefore, notAfter] window against now, failing
%% on the first bad one. Wrapped so a certificate that cannot be parsed maps to
%% input_invalid rather than crashing.
-spec check_cert_validity([binary()]) -> aws_auth_validate_backend:result().
check_cert_validity(Ders) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    try
        check_each(Ders, Now)
    catch
        _Class:_Reason ->
            {error, input_invalid, ?REASON_BAD_CERT}
    end.

check_each([], _Now) ->
    ok;
check_each([Der | Rest], Now) ->
    {NotBefore, NotAfter} = cert_validity_seconds(Der),
    case classify_validity(NotBefore, NotAfter, Now) of
        valid -> check_each(Rest, Now);
        not_yet_valid -> {error, tls_failed, ?REASON_CERT_NOT_YET_VALID};
        expired -> {error, tls_failed, ?REASON_CERT_EXPIRED}
    end.

%% Classify a validity window against a reference time. Separate so the
%% expired/not-yet-valid branches can be tested without a real expired cert.
-spec classify_validity(integer(), integer(), integer()) ->
    valid | not_yet_valid | expired.
classify_validity(NotBefore, _NotAfter, Now) when Now < NotBefore ->
    not_yet_valid;
classify_validity(_NotBefore, NotAfter, Now) when Now > NotAfter ->
    expired;
classify_validity(_NotBefore, _NotAfter, _Now) ->
    valid.

%% Extract {NotBefore, NotAfter} as gregorian seconds from a DER certificate.
%% RFC 5280 requires UTC ("Z") times for these fields, so there is no offset to
%% handle; anything else raises and is caught by check_cert_validity/1.
-spec cert_validity_seconds(binary()) -> {integer(), integer()}.
cert_validity_seconds(Der) ->
    OTPCert = public_key:pkix_decode_cert(Der, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    #'Validity'{notBefore = NotBefore, notAfter = NotAfter} =
        TBS#'OTPTBSCertificate'.validity,
    {asn1_time_to_seconds(NotBefore), asn1_time_to_seconds(NotAfter)}.

%% UTCTime is "YYMMDDHHMMSSZ" with a 2-digit year (RFC 5280: YY >= 50 => 19YY,
%% else 20YY). GeneralizedTime is "YYYYMMDDHHMMSSZ" with a 4-digit year.
%%
%% The fixed 50 pivot is deliberate: RFC 5280 4.1.2.5 requires validity dates
%% through 2049 to be UTCTime and dates in 2050 or later to be GeneralizedTime,
%% so in a compliant certificate a 2-digit year can only mean 1950-2049 and this
%% pivot is exact. Do not replace it with a sliding window relative to the
%% current year (as public_key's pubkey_cert:time_str_2_gregorian_sec/1 does):
%% that only matters for non-compliant certificates that encode a post-2049 date
%% as UTCTime, which this pivot reads as a past year and so rejects as expired --
%% the safe, fail-closed outcome for a pre-flight material check.
asn1_time_to_seconds({utcTime, T}) ->
    S = to_str(T),
    YY = list_to_integer(lists:sublist(S, 1, 2)),
    Year =
        case YY >= 50 of
            true -> 1900 + YY;
            false -> 2000 + YY
        end,
    ymd_to_seconds(Year, lists:nthtail(2, S));
asn1_time_to_seconds({generalTime, T}) ->
    S = to_str(T),
    Year = list_to_integer(lists:sublist(S, 1, 4)),
    ymd_to_seconds(Year, lists:nthtail(4, S)).

%% Rest is "MMDDHHMMSS" (optionally followed by "Z"); take the fixed-width
%% fields positionally.
ymd_to_seconds(Year, Rest) ->
    Month = list_to_integer(lists:sublist(Rest, 1, 2)),
    Day = list_to_integer(lists:sublist(Rest, 3, 2)),
    Hour = list_to_integer(lists:sublist(Rest, 5, 2)),
    Min = list_to_integer(lists:sublist(Rest, 7, 2)),
    Sec = list_to_integer(lists:sublist(Rest, 9, 2)),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}).

to_str(T) when is_binary(T) -> binary_to_list(T);
to_str(T) when is_list(T) -> T.

%%--------------------------------------------------------------------
%% Input parsing: client_cert (optional PEM chain)
%%--------------------------------------------------------------------

%% client_cert is optional; when present it must be a non-empty binary holding
%% only certificate PEM entries (no private key material -- R6).
parse_client_cert(Body, Acc) ->
    case maps:get(<<"client_cert">>, Body, undefined) of
        undefined ->
            {ok, Acc};
        Pem when is_binary(Pem), byte_size(Pem) > 0 ->
            decode_client_cert_pem(Pem, Acc);
        _ ->
            {error, input_invalid, ?REASON_BAD_CLIENT_CERT}
    end.

decode_client_cert_pem(Pem, Acc) ->
    Entries =
        try
            public_key:pem_decode(Pem)
        catch
            _:_ -> error
        end,
    case Entries of
        error ->
            {error, input_invalid, ?REASON_BAD_CLIENT_CERT};
        Decoded when is_list(Decoded) ->
            classify_pem_entries(Decoded, Acc)
    end.

%% Allowlist-structured: only plain Certificate entries are accepted. Any
%% entry whose type is NOT 'Certificate' is rejected immediately -- this
%% closes the class of bypass where an unenumerated key tag (e.g. the tuple
%% {no_asn1, new_openssh} for OpenSSH-format keys) slips past a denylist.
%% ?PRIVATE_KEY_TYPES is retained for message selection: when the offending
%% entry IS a recognizable key type the operator gets the actionable
%% "never send key material" reason; other unexpected entries get a generic
%% non-certificate reason.
classify_pem_entries(Entries, Acc) ->
    case find_non_cert_entry(Entries) of
        none ->
            Ders = [Der || {'Certificate', Der, not_encrypted} <- Entries],
            case Ders of
                [] -> {error, input_invalid, ?REASON_BAD_CLIENT_CERT};
                _ -> {ok, Acc#{client_cert_ders => Ders}}
            end;
        {rejected, Reason} ->
            {error, input_invalid, Reason}
    end.

%% Scan entries for the first non-Certificate entry. Returns `none' if all
%% entries are plain certificates, otherwise {rejected, Reason}.
find_non_cert_entry([]) ->
    none;
find_non_cert_entry([{'Certificate', _Der, not_encrypted} | Rest]) ->
    find_non_cert_entry(Rest);
find_non_cert_entry([{Type, _Der, _Enc} | _Rest]) ->
    case is_known_key_type(Type) of
        true -> {rejected, ?REASON_CLIENT_CERT_PRIVATE_KEY};
        false -> {rejected, ?REASON_CLIENT_CERT_UNEXPECTED_ENTRY}
    end.

%% Returns true when the PEM type tag identifies a private key -- either an
%% atom from the traditional set or the tuple tag used by OpenSSH keys.
%%
%% The {no_asn1, _} clause is reachable at RUNTIME even though dialyzer claims
%% otherwise: public_key's exported pem_entry() type enumerates only the ASN.1
%% record tags, but pubkey_pem emits {no_asn1, new_openssh} for an OpenSSH
%% private key block. Verified on this OTP:
%%
%%   public_key:pem_decode(<<"-----BEGIN OPENSSH PRIVATE KEY-----"...>>)
%%   => [{{no_asn1, new_openssh}, <<...>>, not_encrypted}]
%%
%% So the incomplete upstream spec, not this clause, is the inaccuracy. The
%% clause is what keeps an OpenSSH key from being reported as a generic
%% unexpected entry instead of key material, so it must not be removed. The
%% nowarn is scoped to this function only.
-dialyzer({nowarn_function, is_known_key_type/1}).
is_known_key_type(Type) when is_atom(Type) ->
    lists:member(Type, ?PRIVATE_KEY_TYPES);
is_known_key_type({no_asn1, _}) ->
    true;
is_known_key_type(_) ->
    false.

%%--------------------------------------------------------------------
%% Input parsing: cert_login (optional username-extraction config)
%%--------------------------------------------------------------------

%% cert_login is optional; when present it must be a map with `from' required
%% plus optional `san_type' and `san_index' (only valid in SAN mode).
parse_cert_login(Body, Acc) ->
    case maps:get(<<"cert_login">>, Body, undefined) of
        undefined ->
            {ok, Acc};
        Map when is_map(Map) ->
            validate_cert_login(Map, Acc);
        _ ->
            {error, input_invalid, ?REASON_BAD_CERT_LOGIN}
    end.

validate_cert_login(Map, Acc) ->
    AllowedKeys = [<<"from">>, <<"san_type">>, <<"san_index">>],
    case [K || K <- maps:keys(Map), not lists:member(K, AllowedKeys)] of
        [_ | _] ->
            {error, input_invalid, ?REASON_UNKNOWN_CERT_LOGIN_KEY};
        [] ->
            parse_cert_login_from(Map, Acc)
    end.

parse_cert_login_from(Map, Acc) ->
    case maps:get(<<"from">>, Map, undefined) of
        <<"distinguished_name">> ->
            check_no_san_fields(Map, Acc, distinguished_name);
        <<"common_name">> ->
            check_no_san_fields(Map, Acc, common_name);
        <<"subject_alternative_name">> ->
            parse_san_fields(Map, Acc);
        %% Mirrors the broker's accepted spellings: rabbit_ssl accepts both
        %% subject_alternative_name and subject_alt_name for ssl_cert_login_from.
        <<"subject_alt_name">> ->
            parse_san_fields(Map, Acc);
        _ ->
            {error, input_invalid, ?REASON_BAD_CERT_LOGIN_FROM}
    end.

%% For non-SAN modes, san_type and san_index are not valid.
check_no_san_fields(Map, Acc, From) ->
    case maps:is_key(<<"san_type">>, Map) of
        true ->
            {error, input_invalid, ?REASON_SAN_TYPE_REQUIRES_SAN_FROM};
        false ->
            case maps:is_key(<<"san_index">>, Map) of
                true ->
                    {error, input_invalid, ?REASON_SAN_INDEX_REQUIRES_SAN_FROM};
                false ->
                    {ok, Acc#{cert_login => #{from => From}}}
            end
    end.

%% In SAN mode, validate san_type (required) and san_index (optional, default 0).
parse_san_fields(Map, Acc) ->
    case maps:get(<<"san_type">>, Map, undefined) of
        undefined ->
            {error, input_invalid, ?REASON_BAD_SAN_TYPE};
        TypeBin ->
            case san_type_atom(TypeBin) of
                error ->
                    {error, input_invalid, ?REASON_BAD_SAN_TYPE};
                {ok, TypeAtom} ->
                    parse_san_index(Map, Acc, TypeAtom)
            end
    end.

parse_san_index(Map, Acc, TypeAtom) ->
    case maps:get(<<"san_index">>, Map, 0) of
        I when is_integer(I), I >= 0 ->
            {ok, Acc#{
                cert_login => #{
                    from => subject_alternative_name,
                    san_type => TypeAtom,
                    san_index => I
                }
            }};
        _ ->
            {error, input_invalid, ?REASON_BAD_SAN_INDEX}
    end.

san_type_atom(<<"dns">>) -> {ok, dns};
san_type_atom(<<"ip">>) -> {ok, ip};
san_type_atom(<<"email">>) -> {ok, email};
san_type_atom(<<"uri">>) -> {ok, uri};
san_type_atom(<<"other_name">>) -> {ok, other_name};
san_type_atom(_) -> error.

%%--------------------------------------------------------------------
%% Input parsing: cross-field checks
%%--------------------------------------------------------------------

%% cert_login without client_cert is a conflict; cert_login without verify_peer
%% is unsafe (the chain would not be validated by the broker).
cross_field_checks(_Body, Acc) ->
    case maps:is_key(cert_login, Acc) of
        false ->
            {ok, Acc};
        true ->
            case maps:is_key(client_cert_ders, Acc) of
                false ->
                    {error, config_conflict, ?REASON_CERT_LOGIN_REQUIRES_CLIENT_CERT};
                true ->
                    check_verify_for_cert_login(Acc)
            end
    end.

check_verify_for_cert_login(#{ssl_options := SslMap} = Acc) ->
    case maps:get(<<"verify">>, SslMap, undefined) of
        <<"verify_peer">> -> {ok, Acc};
        _ -> {error, config_conflict, ?REASON_CERT_LOGIN_REQUIRES_VERIFY_PEER}
    end.

%%--------------------------------------------------------------------
%% Layer 1: client certificate chain validation
%%--------------------------------------------------------------------

%% When client_cert_ders is present, validate the chain against the CA bundle.
%% Otherwise the existing CA-only validation already passed and we are done
%% (unless cert_login is present, which cross_field_checks already guarantees
%% cannot happen without client_cert_ders).
maybe_chain_validate(#{client_cert_ders := ClientDers} = Params, CaDers) ->
    Depth = maps:get(<<"depth">>, maps:get(ssl_options, Params, #{}), undefined),
    case validate_chain(ClientDers, CaDers, Depth) of
        ok -> maybe_extract_username(Params);
        {error, _, _} = Err -> Err
    end;
maybe_chain_validate(_Params, _CaDers) ->
    %% No client_cert: existing CA-only validation already passed.
    ok.

%% Validate the client certificate chain against the CA trust anchors using
%% public_key:pkix_path_validation/3. ClientDers is leaf-first (the leaf cert
%% MUST be the first element; the tail may be in any order). CaDers is the
%% trust anchor pool. Depth caps intermediate chain length when set.
%%
%% The implementation rebuilds an ordered chain from the leaf outward by
%% issuer/subject matching (tolerating arbitrarily ordered intermediates in the
%% tail), then finds ALL self-signed bundle certs whose subject matches the
%% top-most cert's issuer, and tries each candidate anchor until one validates.
%% This mirrors the broker's path-iteration behavior on key rollover.
-spec validate_chain([binary()], [binary()], integer() | undefined) ->
    ok | {error, auth_failed | input_invalid, binary()}.
validate_chain(ClientDers, CaDers, Depth) ->
    try
        Opts =
            case Depth of
                undefined -> [];
                N when is_integer(N), N >= 0 -> [{max_path_length, N}]
            end,
        %% Rebuild the chain in issuer-linked order starting from the leaf.
        %% The leaf MUST be element 0; the tail is reordered by issuer linkage.
        Reordered = order_chain(ClientDers),
        %% A listener treats every cacerts entry as chain-building material, not
        %% only as a trust anchor (ssl_certificate:certificate_chain/3), so a
        %% bundle carrying [root, intermediate] completes a client that presents
        %% the leaf alone. Extend the chain with non-self-signed bundle CAs
        %% before searching for an anchor, or that configuration -- which a real
        %% handshake accepts -- would be reported as not chaining.
        OrderedDers = extend_with_bundle_intermediates(Reordered, CaDers),
        %% The top cert (lists:last) is the one closest to (or matching) a
        %% bundle CA. Find anchors based on its issuer. If the top cert itself
        %% is one of the anchors (client included the root in its PEM), strip
        %% it from the path -- pkix_path_validation expects only certs BELOW
        %% the anchor.
        TopCert = lists:last(OrderedDers),
        Anchors = find_trust_anchors(TopCert, CaDers),
        {PathDers, EffectiveAnchors} =
            case Anchors of
                [] ->
                    %% No anchor matches the top cert's issuer -- fail.
                    {OrderedDers, []};
                _ ->
                    case lists:member(TopCert, Anchors) of
                        true ->
                            %% Top cert IS the anchor; validate only the
                            %% certs below it.
                            Below = lists:droplast(OrderedDers),
                            {Below, [TopCert]};
                        false ->
                            {OrderedDers, Anchors}
                    end
            end,
        case EffectiveAnchors of
            [] ->
                {error, auth_failed, ?REASON_CHAIN_FAILED};
            _ ->
                %% pkix_path_validation wants the chain anchor-closest first.
                Chain = lists:reverse(PathDers),
                try_anchors(EffectiveAnchors, Chain, Opts)
        end
    catch
        _:_ -> {error, input_invalid, ?REASON_CHAIN_MALFORMED}
    end.

%% Try each candidate trust anchor; accept if ANY validates. On failure,
%% report the most informative reason (prefer a specific diagnosis over the
%% generic "does not chain" fallback).
try_anchors(Anchors, Chain, Opts) ->
    Results = [
        public_key:pkix_path_validation(A, Chain, Opts)
     || A <- Anchors
    ],
    case
        lists:any(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            Results
        )
    of
        true ->
            ok;
        false ->
            Reasons = [R || {error, {bad_cert, R}} <- Results],
            pick_best_failure(Reasons)
    end.

%% Select the most informative failure from a list of bad_cert reasons.
%% Priority: specific actionable reasons first, generic "does not chain" last.
pick_best_failure([]) ->
    {error, auth_failed, ?REASON_CHAIN_FAILED};
pick_best_failure(Reasons) ->
    Classify = fun(R) ->
        case R of
            cert_expired -> {1, ?REASON_CHAIN_EXPIRED};
            max_path_length_reached -> {2, ?REASON_CHAIN_DEPTH_EXCEEDED};
            invalid_key_usage -> {3, ?REASON_CHAIN_BAD_KEY_USAGE};
            missing_basic_constraint -> {4, ?REASON_CHAIN_MISSING_BASIC_CONSTRAINT};
            invalid_signature -> {5, ?REASON_CHAIN_INVALID_SIGNATURE};
            invalid_issuer -> {6, ?REASON_CHAIN_INVALID_ISSUER};
            _ -> {99, ?REASON_CHAIN_FAILED}
        end
    end,
    Classified = lists:map(Classify, Reasons),
    {_, BestReason} = lists:min(Classified),
    {error, auth_failed, BestReason}.

%% Rebuild the client cert chain in issuer-linked order starting from the leaf
%% (element 0). The tail certs are matched by issuer->subject linkage so an
%% arbitrarily ordered tail (e.g. [leaf, root, int]) still produces the correct
%% ordering [leaf, int, root] as far as the linkage allows.
order_chain([Leaf]) ->
    [Leaf];
order_chain([Leaf | Tail]) ->
    order_chain_loop(Leaf, Tail, [Leaf]).

order_chain_loop(_Current, [], Acc) ->
    lists:reverse(Acc);
order_chain_loop(Current, Remaining, Acc) ->
    CurrOtp = public_key:pkix_decode_cert(Current, otp),
    Issuer = CurrOtp#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer,
    case find_issuer_in(Issuer, Remaining) of
        {ok, IssuerDer, Rest} ->
            order_chain_loop(IssuerDer, Rest, [IssuerDer | Acc]);
        not_found ->
            %% Remaining certs cannot be linked; append them as-is so
            %% pkix_path_validation can reject if they do not belong.
            lists:reverse(Acc) ++ Remaining
    end.

find_issuer_in(_Issuer, []) ->
    not_found;
find_issuer_in(Issuer, [Der | Rest]) ->
    CaOtp = public_key:pkix_decode_cert(Der, otp),
    Subject = CaOtp#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    case public_key:pkix_normalize_name(Issuer) =:= public_key:pkix_normalize_name(Subject) of
        true ->
            {ok, Der, Rest};
        false ->
            case find_issuer_in(Issuer, Rest) of
                {ok, Found, Rem} -> {ok, Found, [Der | Rem]};
                not_found -> not_found
            end
    end.

%% Extend an ordered (leaf-first) chain upward using non-self-signed CAs from
%% the bundle, mirroring how a listener uses cacerts as chain-building material
%% and not only as anchors. Only NON-self-signed entries are appended: a
%% self-signed match is a trust anchor and is handled by find_trust_anchors/2,
%% which also keeps a self-signed cert from being appended to itself.
%%
%% Each appended CA is removed from the candidate pool, so the recursion is
%% bounded by the bundle size and cannot loop on a cross-signed pair.
extend_with_bundle_intermediates(OrderedDers, CaDers) ->
    TopCert = lists:last(OrderedDers),
    TopOtp = public_key:pkix_decode_cert(TopCert, otp),
    Issuer = TopOtp#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer,
    case take_non_self_signed_by_subject(Issuer, CaDers) of
        {ok, IntDer, RestCaDers} ->
            extend_with_bundle_intermediates(OrderedDers ++ [IntDer], RestCaDers);
        not_found ->
            OrderedDers
    end.

%% Take the first non-self-signed bundle CA whose subject matches Issuer,
%% returning it alongside the remaining candidates.
take_non_self_signed_by_subject(_Issuer, []) ->
    not_found;
take_non_self_signed_by_subject(Issuer, [CaDer | Rest]) ->
    CaOtp = public_key:pkix_decode_cert(CaDer, otp),
    Subject = CaOtp#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    Matches =
        public_key:pkix_normalize_name(Issuer) =:=
            public_key:pkix_normalize_name(Subject) andalso
            not public_key:pkix_is_self_signed(CaOtp),
    case Matches of
        true ->
            {ok, CaDer, Rest};
        false ->
            case take_non_self_signed_by_subject(Issuer, Rest) of
                {ok, Found, Rem} -> {ok, Found, [CaDer | Rem]};
                not_found -> not_found
            end
    end.

%% Find ALL trust anchors in CaDers whose subject matches the issuer of
%% TopCertDer AND that are self-signed. A non-self-signed intermediate in the
%% bundle is not a valid trust anchor for pkix_path_validation (the broker's
%% ssl_certificate module only accepts self-signed anchors in the default
%% partial_chain behavior).
find_trust_anchors(TopCertDer, CaDers) ->
    TopOtp = public_key:pkix_decode_cert(TopCertDer, otp),
    Issuer = TopOtp#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer,
    find_all_self_signed_by_subject(Issuer, CaDers).

find_all_self_signed_by_subject(Issuer, CaDers) ->
    [
        CaDer
     || CaDer <- CaDers,
        begin
            CaOtp = public_key:pkix_decode_cert(CaDer, otp),
            Subject = CaOtp#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
            public_key:pkix_normalize_name(Issuer) =:=
                public_key:pkix_normalize_name(Subject) andalso
                public_key:pkix_is_self_signed(CaOtp)
        end
    ].

%%--------------------------------------------------------------------
%% Layer 2: username extraction from the leaf certificate
%%--------------------------------------------------------------------

%% When cert_login is present, extract the username from the leaf cert.
maybe_extract_username(#{cert_login := Login, client_cert_ders := [LeafDer | _]}) ->
    case extract_username(LeafDer, Login) of
        {ok, Username} -> resolve_user(Username);
        {error, _, _} = Err -> Err
    end;
maybe_extract_username(_Params) ->
    %% No cert_login: chain-check-only case (mTLS encryption validation).
    ok.

%% COUPLING NOTE (see the parity-scope discussion in the module header): this
%% function mirrors rabbit_ssl:peer_cert_auth_name/2's three-clause dispatch
%% (distinguished_name | common_name | subject_alternative_name). The extraction
%% itself delegates to rabbit_cert_info (broker code) so the field-extraction
%% logic cannot drift; only this dispatch sequencing and the SAN-type/index
%% selection can. Keep in sync with peer_cert_auth_name/2 if a new mode is added
%% upstream.
-spec extract_username(binary(), map()) ->
    {ok, binary()} | {error, auth_failed, binary()}.
extract_username(LeafDer, #{from := distinguished_name}) ->
    %% Stricter than upstream (which returns an empty binary as a valid name):
    %% an empty subject is treated as extraction failure. This is deliberate for
    %% a validation endpoint -- fail-fast rather than silently accepting a name
    %% that will be rejected later by the auth mechanism.
    Name = iolist_to_binary(rabbit_cert_info:subject(LeafDer)),
    case Name of
        <<>> -> {error, auth_failed, ?REASON_EXTRACT_FAILED};
        _ -> {ok, Name}
    end;
extract_username(LeafDer, #{from := common_name}) ->
    case rabbit_cert_info:subject_items(LeafDer, ?'id-at-commonName') of
        not_found ->
            {error, auth_failed, ?REASON_EXTRACT_FAILED};
        CNs ->
            {ok, list_to_binary(string:join(CNs, ","))}
    end;
extract_username(LeafDer, #{from := subject_alternative_name, san_type := Type, san_index := Index}) ->
    OtpType = otp_san_type(Type),
    SANs = rabbit_cert_info:subject_alternative_names(LeafDer),
    Filtered = [V || {T, V} <- SANs, T =:= OtpType],
    %% Index is 0-based in config, 1-based for lists:nth.
    case length(Filtered) > Index of
        true ->
            Raw = lists:nth(Index + 1, Filtered),
            case maybe_sanitize_other_name(OtpType, Raw) of
                {error, _} ->
                    %% rabbit_cert_info:sanitize_other_name/1 returns an error
                    %% tuple for non-DirectoryString otherName encodings (e.g.
                    %% IA5STRING UPNs). Map to a categorized failure rather than
                    %% crashing in iolist_to_binary.
                    {error, auth_failed, ?REASON_EXTRACT_FAILED};
                Value when is_binary(Value) ->
                    {ok, Value};
                Value when is_list(Value) ->
                    {ok, iolist_to_binary(Value)};
                _Other ->
                    %% Defensive: any non-binary/non-iolist return (unexpected
                    %% upstream shape change) gets a categorized failure.
                    {error, auth_failed, ?REASON_EXTRACT_FAILED}
            end;
        false ->
            {error, auth_failed, ?REASON_EXTRACT_FAILED}
    end.

%% Maps the cert_login san_type atoms to the OTP SAN type tags used by
%% rabbit_cert_info:subject_alternative_names/1.
otp_san_type(dns) -> dNSName;
otp_san_type(ip) -> iPAddress;
otp_san_type(email) -> rfc822Name;
otp_san_type(uri) -> uniformResourceIdentifier;
otp_san_type(other_name) -> otherName.

%% otherName SANs are represented by OTP as {'AnotherName', OID, Value} -- a
%% 3-tuple. rabbit_cert_info:sanitize_other_name/1 expects a binary, so coerce
%% via rabbit_data_coercion:to_binary/1 to match the upstream rabbit_ssl
%% dispatch (rabbit_ssl.erl line ~180).
maybe_sanitize_other_name(otherName, {'AnotherName', _OID, Value}) ->
    rabbit_cert_info:sanitize_other_name(rabbit_data_coercion:to_binary(Value));
maybe_sanitize_other_name(_Type, Value) ->
    Value.

%%--------------------------------------------------------------------
%% Layer 3: user resolution (read-only lookup)
%%--------------------------------------------------------------------

%% Check whether the extracted username resolves to a known broker user.
%%
%% The broker's live EXTERNAL login calls rabbit_access_control:check_user_login/2,
%% which walks ALL configured auth_backends (each entry may be a bare module or a
%% {AuthN, AuthZ} tuple). We can only query rabbit_auth_backend_internal (a local
%% mnesia/khepri read -- no I/O, no state mutation, R3-safe). If other backends
%% are also configured, a not-found in the internal backend does NOT mean the user
%% does not exist -- the broker may authenticate it via LDAP, HTTP, or OAuth.
%%
%% Strategy:
%%   - internal backend unavailable -> config_conflict (cannot check at all).
%%   - user found in internal backend -> ok (definitive positive).
%%   - user NOT found AND internal is the only authN backend -> auth_failed.
%%   - user NOT found AND other authN backends are configured -> config_conflict
%%     with an honest "inconclusive" reason (we cannot check those without I/O).
-spec resolve_user(binary()) -> ok | {error, auth_failed | config_conflict, binary()}.
resolve_user(Username) ->
    case internal_backend_available() of
        false ->
            {error, config_conflict, ?REASON_USER_LOOKUP_UNAVAILABLE};
        true ->
            case ?INTERNAL_BACKEND:exists(Username) of
                true ->
                    ok;
                false ->
                    case only_internal_authn_configured() of
                        true ->
                            {error, auth_failed, ?REASON_USER_NOT_FOUND(Username)};
                        false ->
                            {error, config_conflict, ?REASON_USER_LOOKUP_INCONCLUSIVE(Username)}
                    end
            end
    end.

%% Returns true only when every configured authN backend is
%% rabbit_auth_backend_internal. The auth_backends env entries are either a bare
%% module atom (same module for authN and authZ) or a {AuthN, _AuthZ} tuple.
only_internal_authn_configured() ->
    case application:get_env(rabbit, auth_backends) of
        {ok, Backends} ->
            lists:all(fun is_internal_authn/1, Backends);
        undefined ->
            %% No config at all -- treat as inconclusive (safe side).
            false
    end.

is_internal_authn(?INTERNAL_BACKEND) -> true;
is_internal_authn({?INTERNAL_BACKEND, _AuthZ}) -> true;
is_internal_authn(_) -> false.

internal_backend_available() ->
    module_ready(?INTERNAL_BACKEND) andalso
        erlang:function_exported(?INTERNAL_BACKEND, exists, 1).

module_ready(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} -> true;
        _ -> false
    end.

%% Cap username length and strip control characters, ensuring the result is
%% valid UTF-8 (required for JSON-safe reason binaries in the response). The
%% username derives from the caller's own supplied certificate (R4 basis for
%% echoing it), but we bound it against degenerate inputs. Non-UTF-8 bytes
%% (e.g. raw iPAddress SANs) are hex-escaped so the reason is always encodable.
%%
%% NOTE: iPAddress SANs are kept as raw bytes for the user LOOKUP (matching
%% upstream rabbit_ssl/rabbit_cert_info behavior, which passes the raw value
%% through without inet:ntoa formatting). Formatting it here into dotted-quad
%% would create a parity divergence -- the broker would look up a different
%% username than the endpoint checked. This function only sanitizes the ECHO
%% path (the reason string), not the lookup key.
-spec sanitize_username(binary()) -> binary().
sanitize_username(Name) when is_binary(Name) ->
    %% Truncate on a UTF-8 character boundary: if the input is valid UTF-8,
    %% find the last complete codepoint within ?MAX_USERNAME_LEN bytes. If the
    %% input is not valid UTF-8 at all, truncate at the byte limit (the
    %% subsequent hex-escape pass will handle the non-UTF-8 bytes).
    Capped = truncate_utf8(Name, ?MAX_USERNAME_LEN),
    %% Strip control characters (< 32 or DEL), then ensure UTF-8 validity:
    %% valid codepoints pass through, invalid bytes are hex-escaped as <0xHH>.
    Escaped = ensure_utf8(strip_control(Capped)),
    %% Hex-escaping expands each invalid byte to 6 characters, so an all-invalid
    %% input (e.g. 256 bytes of 0xC0) would otherwise blow past the cap by ~6x.
    %% Re-truncate on a character boundary so ?MAX_USERNAME_LEN bounds the value
    %% that actually reaches the response, which is the point of the cap.
    truncate_utf8(Escaped, ?MAX_USERNAME_LEN).

%% Truncate a binary to at most MaxBytes, respecting UTF-8 character boundaries.
truncate_utf8(Bin, MaxBytes) when byte_size(Bin) =< MaxBytes ->
    Bin;
truncate_utf8(Bin, MaxBytes) ->
    Candidate = binary:part(Bin, 0, MaxBytes),
    %% Walk backwards from the cut point to find a valid UTF-8 boundary.
    %% A UTF-8 continuation byte has the pattern 10xxxxxx (0x80-0xBF).
    trim_trailing_partial_utf8(Candidate).

trim_trailing_partial_utf8(<<>>) ->
    <<>>;
trim_trailing_partial_utf8(Bin) ->
    Size = byte_size(Bin),
    Last = binary:at(Bin, Size - 1),
    case Last of
        B when B < 16#80 ->
            %% ASCII -- boundary is clean.
            Bin;
        B when B >= 16#C0 ->
            %% A lead byte at the very end means the multibyte char was split.
            binary:part(Bin, 0, Size - 1);
        _ ->
            %% Continuation byte -- verify the preceding sequence is complete.
            verify_tail_sequence(Bin)
    end.

%% Walk back over continuation bytes to find the lead byte and check if the
%% sequence is complete (expected length matches actual length).
verify_tail_sequence(Bin) ->
    Size = byte_size(Bin),
    %% Count trailing continuation bytes (max 3 in valid UTF-8).
    ContCount = count_trailing_continuations(Bin, Size - 1, 0),
    LeadPos = Size - 1 - ContCount,
    case LeadPos >= 0 of
        false ->
            %% All continuation bytes, no lead -- not valid, trim all.
            <<>>;
        true ->
            Lead = binary:at(Bin, LeadPos),
            Expected = expected_continuation_count(Lead),
            case Expected =:= ContCount of
                true ->
                    %% Complete sequence.
                    Bin;
                false ->
                    %% Incomplete -- trim back to before the lead byte.
                    binary:part(Bin, 0, LeadPos)
            end
    end.

count_trailing_continuations(_Bin, Pos, Acc) when Pos < 0 ->
    Acc;
count_trailing_continuations(Bin, Pos, Acc) ->
    B = binary:at(Bin, Pos),
    case B >= 16#80 andalso B < 16#C0 of
        true -> count_trailing_continuations(Bin, Pos - 1, Acc + 1);
        false -> Acc
    end.

expected_continuation_count(Lead) when Lead >= 16#F0 -> 3;
expected_continuation_count(Lead) when Lead >= 16#E0 -> 2;
expected_continuation_count(Lead) when Lead >= 16#C0 -> 1;
expected_continuation_count(_) -> 0.

%% Strip ASCII control characters (bytes < 32 and DEL 127) while preserving
%% multibyte sequences intact.
strip_control(Bin) ->
    <<<<C>> || <<C>> <= Bin, C >= 32, C =/= 127>>.

%% Ensure the result is valid UTF-8. Valid codepoints pass through; any byte
%% that does not form part of a valid UTF-8 sequence is hex-escaped as <0xHH>.
ensure_utf8(Bin) ->
    case unicode:characters_to_binary(Bin) of
        Bin -> Bin;
        _ -> hex_escape_non_utf8(Bin, <<>>)
    end.

hex_escape_non_utf8(<<>>, Acc) ->
    Acc;
hex_escape_non_utf8(Bin, Acc) ->
    case unicode:characters_to_binary(Bin) of
        <<>> ->
            Acc;
        Bin ->
            <<Acc/binary, Bin/binary>>;
        {error, Good, <<B, Rest/binary>>} ->
            Hex = list_to_binary(io_lib:format("<0x~2.16.0B>", [B])),
            hex_escape_non_utf8(Rest, <<Acc/binary, Good/binary, Hex/binary>>);
        {error, Good, <<>>} ->
            <<Acc/binary, Good/binary>>;
        {incomplete, Good, <<B, Rest/binary>>} ->
            Hex = list_to_binary(io_lib:format("<0x~2.16.0B>", [B])),
            hex_escape_non_utf8(Rest, <<Acc/binary, Good/binary, Hex/binary>>);
        {incomplete, Good, <<>>} ->
            <<Acc/binary, Good/binary>>
    end.
