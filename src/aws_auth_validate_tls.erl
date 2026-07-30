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
%% there is nothing for the broker to connect to (and no client keystore to
%% connect with). Checks:
%%   1. the cacertfile ARN resolves,
%%   2. the resolved PEM holds at least one well-formed CA certificate,
%%   3. none of those certificates is expired or not yet valid,
%%   4. verify / fail_if_no_peer_cert / depth / versions are well-shaped.
%%
%% A pass means the material is usable, not that mTLS as a whole works: it does
%% not cover client-cert chaining, common-name-to-user mapping, network
%% reachability, or whether the broker is actually running this config.
%%
%% Result categories (shared with the other backends):
%%   * input_invalid (400) -- bad target/ssl_options, missing cacertfile_arn,
%%     ARN resolve failure, or a PEM with no parseable CA certificate.
%%   * tls_failed (400) -- a CA certificate is expired or not yet valid.
%%   * config_conflict (422) -- a cacertfile_arn is given but no
%%     aws.arns.assume_role_arn is configured.
-module(aws_auth_validate_tls).

-behaviour(aws_auth_validate_backend).

-export([method_name/0, validate/1, allowed_fields/0]).

-ifdef(TEST).
%% Exposed for the unit tests: the pure input parser and the certificate-validity
%% helpers (classify_validity/3 lets the expired/not-yet-valid branches be tested
%% without generating an actually-expired cert).
-export([
    parse_input/1,
    check_cert_validity/1,
    cert_validity_seconds/1,
    classify_validity/3
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
%% Names the field whose ARN failed to resolve. The field name comes from the
%% caller's own request, so it leaks nothing; the resolved content and the
%% underlying AWS error are never included.
-define(REASON_ARN_RESOLVE,
    aws_auth_validate_ssl:arn_resolve_reason([<<"ssl_options.cacertfile_arn">>])
).
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
    [<<"target">>, <<"ssl_options">>].

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
        fun require_cacert/2
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
%% network call is the ARN fetch; nothing connects to a listener.
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
            decode_and_check(Pem)
    end.

%% This backend takes exactly one ARN, so there is never more than one field to
%% attribute; the shared aggregating resolver is unnecessary here.

%% Decode the CA PEM and check each certificate. The decode is wrapped because
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
        error -> {error, input_invalid, ?REASON_NO_CERTS};
        skip -> {error, input_invalid, ?REASON_NO_CERTS};
        Ders -> check_cert_validity(Ders)
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
