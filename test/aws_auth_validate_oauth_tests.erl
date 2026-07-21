%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for the OAuth 2.0 auth-validation backend's pure phase
%% (no real network). Covers URL parsing, SSRF classification, ssl_options
%% validation, and mocked JWKS/OIDC well-formedness checks.
-module(aws_auth_validate_oauth_tests).

-include_lib("eunit/include/eunit.hrl").

%% Shared JWT-minting and httpc-request helpers (see the helper module). Imported
%% so the existing rsa_signer/1 and request_url/1 call sites need no changes.
-import(aws_auth_validate_oauth_test_helpers, [rsa_signer/1, request_url/1]).

%%--------------------------------------------------------------------
%% Method identity
%%--------------------------------------------------------------------

method_name_test() ->
    ?assertEqual(<<"oauth">>, aws_auth_validate_oauth:method_name()).

allowed_fields_test() ->
    Fields = aws_auth_validate_oauth:allowed_fields(),
    [
        ?assert(lists:member(F, Fields))
     || F <- [
            <<"jwks_uri">>,
            <<"issuer">>,
            <<"resource_server_id">>,
            <<"ssl_options">>
        ]
    ].

%%--------------------------------------------------------------------
%% Pure phase: URL presence / scheme enforcement
%%--------------------------------------------------------------------

%% At least one of jwks_uri or issuer is required.
missing_both_jwks_uri_and_issuer_test() ->
    Body = #{<<"resource_server_id">> => <<"my-api">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A plain http:// jwks_uri is rejected (https-only for signing keys).
http_jwks_uri_rejected_test() ->
    Body = #{<<"jwks_uri">> => <<"http://idp.example.com/.well-known/jwks.json">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A plain http:// issuer is rejected.
http_issuer_rejected_test() ->
    Body = #{<<"issuer">> => <<"http://idp.example.com">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% An empty string jwks_uri is rejected.
empty_jwks_uri_rejected_test() ->
    Body = #{<<"jwks_uri">> => <<>>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% An empty string issuer is rejected.
empty_issuer_rejected_test() ->
    Body = #{<<"issuer">> => <<>>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A non-string jwks_uri value is rejected.
non_string_jwks_uri_rejected_test() ->
    Body = #{<<"jwks_uri">> => 12345},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%%--------------------------------------------------------------------
%% Pure phase: URL shape rejections
%%--------------------------------------------------------------------

%% A URL with userinfo (user:pass@host) is rejected.
url_with_userinfo_rejected_test() ->
    Body = #{<<"jwks_uri">> => <<"https://admin:secret@idp.example.com/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A jwks_uri with a pre-existing query string is ACCEPTED in the pure phase: it
%% is fetched verbatim (the live broker's uaa_jwks:get/2 does the same), so we
%% must not reject a config the broker would accept. Exercised via the pure
%% parse_input/1 (no network) so it stays deterministic; the query survives into
%% the parsed jwks_uri.
jwks_uri_with_query_allowed_test() ->
    Body = #{<<"jwks_uri">> => <<"https://idp.example.com/jwks?foo=bar">>},
    ?assertMatch({ok, #{jwks_uri := #{}}}, aws_auth_validate_oauth:parse_input(Body)).

%% An issuer with a pre-existing query string is still rejected: OIDC discovery
%% appends the well-known path to it, so a query there is ambiguous.
issuer_with_query_rejected_test() ->
    Body = #{<<"issuer">> => <<"https://idp.example.com?foo=bar">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:parse_input(Body)).

%% A jwks_uri with a #fragment is ACCEPTED: the URL is fetched verbatim (httpc
%% drops the fragment on the wire), so rejecting it would refuse a config the
%% broker accepts. Pins the oauth backend's fragment-permissive behavior against
%% the shared parser, whose default is to reject fragments (as http requires).
jwks_uri_with_fragment_allowed_test() ->
    Body = #{<<"jwks_uri">> => <<"https://idp.example.com/jwks#kid">>},
    ?assertMatch({ok, #{jwks_uri := #{}}}, aws_auth_validate_oauth:parse_input(Body)).

%% An issuer with a #fragment is REJECTED, unlike jwks_uri: OIDC discovery appends
%% the well-known path to the issuer, which would land AFTER the '#' (httpc then
%% drops it), so the discovery URL would be mangled exactly as a pre-existing
%% query string mangles it.
issuer_with_fragment_rejected_test() ->
    Body = #{<<"issuer">> => <<"https://idp.example.com#frag">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:parse_input(Body)).

%% A URL with an out-of-range port (0) is rejected.
url_port_zero_rejected_test() ->
    Body = #{<<"jwks_uri">> => <<"https://idp.example.com:0/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A URL with an out-of-range port (>65535) is rejected.
url_port_too_high_rejected_test() ->
    Body = #{<<"jwks_uri">> => <<"https://idp.example.com:70000/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A URL with an unparseable structure is rejected.
url_garbage_rejected_test() ->
    Body = #{<<"jwks_uri">> => <<"not a url at all">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%%--------------------------------------------------------------------
%% Pure phase: SSRF literal-IP classification
%%--------------------------------------------------------------------

%% 169.254.169.254 (IMDS) is denied.
ssrf_imds_v4_test() ->
    Body = #{<<"jwks_uri">> => <<"https://169.254.169.254/latest/meta-data/">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% 127.0.0.1 (loopback) is denied.
ssrf_loopback_v4_test() ->
    Body = #{<<"jwks_uri">> => <<"https://127.0.0.1/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% ::1 (IPv6 loopback) is denied.
ssrf_loopback_v6_test() ->
    Body = #{<<"jwks_uri">> => <<"https://[::1]/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% IPv4-mapped IMDS (::ffff:169.254.169.254) is denied.
ssrf_v4_mapped_imds_test() ->
    Body = #{<<"jwks_uri">> => <<"https://[::ffff:169.254.169.254]/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% fd00:ec2::254 (IPv6 IMDS) is denied.
ssrf_v6_imds_test() ->
    Body = #{<<"jwks_uri">> => <<"https://[fd00:ec2::254]/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% fe80::1 (link-local) is denied.
ssrf_link_local_v6_test() ->
    Body = #{<<"jwks_uri">> => <<"https://[fe80::1]/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% 0.0.0.0 (unspecified) is denied.
ssrf_unspecified_v4_test() ->
    Body = #{<<"jwks_uri">> => <<"https://0.0.0.0/jwks">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% The SSRF guard also applies to the issuer URL.
ssrf_issuer_imds_test() ->
    Body = #{<<"issuer">> => <<"https://169.254.169.254">>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% The auth_validation_allow_private_networks flag (test-only, default false)
%% relaxes ONLY loopback so a local integration stub is reachable. With the flag
%% on, 127.0.0.1 passes the pure SSRF guard (classify_ip -> allow); IMDS stays
%% denied even with the flag on.
loopback_allowed_with_private_networks_flag_test() ->
    application:set_env(aws, auth_validation_allow_private_networks, true),
    try
        ?assertEqual(allow, aws_auth_validate_oauth:classify_ip({127, 0, 0, 1})),
        ?assertEqual(allow, aws_auth_validate_oauth:classify_ip({0, 0, 0, 0, 0, 0, 0, 1})),
        %% IMDS is NOT relaxed by the flag.
        ?assertEqual(deny, aws_auth_validate_oauth:classify_ip({169, 254, 169, 254}))
    after
        application:unset_env(aws, auth_validation_allow_private_networks)
    end.

%% With the flag OFF (production default), loopback stays denied.
loopback_denied_without_flag_test() ->
    application:unset_env(aws, auth_validation_allow_private_networks),
    ?assertEqual(deny, aws_auth_validate_oauth:classify_ip({127, 0, 0, 1})),
    ?assertEqual(deny, aws_auth_validate_oauth:classify_ip({0, 0, 0, 0, 0, 0, 0, 1})).

%%--------------------------------------------------------------------
%% Pure phase: resource_server_id validation
%%--------------------------------------------------------------------

%% resource_server_id, if present, must be a non-empty string.
resource_server_id_empty_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"resource_server_id">> => <<>>
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A non-string resource_server_id is rejected.
resource_server_id_non_string_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"resource_server_id">> => 42
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%%--------------------------------------------------------------------
%% Pure phase: ssl_options validation
%%--------------------------------------------------------------------

%% ssl_options must be an object (map), not a scalar.
ssl_options_not_map_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => <<"bad">>
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% An unknown ssl_options key is rejected.
ssl_options_unknown_key_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"unknown_key">> => <<"value">>}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% verify must be verify_peer or verify_none.
ssl_options_bad_verify_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"verify">> => <<"verfy_peer">>}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% depth must be a non-negative integer.
ssl_options_bad_depth_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"depth">> => -1}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% versions must be a list of known TLS versions.
ssl_options_bad_versions_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"versions">> => [<<"sslv3">>]}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% sni must be a non-empty string.
ssl_options_bad_sni_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"sni">> => <<>>}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% certfile_arn and keyfile_arn must be supplied together -- one without the
%% other is rejected.
ssl_options_cert_without_key_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"certfile_arn">> => <<"arn:aws:s3:::bucket/cert.pem">>}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

ssl_options_key_without_cert_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{
            <<"keyfile_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:k">>
        }
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% cacertfile_arn must be a non-empty string.
ssl_options_bad_cacert_arn_rejected_test() ->
    Body = #{
        <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => <<>>}
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%%--------------------------------------------------------------------
%% Network phase (mocked): JWKS well-formedness
%%--------------------------------------------------------------------

%% A 200 response with {"keys":[{...}]} is well-formed -> ok.
jwks_valid_200_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        JwksBody = rabbit_json:encode(#{
            <<"keys">> => [
                #{<<"kty">> => <<"RSA">>, <<"kid">> => <<"key1">>}
            ]
        }),
        mock_httpc_response(200, JwksBody),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        [?_assertEqual(ok, Result)]
    end}.

%% A 200 with {"keys":[]} (empty array) -> auth_failed.
jwks_empty_keys_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        JwksBody = rabbit_json:encode(#{<<"keys">> => []}),
        mock_httpc_response(200, JwksBody),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        [?_assertMatch({error, auth_failed, _}, Result)]
    end}.

%% A 200 with a non-JWKS body (no "keys" field) -> auth_failed.
jwks_non_jwks_body_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        NotJwks = rabbit_json:encode(#{<<"something">> => <<"else">>}),
        mock_httpc_response(200, NotJwks),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        [?_assertMatch({error, auth_failed, _}, Result)]
    end}.

%% A non-200 HTTP response -> auth_failed (endpoint reachable but not
%% serving a JWKS).
jwks_non_200_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        mock_httpc_response(403, <<"Forbidden">>),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        [?_assertMatch({error, auth_failed, _}, Result)]
    end}.

%% An httpc transport error -> connection_failed.
jwks_connection_error_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        meck:expect(httpc, request, fun(_Method, _Req, _HttpOpts, _Opts, _Profile) ->
            {error,
                {failed_connect, [{to_address, {"idp.example.com", 443}}, {inet, [], econnrefused}]}}
        end),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        [?_assertMatch({error, connection_failed, _}, Result)]
    end}.

%%--------------------------------------------------------------------
%% Network phase (mocked): OIDC discovery
%%--------------------------------------------------------------------

%% When only issuer is given, the backend fetches .well-known/openid-configuration
%% and extracts jwks_uri. A discovery doc lacking jwks_uri -> auth_failed.
oidc_discovery_missing_jwks_uri_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        %% First request = discovery, returns a doc without jwks_uri.
        DiscoveryBody = rabbit_json:encode(#{
            <<"issuer">> => <<"https://idp.example.com">>,
            <<"authorization_endpoint">> => <<"https://idp.example.com/authorize">>
        }),
        meck:expect(httpc, request, fun(_Method, _Req, _HttpOpts, _Opts, _Profile) ->
            {ok, {{"HTTP/1.1", 200, "OK"}, [], DiscoveryBody}}
        end),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(issuer_body()),
        [?_assertMatch({error, auth_failed, _}, Result)]
    end}.

%% A valid discovery doc with jwks_uri pointing to a valid JWKS -> ok.
oidc_discovery_success_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        JwksBody = rabbit_json:encode(#{
            <<"keys">> => [#{<<"kty">> => <<"RSA">>, <<"kid">> => <<"k1">>}]
        }),
        DiscoveryBody = rabbit_json:encode(#{
            <<"issuer">> => <<"https://idp.example.com">>,
            <<"jwks_uri">> => <<"https://idp.example.com/.well-known/jwks.json">>
        }),
        %% Sequence: first call = discovery, second call = JWKS fetch.
        meck:expect(httpc, request, fun(_Method, Req, _HttpOpts, _Opts, _Profile) ->
            Url = request_url(Req),
            case string:find(Url, "openid-configuration") of
                nomatch ->
                    {ok, {{"HTTP/1.1", 200, "OK"}, [], JwksBody}};
                _ ->
                    {ok, {{"HTTP/1.1", 200, "OK"}, [], DiscoveryBody}}
            end
        end),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(issuer_body()),
        [?_assertEqual(ok, Result)]
    end}.

%% A discovery doc whose jwks_uri points at IMDS is rejected, not dereferenced.
oidc_discovery_jwks_uri_ssrf_denied_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        JwksBody = rabbit_json:encode(#{
            <<"keys">> => [#{<<"kty">> => <<"RSA">>, <<"kid">> => <<"k1">>}]
        }),
        DiscoveryBody = rabbit_json:encode(#{
            <<"issuer">> => <<"https://idp.example.com">>,
            <<"jwks_uri">> => <<"https://169.254.169.254/latest/meta-data/jwks">>
        }),
        %% Sequence: first call = discovery, second call = JWKS fetch (never
        %% reached if the SSRF guard rejects the derived jwks_uri).
        meck:expect(httpc, request, fun(_Method, Req, _HttpOpts, _Opts, _Profile) ->
            Url = request_url(Req),
            case string:find(Url, "openid-configuration") of
                nomatch -> {ok, {{"HTTP/1.1", 200, "OK"}, [], JwksBody}};
                _ -> {ok, {{"HTTP/1.1", 200, "OK"}, [], DiscoveryBody}}
            end
        end),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(issuer_body()),
        [
            ?_assertMatch({error, input_invalid, _}, Result),
            ?_assertEqual(1, meck:num_calls(httpc, request, '_'))
        ]
    end}.

%%--------------------------------------------------------------------
%% R6: no secret leakage in error results
%%--------------------------------------------------------------------

%% A network-phase error must never include resolved secret material.
no_secret_in_error_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        %% Simulate a TLS failure that includes secret bytes in the error term.
        meck:expect(httpc, request, fun(_Method, _Req, _HttpOpts, _Opts, _Profile) ->
            {error, {failed_connect, [{tls_alert, {certificate_expired, "SECRET-PEM-DATA"}}]}}
        end),
        mock_arn_resolve_noop(),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        Rendered = lists:flatten(io_lib:format("~p", [Result])),
        [
            ?_assertMatch({error, tls_failed, _}, Result),
            ?_assertEqual(nomatch, string:find(Rendered, "SECRET-PEM-DATA"))
        ]
    end}.

%%--------------------------------------------------------------------
%% assume_role gating: resolving an ssl_options ARN requires a configured
%% aws.arns.assume_role_arn; the broker instance role is never used.
%%--------------------------------------------------------------------

%% A request that references a cacertfile_arn with NO configured assume_role is
%% refused with config_conflict BEFORE any ARN resolve or outbound request.
arn_without_assume_role_is_config_conflict_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        %% Ensure no assume_role is configured.
        application:unset_env(aws, arn_config),
        %% resolve_arn must NOT be reached; make it fail loudly if it is.
        catch meck:unload(aws_arn_util),
        ok = meck:new(aws_arn_util, [passthrough]),
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, _State) ->
            erlang:error(arn_resolved_without_assume_role)
        end),
        Body = #{
            <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
            <<"ssl_options">> => #{
                <<"cacertfile_arn">> => <<"arn:aws:s3:::bucket/ca.pem">>,
                <<"verify">> => <<"verify_peer">>
            }
        },
        Result = aws_auth_validate_oauth:validate(Body),
        [
            ?_assertMatch({error, config_conflict, _}, Result),
            ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_'))
        ]
    end}.

%% A request that references a cacertfile_arn WITH a configured assume_role
%% assumes the role and resolves the ARN under it; a valid JWKS then yields ok.
arn_with_assume_role_resolves_and_succeeds_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        application:set_env(aws, arn_config, [
            {assume_role_arn, "arn:aws:iam::123456789012:role/r"}
        ]),
        ok = meck:new(aws_iam, [no_link]),
        meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
        %% Resolve the CA ARN to a real (empty-decode) PEM sentinel; decode_pem
        %% returns skip on a non-PEM binary, so no bogus cacerts enter the opts.
        catch meck:unload(aws_arn_util),
        ok = meck:new(aws_arn_util, [passthrough]),
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
            {ok, <<"not-a-pem">>, State}
        end),
        JwksBody = rabbit_json:encode(#{
            <<"keys">> => [#{<<"kty">> => <<"RSA">>, <<"kid">> => <<"k1">>}]
        }),
        mock_httpc_response(200, JwksBody),
        Body = #{
            <<"jwks_uri">> => <<"https://idp.example.com/jwks">>,
            <<"ssl_options">> => #{
                <<"cacertfile_arn">> => <<"arn:aws:s3:::bucket/ca.pem">>
            }
        },
        Result = aws_auth_validate_oauth:validate(Body),
        Assumed = meck:num_calls(aws_iam, assume_role, '_'),
        Resolved = meck:num_calls(aws_arn_util, resolve_arn, '_'),
        catch meck:unload(aws_iam),
        application:unset_env(aws, arn_config),
        [
            ?_assertEqual(ok, Result),
            ?_assert(Assumed >= 1),
            ?_assert(Resolved >= 1)
        ]
    end}.

%% A pure reachability request that references NO ARN must still succeed with no
%% assume_role configured (the credential-free JWKS reachability check).
no_arn_needs_no_assume_role_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        application:unset_env(aws, arn_config),
        ok = meck:new(aws_iam, [no_link]),
        meck:expect(aws_iam, assume_role, fun(_RoleArn, _State) ->
            erlang:error(assume_role_called_without_arn)
        end),
        JwksBody = rabbit_json:encode(#{
            <<"keys">> => [#{<<"kty">> => <<"RSA">>, <<"kid">> => <<"k1">>}]
        }),
        mock_httpc_response(200, JwksBody),
        Result = aws_auth_validate_oauth:validate(jwks_body()),
        Assumed = meck:num_calls(aws_iam, assume_role, '_'),
        catch meck:unload(aws_iam),
        [
            ?_assertEqual(ok, Result),
            ?_assertEqual(0, Assumed)
        ]
    end}.

%%--------------------------------------------------------------------
%% Customer-supplied access_token verification: pure phase
%%--------------------------------------------------------------------

%% A non-binary / empty access_token is rejected as input_invalid.
access_token_empty_rejected_test() ->
    Body = (jwks_body())#{<<"access_token">> => <<>>},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A token that is not three non-empty dot-separated segments is malformed.
parse_access_token_not_three_segments_test() ->
    ?assertMatch(
        {error, input_invalid, _},
        aws_auth_validate_oauth:parse_access_token(<<"only.two">>)
    ).

%% A token whose header is not base64url JSON is malformed.
parse_access_token_bad_header_test() ->
    ?assertMatch(
        {error, input_invalid, _},
        aws_auth_validate_oauth:parse_access_token(<<"!!!.payload.sig">>)
    ).

%% alg:none is refused in the pure phase (alg-confusion defense).
parse_access_token_alg_none_rejected_test() ->
    Token = compact_token(#{<<"alg">> => <<"none">>, <<"typ">> => <<"JWT">>}, #{}, <<>>),
    ?assertMatch(
        {error, input_invalid, _},
        aws_auth_validate_oauth:parse_access_token(Token)
    ).

%% An HMAC alg (HS256) is refused in the pure phase: a JWKS holds only public
%% keys, so accepting HS* would enable the public-key-as-HMAC-secret forgery.
parse_access_token_hs256_rejected_test() ->
    Token = compact_token(#{<<"alg">> => <<"HS256">>}, #{<<"sub">> => <<"s">>}, <<"sig">>),
    ?assertMatch(
        {error, input_invalid, _},
        aws_auth_validate_oauth:parse_access_token(Token)
    ).

%% A well-formed RS256 header is accepted in the pure phase; the header
%% (including kid) is returned for the verification step.
parse_access_token_rs256_ok_test() ->
    Token = compact_token(
        #{<<"alg">> => <<"RS256">>, <<"kid">> => <<"k1">>}, #{<<"sub">> => <<"s">>}, <<"sig">>
    ),
    ?assertMatch(
        {ok, #{<<"alg">> := <<"RS256">>, <<"kid">> := <<"k1">>}},
        aws_auth_validate_oauth:parse_access_token(Token)
    ).

%% A present-but-non-binary kid (e.g. a JSON number) is a malformed header,
%% rejected in the pure phase rather than crashing select_jwk/2 later.
parse_access_token_non_binary_kid_rejected_test() ->
    Token = compact_token(
        #{<<"alg">> => <<"RS256">>, <<"kid">> => 123}, #{<<"sub">> => <<"s">>}, <<"sig">>
    ),
    ?assertMatch(
        {error, input_invalid, _},
        aws_auth_validate_oauth:parse_access_token(Token)
    ).

%% select_jwk: a token with NO kid is rejected even against a single-key JWKS.
%% The broker resolves no-kid tokens via a configured default_key (not "the only
%% key present"), so accepting a lone JWKS key here would be a false pass.
select_jwk_single_no_kid_rejected_test() ->
    Key = #{<<"kty">> => <<"RSA">>},
    ?assertMatch(
        {error, token_invalid, _}, aws_auth_validate_oauth:select_jwk(undefined, [Key])
    ).

%% select_jwk: no kid against a multi-key JWKS is likewise rejected -> token_invalid.
select_jwk_no_kid_multi_test() ->
    Keys = [#{<<"kid">> => <<"a">>}, #{<<"kid">> => <<"b">>}],
    ?assertMatch({error, token_invalid, _}, aws_auth_validate_oauth:select_jwk(undefined, Keys)).

%% select_jwk: a kid with no match -> token_invalid.
select_jwk_no_match_test() ->
    Keys = [#{<<"kid">> => <<"a">>}],
    ?assertMatch({error, token_invalid, _}, aws_auth_validate_oauth:select_jwk(<<"zzz">>, Keys)).

%%--------------------------------------------------------------------
%% Customer-supplied access_token verification: signature + claims
%%--------------------------------------------------------------------

%% A validly RS256-signed token verifies against the matching JWKS key.
verify_token_valid_signature_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => future()}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertEqual(
        ok,
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
    ).

%% A token signed by a DIFFERENT key fails signature verification -> token_invalid.
verify_token_wrong_key_test() ->
    #{sign := Sign} = rsa_signer(<<"k1">>),
    #{jwk_pub := OtherPub} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => future()}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertMatch(
        {error, token_invalid, _},
        aws_auth_validate_oauth:verify_token(Token, Header, {[OtherPub], undefined})
    ).

%% An expired token (exp in the past) is refused even with a valid signature
%% -> token_expired (transient, distinct from a config mismatch).
verify_token_expired_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => past()}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertMatch(
        {error, token_expired, _},
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
    ).

%% A token with a future nbf is ACCEPTED (given a valid signature + exp): the
%% broker's validate_token_expiry/1 checks exp only and has no nbf handling, so
%% we deliberately do not check nbf either (checking it would false-fail a
%% post-dated / clock-skewed token the live broker accepts).
verify_token_future_nbf_ignored_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>, <<"nbf">> => future(), <<"exp">> => future()}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertEqual(
        ok,
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
    ).

%% A present but non-numeric exp is a malformed claim -> token_invalid, NOT
%% token_expired (the token is invalid, not merely expired).
verify_token_non_numeric_exp_is_invalid_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => <<"not-a-number">>}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertMatch(
        {error, token_invalid, _},
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
    ).

%% A non-numeric nbf is ignored (nbf is not checked at all -- see
%% verify_token_future_nbf_ignored_test): a valid signature + exp still passes.
verify_token_non_numeric_nbf_ignored_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => future(), <<"nbf">> => <<"soon">>}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertEqual(
        ok,
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
    ).

%% A token with NO exp is REJECTED by default (require_exp unset), matching the
%% broker whose require_exp default is true -> token_invalid (a config mismatch
%% the live broker would also reject, not a transient expiry).
verify_token_no_exp_rejected_by_default_test() ->
    application:unset_env(rabbitmq_auth_backend_oauth2, require_exp),
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"sub">> => <<"alice">>}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertMatch(
        {error, token_invalid, _},
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
    ).

%% A token with NO exp is accepted only when the operator explicitly sets
%% auth_oauth2.require_exp to false, mirroring the broker's opt-out.
verify_token_no_exp_ok_when_not_required_test() ->
    application:set_env(rabbitmq_auth_backend_oauth2, require_exp, false),
    try
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"alice">>}),
        {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
        ?assertEqual(
            ok,
            aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
        )
    after
        application:unset_env(rabbitmq_auth_backend_oauth2, require_exp)
    end.

%% When the server sets auth_oauth2.require_exp, a token with NO exp is refused
%% -> token_invalid (the live broker would also reject it -- a config mismatch,
%% not a transient expiry). Mirrors rabbit_auth_backend_oauth2's require_exp.
verify_token_no_exp_rejected_when_required_test() ->
    application:set_env(rabbitmq_auth_backend_oauth2, require_exp, true),
    try
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"alice">>}),
        {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
        ?assertMatch(
            {error, token_invalid, _},
            aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
        )
    after
        application:unset_env(rabbitmq_auth_backend_oauth2, require_exp)
    end.

%% require_exp only affects the ABSENT-exp case: a token WITH a valid exp still
%% passes when the option is on.
verify_token_present_exp_ok_when_required_test() ->
    application:set_env(rabbitmq_auth_backend_oauth2, require_exp, true),
    try
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => future()}),
        {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
        ?assertEqual(
            ok,
            aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], undefined})
        )
    after
        application:unset_env(rabbitmq_auth_backend_oauth2, require_exp)
    end.

%% aud check: resource_server_id present in a list aud -> ok.
verify_token_audience_ok_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future(), <<"aud">> => [<<"rabbitmq">>, <<"other">>]}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertEqual(
        ok,
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], <<"rabbitmq">>})
    ).

%% aud check: resource_server_id absent from aud -> auth_failed.
verify_token_audience_mismatch_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future(), <<"aud">> => <<"someone-else">>}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertMatch(
        {error, auth_failed, _},
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], <<"rabbitmq">>})
    ).

%% aud check: a space-delimited string aud is split before matching, mirroring
%% the broker's find_audience/2 -> resource_server_id present in the split -> ok.
verify_token_audience_space_delimited_string_test() ->
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future(), <<"aud">> => <<"rabbitmq api://default">>}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertEqual(
        ok,
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], <<"rabbitmq">>})
    ).

%% aud check: when the operator disables the broker's verify_aud, a token whose
%% aud OMITS the resource_server_id is accepted -- the live broker skips audience
%% matching (falls back to find_unique_resource_server_without_verify_aud), so
%% enforcing it here would false-fail a token the broker accepts.
verify_token_audience_skipped_when_verify_aud_false_test() ->
    application:set_env(rabbitmq_auth_backend_oauth2, verify_aud, false),
    try
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"exp">> => future(), <<"aud">> => <<"someone-else">>}),
        {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
        ?assertEqual(
            ok,
            aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], <<"rabbitmq">>})
        )
    after
        application:unset_env(rabbitmq_auth_backend_oauth2, verify_aud)
    end.

%% aud check: with verify_aud at its default (unset -> true), an aud mismatch is
%% still a failure (guards against the verify_aud=false path leaking into the
%% default case).
verify_token_audience_enforced_by_default_test() ->
    application:unset_env(rabbitmq_auth_backend_oauth2, verify_aud),
    #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future(), <<"aud">> => <<"someone-else">>}),
    {ok, Header} = aws_auth_validate_oauth:parse_access_token(Token),
    ?assertMatch(
        {error, auth_failed, _},
        aws_auth_validate_oauth:verify_token(Token, Header, {[PubJwk], <<"rabbitmq">>})
    ).

%%--------------------------------------------------------------------
%% Customer-supplied access_token: end-to-end (mocked JWKS fetch)
%%--------------------------------------------------------------------

%% Valid JWKS reachable AND a supplied token that verifies against it -> ok.
%% No secret / assume_role is involved (the customer minted the token).
access_token_end_to_end_ok_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => future()}),
        JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
        mock_httpc_response(200, JwksBody),
        Body = (jwks_body())#{<<"access_token">> => Token},
        [?_assertEqual(ok, aws_auth_validate_oauth:validate(Body))]
    end}.

%% Valid JWKS but the supplied token is signed by a different key -> token_invalid.
access_token_end_to_end_bad_signature_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        #{sign := Sign} = rsa_signer(<<"k1">>),
        #{jwk_pub := OtherPub} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => future()}),
        JwksBody = rabbit_json:encode(#{<<"keys">> => [OtherPub]}),
        mock_httpc_response(200, JwksBody),
        Body = (jwks_body())#{<<"access_token">> => Token},
        [?_assertMatch({error, token_invalid, _}, aws_auth_validate_oauth:validate(Body))]
    end}.

%% Valid JWKS, valid signature, but the token is expired -> token_expired
%% (end-to-end, so the category-not-message distinction is exercised through
%% the full validate/1 path an operator would hit).
access_token_end_to_end_expired_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"alice">>, <<"exp">> => past()}),
        JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
        mock_httpc_response(200, JwksBody),
        Body = (jwks_body())#{<<"access_token">> => Token},
        [?_assertMatch({error, token_expired, _}, aws_auth_validate_oauth:validate(Body))]
    end}.

%% R6: a valid supplied token's claims must not leak into the rendered result.
access_token_no_leak_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
        Token = Sign(#{<<"sub">> => <<"SECRET-SUBJECT-CLAIM">>, <<"exp">> => future()}),
        JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
        mock_httpc_response(200, JwksBody),
        Body = (jwks_body())#{<<"access_token">> => Token},
        Result = aws_auth_validate_oauth:validate(Body),
        Rendered = lists:flatten(io_lib:format("~p", [Result])),
        [
            ?_assertEqual(ok, Result),
            ?_assertEqual(nomatch, string:find(Rendered, "SECRET-SUBJECT-CLAIM"))
        ]
    end}.

%%--------------------------------------------------------------------
%% Optional authorization-evaluation layer (runtime soft dependency
%% on rabbitmq_auth_backend_oauth2). These tests only run when that backend is
%% loaded on the node (it is a TEST_DEPS-style presence, not a build DEPS);
%% each test skips gracefully via availability/0 so the suite passes whether or
%% not the oauth2 modules are present.
%%--------------------------------------------------------------------

%% The soft-dependency probe returns a boolean and never raises, regardless of
%% whether the oauth2 backend is loaded.
authz_available_is_boolean_test() ->
    ?assert(is_boolean(aws_auth_validate_oauth_authz:available())).

%% A verified token whose scopes grant the requested permission -> 204. Uses a
%% direct `scope' claim (no alias) so it exercises the scope->permission match.
authz_direct_scope_grants_access_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        maybe_skip_authz(fun() ->
            #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
            Token = Sign(#{
                <<"exp">> => future(),
                <<"aud">> => <<"rabbitmq">>,
                <<"scope">> => <<"rabbitmq.configure:*/* rabbitmq.write:*/* rabbitmq.read:*/*">>
            }),
            JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
            mock_httpc_response(200, JwksBody),
            Body = (jwks_body())#{
                <<"access_token">> => Token,
                <<"resource_server_id">> => <<"rabbitmq">>,
                <<"scope_prefix">> => <<"rabbitmq.">>,
                <<"authz_check">> => #{
                    <<"vhost">> => <<"/">>,
                    <<"resource">> => <<"my-queue">>,
                    <<"permission">> => <<"configure">>
                }
            },
            [?_assertEqual(ok, aws_auth_validate_oauth:validate(Body))]
        end)
    end}.

%% scope_pattern_syntax => regex is accepted and runs the broker's bounded regex
%% matcher (A2). A regex scope granting configure on any vhost/resource -> 204.
%% Proves the regex path is wired through and functional (its ReDoS bounds are
%% the broker's own -- see the aws_auth_validate_oauth_authz header).
authz_regex_syntax_grants_access_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        maybe_skip_authz(fun() ->
            #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
            Token = Sign(#{
                <<"exp">> => future(),
                <<"aud">> => <<"rabbitmq">>,
                <<"scope">> => <<"rabbitmq.configure:.*/.*">>
            }),
            JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
            mock_httpc_response(200, JwksBody),
            Body = (jwks_body())#{
                <<"access_token">> => Token,
                <<"resource_server_id">> => <<"rabbitmq">>,
                <<"scope_prefix">> => <<"rabbitmq.">>,
                <<"scope_pattern_syntax">> => <<"regex">>,
                <<"authz_check">> => #{
                    <<"vhost">> => <<"/">>,
                    <<"resource">> => <<"my-queue">>,
                    <<"permission">> => <<"configure">>
                }
            },
            [?_assertEqual(ok, aws_auth_validate_oauth:validate(Body))]
        end)
    end}.

%% A verified token that HAS an effective scope but it does NOT grant the
%% requested permission -> authz_unverified with the "none grant" message (the
%% genuine mapping mismatch). Token grants read; we ask for configure. The
%% category stays authz_unverified; the message distinguishes this from the
%% no-effective-scopes case below.
authz_present_scope_no_match_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        maybe_skip_authz(fun() ->
            #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
            Token = Sign(#{
                <<"exp">> => future(),
                <<"aud">> => <<"rabbitmq">>,
                <<"scope">> => <<"rabbitmq.read:*/*">>
            }),
            JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
            mock_httpc_response(200, JwksBody),
            Body = (jwks_body())#{
                <<"access_token">> => Token,
                <<"resource_server_id">> => <<"rabbitmq">>,
                <<"scope_prefix">> => <<"rabbitmq.">>,
                <<"authz_check">> => #{
                    <<"resource">> => <<"my-queue">>,
                    <<"permission">> => <<"configure">>
                }
            },
            Result = aws_auth_validate_oauth:validate(Body),
            [
                ?_assertMatch({error, authz_unverified, _}, Result),
                ?_assert(reason_contains(Result, "none grant"))
            ]
        end)
    end}.

%% A verified token whose scopes ALL have the wrong prefix -> nothing survives
%% scope_prefix filtering -> authz_unverified with the no-effective-scopes
%% message (the prefix/alias-typo footgun that generates "my token is broken"
%% tickets). Token carries `other.read:*/*'; prefix is `rabbitmq.'.
authz_no_effective_scopes_after_prefix_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        maybe_skip_authz(fun() ->
            #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
            Token = Sign(#{
                <<"exp">> => future(),
                <<"aud">> => <<"rabbitmq">>,
                <<"scope">> => <<"other.configure:*/*">>
            }),
            JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
            mock_httpc_response(200, JwksBody),
            Body = (jwks_body())#{
                <<"access_token">> => Token,
                <<"resource_server_id">> => <<"rabbitmq">>,
                <<"scope_prefix">> => <<"rabbitmq.">>,
                <<"authz_check">> => #{
                    <<"resource">> => <<"my-queue">>,
                    <<"permission">> => <<"configure">>
                }
            },
            Result = aws_auth_validate_oauth:validate(Body),
            [
                ?_assertMatch({error, authz_unverified, _}, Result),
                ?_assert(reason_contains(Result, "no scopes for this resource_server"))
            ]
        end)
    end}.

%% IAM-style path: the token carries the role ARN in `sub', additional_scopes_key
%% points at `sub', and scope_aliases maps that ARN to concrete RabbitMQ scopes.
%% Proves the alias expansion (the actual MQ IAM mechanism) reaches an allow.
authz_iam_scope_alias_grants_access_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        maybe_skip_authz(fun() ->
            RoleArn = <<"arn:aws:iam::123456789012:role/RabbitMqAdminRole">>,
            #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
            Token = Sign(#{
                <<"exp">> => future(), <<"aud">> => <<"rabbitmq">>, <<"sub">> => RoleArn
            }),
            JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
            mock_httpc_response(200, JwksBody),
            Body = (jwks_body())#{
                <<"access_token">> => Token,
                <<"resource_server_id">> => <<"rabbitmq">>,
                <<"scope_prefix">> => <<"rabbitmq.">>,
                <<"additional_scopes_key">> => <<"sub">>,
                <<"scope_aliases">> => #{
                    RoleArn => [
                        <<"rabbitmq.tag:administrator">>,
                        <<"rabbitmq.read:*/*">>,
                        <<"rabbitmq.write:*/*">>,
                        <<"rabbitmq.configure:*/*">>
                    ]
                },
                <<"authz_check">> => #{
                    <<"resource">> => <<"my-queue">>,
                    <<"permission">> => <<"write">>
                }
            },
            [?_assertEqual(ok, aws_auth_validate_oauth:validate(Body))]
        end)
    end}.

%% -------- PARITY PIN: rabbitmq/rabbitmq-server discussion #16947 --------
%%
%% rabbit_auth_backend_oauth2 resolves additional_scopes_key via split_path/1:
%%
%%   split_path(Path) ->
%%       binary:split(Path, <<".">>, [global, trim_all]).
%%
%% Flat claim keys containing dots -- typical for OIDC/STS-style URIs like
%% <<"https://sts.amazonaws.com/tags">> -- are split into nested-map path
%% segments (["https://sts", "amazonaws", "com/tags"]) and looked up as a
%% deeply nested key in the flat claims map. The key is never found, so the
%% scopes under it are never extracted.
%%
%% This test PINS the current (broken) behavior: the authz evaluator returns
%% authz_unverified because it sees no effective scopes. When upstream fixes
%% split_path (or adds a dedicated flat-key accessor for additional_scopes_key),
%% this test MUST be revisited: the assertion should flip from authz_unverified
%% to ok, and the change landed together with the broker dependency bump.
%%
%% This matches the project's parity stance: document upstream behavior, pin
%% against drift, and surface regressions as test failures the moment the
%% dependency changes.
%% -------------------------------------------------------------------
authz_dotted_additional_scopes_key_parity_pin_test_() ->
    {setup, fun setup_httpc_mock/0, fun teardown_httpc_mock/1, fun(_) ->
        maybe_skip_authz(fun() ->
            #{jwk_pub := PubJwk, sign := Sign} = rsa_signer(<<"k1">>),
            %% Token carries scopes under a dotted claim key (OIDC/STS-style URI).
            %% split_path/1 will incorrectly split this on dots, breaking resolution.
            Token = Sign(#{
                <<"exp">> => future(),
                <<"aud">> => <<"rabbitmq">>,
                <<"https://sts.amazonaws.com/tags">> => [
                    <<"rabbitmq.write:*/*">>, <<"rabbitmq.read:*/*">>
                ]
            }),
            JwksBody = rabbit_json:encode(#{<<"keys">> => [PubJwk]}),
            mock_httpc_response(200, JwksBody),
            Body = (jwks_body())#{
                <<"access_token">> => Token,
                <<"resource_server_id">> => <<"rabbitmq">>,
                <<"scope_prefix">> => <<"rabbitmq.">>,
                %% Point additional_scopes_key at the dotted claim.
                <<"additional_scopes_key">> => <<"https://sts.amazonaws.com/tags">>,
                <<"authz_check">> => #{
                    <<"resource">> => <<"my-queue">>,
                    <<"permission">> => <<"write">>
                }
            },
            Result = aws_auth_validate_oauth:validate(Body),
            [
                %% Current behavior: split_path splits the dotted key, lookup fails,
                %% no scopes extracted -> authz_unverified. This will flip to ok once
                %% upstream fixes the lookup.
                ?_assertMatch({error, authz_unverified, _}, Result),
                ?_assert(reason_contains(Result, "no scopes for this resource_server"))
            ]
        end)
    end}.

%% authz_check without an access_token is rejected in the pure phase.
authz_check_without_token_rejected_test() ->
    Body = (jwks_body())#{
        <<"authz_check">> => #{
            <<"resource">> => <<"q">>, <<"permission">> => <<"read">>
        }
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% authz_check without a resource_server_id is rejected in the pure phase.
%% The evaluator builds the broker's #resource_server{} from resource_server_id
%% (it seeds the record id and default scope_prefix); without it,
%% new_resource_server(undefined) would crash on iolist_to_binary([undefined,
%% <<".">>]) and be misreported by the network catch as connection_failed. The
%% pure-phase guard turns that into a clean input_invalid before any JWKS fetch.
authz_check_without_resource_server_id_rejected_test() ->
    #{sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future()}),
    Body = (jwks_body())#{
        <<"access_token">> => Token,
        <<"authz_check">> => #{
            <<"resource">> => <<"q">>, <<"permission">> => <<"read">>
        }
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A malformed authz_check (bad permission verb) is rejected in the pure phase.
authz_check_bad_permission_rejected_test() ->
    #{sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future()}),
    Body = (jwks_body())#{
        <<"access_token">> => Token,
        <<"authz_check">> => #{
            <<"resource">> => <<"q">>, <<"permission">> => <<"admin">>
        }
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A non-binary authz_check.vhost is rejected in the pure phase. Without this
%% guard the value would reach the broker's scope matcher as
%% #resource{virtual_host = VHost} and crash it, a crash the network catch would
%% misreport as a connection failure. resource_server_id is supplied so the
%% check reaches the vhost validation rather than the earlier
%% resource_server_id guard.
authz_check_non_binary_vhost_rejected_test() ->
    #{sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future()}),
    Body = (jwks_body())#{
        <<"access_token">> => Token,
        <<"resource_server_id">> => <<"rabbitmq">>,
        <<"authz_check">> => #{
            <<"vhost">> => 1.5,
            <<"resource">> => <<"q">>,
            <<"permission">> => <<"read">>
        }
    },
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% scope_aliases must be a map of alias-name => list-of-scope-strings (the shape
%% the broker's #resource_server{} record holds). These pure-phase tests assert
%% the malformed shapes that would otherwise reach the broker matcher and crash
%% rabbit_data_coercion:to_binary/1 (float / map values), or describe a config
%% the broker could never hold, are rejected as input_invalid before any network
%% or crypto. A valid authz_check + resource_server_id + access_token are
%% supplied so the request reaches parse_authz_scope_config rather than an
%% earlier guard.
authz_scope_aliases_helper_body(Aliases) ->
    #{sign := Sign} = rsa_signer(<<"k1">>),
    Token = Sign(#{<<"exp">> => future()}),
    (jwks_body())#{
        <<"access_token">> => Token,
        <<"resource_server_id">> => <<"rabbitmq">>,
        <<"scope_aliases">> => Aliases,
        <<"authz_check">> => #{
            <<"resource">> => <<"q">>,
            <<"permission">> => <<"read">>
        }
    }.

%% A float alias value would crash to_binary/1 on a matching scope; reject it.
authz_scope_aliases_float_value_rejected_test() ->
    Body = authz_scope_aliases_helper_body(#{<<"admin">> => 1.5}),
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A nested-object alias value would crash to_binary/1; reject it.
authz_scope_aliases_object_value_rejected_test() ->
    Body = authz_scope_aliases_helper_body(#{<<"admin">> => #{<<"x">> => 1}}),
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A bare-string alias value is not the record shape (broker holds a list);
%% require the caller to supply the already-split list, so reject a string.
authz_scope_aliases_bare_string_value_rejected_test() ->
    Body = authz_scope_aliases_helper_body(#{<<"admin">> => <<"rabbitmq.read:*/*">>}),
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% A list containing a non-binary (mixed list) crashes to_binary/1 element-wise;
%% reject the whole value.
authz_scope_aliases_mixed_list_rejected_test() ->
    Body = authz_scope_aliases_helper_body(#{<<"admin">> => [<<"ok">>, 1.5]}),
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:validate(Body)).

%% Note: the valid scope_aliases shape (alias => list of scope strings) is
%% already exercised end-to-end through the JWKS mock by
%% authz_iam_scope_alias_grants_access_test_/0, which asserts a grant, so no
%% separate positive shape test is added here (a bare validate/1 without the
%% network mock would attempt a real JWKS fetch).

%% Run Fun only if the oauth2 backend's arity-4 scope API is available.
%%
%% Distinct states, deliberately handled differently (Luke's blocking
%% coverage-gap note): a bare `available() -> false' skip would let a
%% portability regression pass CI green.
%%   * available()                                 -> run the tests.
%%   * NOT available, evaluator NOT compiled in    -> legitimate skip ([]). This
%%     is a pre-floor broker series (oauth2.hrl lacks the arity-4 scope API), so
%%     the feature is genuinely unavailable by design -- same as the LDAP suite
%%     skipping without slapd.
%%   * NOT available, evaluator compiled in, but
%%     backend loaded                              -> HARD FAILURE. The evaluator
%%     was built (the Makefile saw the arity-4 API) and the backend is loaded,
%%     yet available/0 is false at runtime: a genuine portability regression. It
%%     must turn CI red, not silently skip.
%%   * NOT available, evaluator compiled in,
%%     backend NOT loaded                          -> legitimate skip ([]): the
%%     backend simply is not running on this node.
%%
%% NOTE: gate the hard failure on evaluator_compiled_in/0, not backend_loaded/0
%% alone. rabbitmq_auth_backend_oauth2 is in TEST_DEPS, so the module is loadable
%% even on a pre-floor series where the evaluator was correctly NOT compiled in;
%% keying the hard failure on backend_loaded alone would wrongly fail that
%% legitimate case.
maybe_skip_authz(Fun) ->
    case aws_auth_validate_oauth_authz:available() of
        true ->
            Fun();
        false ->
            maybe_skip_authz_unavailable(),
            []
    end.

%% available/0 is false. If the evaluator was compiled in AND the backend is
%% loaded, that is the portability regression -- fail loudly (a runtime error,
%% not a two-literal ?assertEqual, which erlc rejects with a "guard evaluates to
%% false" warning under warnings-as-errors). Otherwise the unavailability is
%% legitimate and the caller skips.
maybe_skip_authz_unavailable() ->
    case
        aws_auth_validate_oauth_authz:evaluator_compiled_in() andalso
            aws_auth_validate_oauth_authz:backend_loaded()
    of
        true ->
            erlang:error(
                {backend_loaded_but_authz_api_absent,
                    "rabbitmq_auth_backend_oauth2 is loaded and the authz evaluator "
                    "was compiled in, but available/0 is false: the arity-4 scope API "
                    "this layer requires is missing. The build guard admitted an "
                    "unsupported broker series."}
            );
        false ->
            ok
    end.

%% True when an {error, _, Reason} result's Reason binary contains Substr. Used
%% to assert the specific authz_unverified message (the category is constant;
%% the message is what distinguishes the failure stage).
reason_contains({error, _Cat, Reason}, Substr) when is_binary(Reason) ->
    string:find(Reason, Substr) =/= nomatch;
reason_contains(_Other, _Substr) ->
    false.

%%--------------------------------------------------------------------
%% hostname_verification (broker parity): the oauth backend must default to
%% STRICT matching and only add the RFC 6125 https match fun when
%% ssl_options.hostname_verification = wildcard -- mirroring oauth2_client, which
%% reads hostname_verification (default none = strict). build_client_ssl_opts/1
%% is called directly (no network); an OS trust anchor is mocked.
%%--------------------------------------------------------------------

oauth_hostname_verification_test_() ->
    {setup,
        fun() ->
            ok = meck:new(public_key, [unstick, passthrough]),
            meck:expect(public_key, cacerts_get, fun() -> [<<"der">>] end),
            ok
        end,
        fun(_) -> meck:unload(public_key) end, fun(_) ->
            Build = fun(Ssl) ->
                aws_auth_validate_oauth:build_client_ssl_opts(#{
                    ssl_options => Ssl, aws_state => none
                })
            end,
            {ok, Unset} = Build(#{<<"verify">> => <<"verify_peer">>}),
            {ok, Wild} = Build(#{
                <<"verify">> => <<"verify_peer">>,
                <<"hostname_verification">> => <<"wildcard">>
            }),
            {ok, NoneMode} = Build(#{
                <<"verify">> => <<"verify_peer">>,
                <<"hostname_verification">> => <<"none">>
            }),
            [
                ?_assertNot(lists:keymember(customize_hostname_check, 1, Unset)),
                ?_assertNot(lists:keymember(customize_hostname_check, 1, NoneMode)),
                ?_assert(lists:keymember(customize_hostname_check, 1, Wild))
            ]
        end}.

%% An unknown hostname_verification value is rejected in the pure phase.
oauth_hostname_verification_bad_value_test() ->
    Body = (jwks_body())#{<<"ssl_options">> => #{<<"hostname_verification">> => <<"bogus">>}},
    ?assertMatch({error, input_invalid, _}, aws_auth_validate_oauth:parse_input(Body)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% A minimal body with a direct jwks_uri (bypasses OIDC discovery).
jwks_body() ->
    #{<<"jwks_uri">> => <<"https://idp.example.com/.well-known/jwks.json">>}.

%% Seconds since epoch, comfortably in the future / past for exp/nbf tests.
future() -> os:system_time(seconds) + 3600.
past() -> os:system_time(seconds) - 3600.

%% Build a compact JWS string from an already-chosen header, claims map, and a
%% raw (already-bytes) signature. Used for the pure-phase shape tests that must
%% NOT require a real signature (alg:none, HS256, malformed).
compact_token(Header, Claims, SigBytes) ->
    H = b64url(rabbit_json:encode(Header)),
    P = b64url(rabbit_json:encode(Claims)),
    S = b64url(SigBytes),
    <<H/binary, ".", P/binary, ".", S/binary>>.

b64url(Bin) -> base64:encode(Bin, #{mode => urlsafe, padding => false}).

%% A minimal body with only issuer (triggers OIDC discovery).
issuer_body() ->
    #{<<"issuer">> => <<"https://idp.example.com">>}.

%% Setup meck for httpc and inets (the backend starts/stops an ephemeral
%% httpc profile). Also mock inet for DNS resolution in the network phase
%% so the resolve_and_pin step does not hit real DNS.
setup_httpc_mock() ->
    %% Ensure no stale mocks from a prior group whose teardown was skipped.
    catch meck:unload(httpc),
    catch meck:unload(inets),
    catch meck:unload(inet),
    catch meck:unload(aws_arn_util),
    ok = meck:new(httpc, [unstick, non_strict]),
    ok = meck:new(inets, [unstick, non_strict]),
    ok = meck:new(inet, [unstick, passthrough]),
    %% Create the aws_arn_util mock up front (passthrough) so a test's
    %% meck:expect(aws_arn_util, ...) always lands on a mecked module rather
    %% than silently no-opping against the real one. Tests that resolve no ARN
    %% simply never trigger it. (Previously tests relied on leaked mock state
    %% from an earlier case, which was order-dependent and fragile.)
    ok = meck:new(aws_arn_util, [passthrough]),
    %% Default: inets profile start/stop always succeed.
    meck:expect(inets, start, fun(httpc, _Opts) -> {ok, self()} end),
    meck:expect(inets, stop, fun(httpc, _Profile) -> ok end),
    %% Default: httpc set_options succeeds.
    meck:expect(httpc, set_options, fun(_Opts, _Profile) -> ok end),
    %% DNS resolution returns a safe public IP for any hostname. This avoids
    %% hitting real DNS and ensures resolve_and_pin's SSRF re-check passes.
    meck:expect(inet, getaddrs, fun
        (_Host, inet) ->
            {ok, [{93, 184, 216, 34}]};
        (_Host, inet6) ->
            {error, nxdomain}
    end),
    ok.

teardown_httpc_mock(_) ->
    catch meck:unload(httpc),
    catch meck:unload(inets),
    catch meck:unload(inet),
    catch meck:unload(aws_arn_util),
    ok.

%% Mock httpc:request to return a fixed HTTP response.
mock_httpc_response(StatusCode, Body) when is_binary(Body) ->
    meck:expect(httpc, request, fun(_Method, _Req, _HttpOpts, _Opts, _Profile) ->
        {ok, {{"HTTP/1.1", StatusCode, "OK"}, [], Body}}
    end).

%% Mock ARN resolution to succeed as a no-op (the pure-phase tests do not
%% supply ssl_options with ARNs, so this only fires if the backend
%% optionally resolves something).
mock_arn_resolve_noop() ->
    %% aws_arn_util is already mecked (passthrough) by setup_httpc_mock; just
    %% override resolve_arn. Kept as a helper so the pure-phase tests read clearly.
    meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, <<"noop">>, State} end),
    ok.
