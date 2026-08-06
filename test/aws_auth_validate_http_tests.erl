%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for aws_auth_validate_http: pure input validation (every
%% ?REASON_*), the SSRF literal-IP classifier reached through validate/1,
%% ARN-first ordering + no-secret-leakage (resolve_arn mocked), and the behaviour
%% callbacks. The live (m)TLS probe path runs in aws_auth_validate_mgmt_SUITE /
%% an HTTP integration suite. Internal helpers (classify_ip/1, in_cidr/2,
%% resolve_and_pin/1, classify_http_error/1, is_tls_error/1 etc.) are exported
%% under -ifdef(TEST) in the source module.
-module(aws_auth_validate_http_tests).

-include_lib("eunit/include/eunit.hrl").
%% NOTE: the reference file test/aws_auth_validate_tests.erl includes ONLY
%% eunit/include/eunit.hrl -- it does not include aws_lib.hrl or any other
%% header (resolve_arn is reached purely via the meck mock, never via a real
%% aws_lib:aws_state() record literal). Mirror that: include eunit only.

%% A unique sentinel standing in for a resolved ARN secret (CA/client-cert/key
%% material). If it appears in a rendered result term, the no-credential-leakage
%% invariant is violated. Mirrors the ?SECRET macro and string_find/2 helper in
%% aws_auth_validate_tests.erl.
%% Binary (not list) to match aws_arn_util:resolve_arn/2's return type.
-define(SECRET, <<"S3cr3t-Sentinel-Cert-Material-DO-NOT-LEAK">>).

%%--------------------------------------------------------------------
%% Behaviour callbacks
%%--------------------------------------------------------------------

http_method_name_test() ->
    ?assertEqual(<<"http">>, aws_auth_validate_http:method_name()).

http_allowed_fields_test() ->
    Fields = aws_auth_validate_http:allowed_fields(),
    [
        ?assert(lists:member(F, Fields))
     || F <- [
            <<"user_path">>,
            <<"vhost_path">>,
            <<"resource_path">>,
            <<"topic_path">>,
            <<"http_method">>,
            <<"ssl_options">>,
            <<"username">>,
            <<"password_arn">>
        ]
    ].

%% ARN keys live UNDER ssl_options, not at the top level. Defensive pin.
http_allowed_fields_excludes_arns_test() ->
    Fields = aws_auth_validate_http:allowed_fields(),
    ?assertNot(lists:member(<<"cacertfile_arn">>, Fields)),
    ?assertNot(lists:member(<<"certfile_arn">>, Fields)),
    ?assertNot(lists:member(<<"keyfile_arn">>, Fields)).

%%--------------------------------------------------------------------
%% Pure input validation: paths
%%--------------------------------------------------------------------

%% user_path is required. Its absence (even when another path is present) is
%% ?REASON_BAD_PATHS, and an empty/non-binary user_path is the same reason.
http_paths_input_test_() ->
    [
        %% user_path absent entirely.
        ?_assertMatch(
            {error, input_invalid, <<"at least user_path must be a non-empty URL string">>},
            aws_auth_validate_http:validate(#{})
        ),
        %% user_path absent though another path is present.
        ?_assertMatch(
            {error, input_invalid, <<"at least user_path must be a non-empty URL string">>},
            aws_auth_validate_http:validate(#{<<"vhost_path">> => <<"https://8.8.8.8/v">>})
        ),
        %% user_path present but empty binary.
        ?_assertMatch(
            {error, input_invalid, <<"at least user_path must be a non-empty URL string">>},
            aws_auth_validate_http:validate(#{<<"user_path">> => <<>>})
        ),
        %% user_path present but non-binary.
        ?_assertMatch(
            {error, input_invalid, <<"at least user_path must be a non-empty URL string">>},
            aws_auth_validate_http:validate(#{<<"user_path">> => 123})
        ),
        %% An optional path present as a non-binary -> ?REASON_BAD_PATH_VALUE.
        ?_assertMatch(
            {error, input_invalid, <<"each path must be a non-empty URL string">>},
            aws_auth_validate_http:validate(base_body(#{<<"vhost_path">> => 5}))
        ),
        %% An optional path present as an empty binary -> ?REASON_BAD_PATH_VALUE.
        ?_assertMatch(
            {error, input_invalid, <<"each path must be a non-empty URL string">>},
            aws_auth_validate_http:validate(base_body(#{<<"vhost_path">> => <<>>}))
        )
    ].

%% Off-shape URLs all collapse to ?REASON_BAD_URL.
http_bad_url_input_test_() ->
    [
        %% Not a URL at all.
        ?_assertMatch(
            {error, input_invalid, <<"a configured path is not a valid http(s) URL">>},
            aws_auth_validate_http:validate(body_with_user_path(<<"not a url">>))
        ),
        %% Missing host.
        ?_assertMatch(
            {error, input_invalid, <<"a configured path is not a valid http(s) URL">>},
            aws_auth_validate_http:validate(body_with_user_path(<<"https:///path">>))
        ),
        %% Embedded userinfo (user:pass@host).
        ?_assertMatch(
            {error, input_invalid, <<"a configured path is not a valid http(s) URL">>},
            aws_auth_validate_http:validate(body_with_user_path(<<"https://u:p@8.8.8.8/x">>))
        ),
        %% Pre-existing query string.
        ?_assertMatch(
            {error, input_invalid, <<"a configured path is not a valid http(s) URL">>},
            aws_auth_validate_http:validate(body_with_user_path(<<"https://8.8.8.8/x?a=1">>))
        ),
        %% Out-of-range port.
        ?_assertMatch(
            {error, input_invalid, <<"a configured path is not a valid http(s) URL">>},
            aws_auth_validate_http:validate(body_with_user_path(<<"https://8.8.8.8:70000/x">>))
        ),
        %% URL fragment (#frag) is rejected -- a configured path must not carry a
        %% fragment component.
        ?_assertMatch(
            {error, input_invalid, <<"a configured path is not a valid http(s) URL">>},
            aws_auth_validate_http:validate(
                body_with_user_path(<<"https://example.com/auth#frag">>)
            )
        )
    ].

%%--------------------------------------------------------------------
%% Pure input validation: scheme
%%--------------------------------------------------------------------

%% A non-http(s) scheme is rejected. NOTE: parse_url/1 only accepts http/https,
%% so a disallowed scheme is actually caught earlier in collect_paths as
%% ?REASON_BAD_URL rather than by the url_allowed/1 scheme check
%% (?REASON_URL_NOT_ALLOWED). We therefore assert the broad input_invalid shape
%% only and do not over-specify the reason: a scheme that passes parse_url
%% cannot exist here, so the dedicated ?REASON_URL_NOT_ALLOWED scheme branch is
%% unreachable through validate/1.
http_disallowed_scheme_test_() ->
    [
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_http:validate(body_with_user_path(<<"ftp://8.8.8.8/x">>))
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_http:validate(body_with_user_path(<<"gopher://8.8.8.8/x">>))
        )
    ].

%%--------------------------------------------------------------------
%% Pure input validation: http_method
%%--------------------------------------------------------------------

http_method_input_test_() ->
    [
        %% A binary that is not get/post.
        ?_assertMatch(
            {error, input_invalid, <<"http_method must be get or post">>},
            aws_auth_validate_http:validate(base_body(#{<<"http_method">> => <<"put">>}))
        ),
        %% A non-binary http_method.
        ?_assertMatch(
            {error, input_invalid, <<"http_method must be get or post">>},
            aws_auth_validate_http:validate(base_body(#{<<"http_method">> => 1}))
        ),
        %% get/post are ACCEPTED in the pure phase: validate proceeds to the
        %% probe (which fails to connect to 8.8.8.8), so we assert only that the
        %% outcome is NOT an input_invalid http_method rejection.
        ?_assertNotMatch(
            {error, input_invalid, _},
            aws_auth_validate_http:validate(base_body(#{<<"http_method">> => <<"post">>}))
        ),
        ?_assertNotMatch(
            {error, input_invalid, _},
            aws_auth_validate_http:validate(base_body(#{<<"http_method">> => <<"get">>}))
        )
    ].

%%--------------------------------------------------------------------
%% Pure input validation: ssl_options
%%--------------------------------------------------------------------

http_ssl_options_shape_test_() ->
    [
        %% ssl_options must be an object (map).
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options must be an object">>},
            aws_auth_validate_http:validate(body_with_ssl(<<"x">>))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options must be an object">>},
            aws_auth_validate_http:validate(body_with_ssl([1, 2]))
        ),
        %% An unknown ssl_options key is rejected, not silently dropped.
        ?_assertMatch(
            {error, input_invalid, <<
                "ssl_options contains an unknown key; allowed keys are cacertfile_arn, "
                "certfile_arn, keyfile_arn, verify, depth, versions, sni, hostname_verification"
            >>},
            aws_auth_validate_http:validate(
                body_with_ssl(#{<<"verfy">> => <<"verify_peer">>})
            )
        )
    ].

http_ssl_options_value_test_() ->
    [
        %% verify must be verify_peer or verify_none.
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.verify must be verify_peer or verify_none">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"verify">> => <<"verfy_none">>}))
        ),
        %% depth must be a non-negative integer.
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.depth must be a non-negative integer">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"depth">> => -1}))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.depth must be a non-negative integer">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"depth">> => <<"3">>}))
        ),
        %% versions must be a non-empty list of known TLS versions.
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.versions must be a list of known TLS versions">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"versions">> => [<<"sslv3">>]}))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.versions must be a list of known TLS versions">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"versions">> => []}))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.versions must be a list of known TLS versions">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"versions">> => <<"tlsv1.2">>}))
        ),
        %% sni must be a non-empty string.
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.sni must be a string">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"sni">> => 1}))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.sni must be a string">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"sni">> => <<>>}))
        ),
        %% cacertfile_arn must be a non-empty string.
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.cacertfile_arn must be a non-empty string">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"cacertfile_arn">> => <<>>}))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options.cacertfile_arn must be a non-empty string">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"cacertfile_arn">> => 1}))
        )
    ].

%% Client cert + key are an inseparable pair: one present without the other is
%% ?REASON_CLIENT_CERT_INCOMPLETE. This pairing check runs BEFORE per-value
%% validation, so an empty-string certfile_arn supplied alone still yields the
%% incomplete-pair reason, not the bad-cert-arn reason.
http_ssl_client_cert_pairing_test_() ->
    [
        ?_assertMatch(
            {error, input_invalid,
                <<"ssl_options.certfile_arn and keyfile_arn must be supplied together">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"certfile_arn">> => <<"arn:cert">>}))
        ),
        ?_assertMatch(
            {error, input_invalid,
                <<"ssl_options.certfile_arn and keyfile_arn must be supplied together">>},
            aws_auth_validate_http:validate(body_with_ssl(#{<<"keyfile_arn">> => <<"arn:key">>}))
        )
    ].

%% With BOTH keys present the pairing check passes and per-value validation
%% runs. maps:to_list/1 ordering is unspecified, so when one of the pair is an
%% invalid (empty) binary we assert only the broad input_invalid shape (it will
%% be either ?REASON_BAD_SSL_CERT_ARN or ?REASON_BAD_SSL_KEY_ARN depending on
%% iteration order). We keep the OTHER key a valid non-empty binary so the
%% pairing check itself is satisfied.
http_ssl_client_cert_bad_value_test_() ->
    [
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_http:validate(
                body_with_ssl(#{<<"certfile_arn">> => <<>>, <<"keyfile_arn">> => <<"arn:key">>})
            )
        ),
        ?_assertMatch(
            {error, input_invalid, _},
            aws_auth_validate_http:validate(
                body_with_ssl(#{<<"certfile_arn">> => <<"arn:cert">>, <<"keyfile_arn">> => <<>>})
            )
        )
    ].

%%--------------------------------------------------------------------
%% SSRF literal-IP classification (reached through validate/1 -> guard_urls).
%% The host is a literal IP, so classify_ip/1 runs in the pure phase and a
%% denied infra address yields ?REASON_URL_NOT_ALLOWED before any ARN resolve
%% or outbound request.
%%--------------------------------------------------------------------

http_ssrf_literal_ip_denied_test_() ->
    Denied = [
        %% IMDS v4 (link-local 169.254.0.0/16).
        <<"http://169.254.169.254/latest/meta-data/">>,
        %% loopback v4.
        <<"http://127.0.0.1/x">>,
        %% unspecified v4 (0.0.0.0/8).
        <<"http://0.0.0.0/x">>,
        %% loopback v6.
        <<"https://[::1]/x">>,
        %% link-local v6 (fe80::/10 spans fe80..febf).
        <<"https://[fe80::1]/x">>,
        <<"https://[febf::1]/x">>,
        %% IPv6 IMDS fd00:ec2::254.
        <<"https://[fd00:ec2::254]/x">>,
        %% IPv4-mapped v6 embedding IMDS (::ffff:a.b.c.d unwrap clause).
        <<"https://[::ffff:169.254.169.254]/x">>
    ],
    [
        ?_assertMatch(
            {error, input_invalid, <<"a configured path targets a disallowed address">>},
            aws_auth_validate_http:validate(body_with_user_path(Url))
        )
     || Url <- Denied
    ].

%% 6to4 2002::/16 unwrap: 2002:a9fe:a9fe::/48 embeds 169.254.169.254
%% (a9fe = 169.254, a9fe = 169.254) in segments 2-3, which classify_ip/1
%% unwraps to the denied v4 IMDS address. Defensive case -- the embedding
%% arithmetic is verified against v4_from_words/2 (W1=0xa9fe -> 169.254,
%% W2=0xa9fe -> 169.254).
http_ssrf_6to4_embedded_imds_test() ->
    ?assertMatch(
        {error, input_invalid, <<"a configured path targets a disallowed address">>},
        aws_auth_validate_http:validate(body_with_user_path(<<"https://[2002:a9fe:a9fe::]/x">>))
    ).

%% RFC1918 / unique-local are deliberately NOT denied (a customer auth server
%% normally lives in their VPC -- see src ?DENIED_* comment). These pass the
%% SSRF guard and proceed to the probe (which fails to connect), so we assert
%% only that they do NOT produce the disallowed-address rejection.
http_ssrf_rfc1918_allowed_test_() ->
    Allowed = [
        <<"https://10.0.0.5/x">>,
        <<"https://192.168.1.1/x">>,
        <<"https://172.16.0.1/x">>
    ],
    [
        ?_assertNotMatch(
            {error, input_invalid, <<"a configured path targets a disallowed address">>},
            aws_auth_validate_http:validate(body_with_user_path(Url))
        )
     || Url <- Allowed
    ].

%%--------------------------------------------------------------------
%% ARN-first ordering: resolve_arn is only reached AFTER the full pure
%% validation pipeline (URL shape, SSRF guard, method, ssl_options) passes.
%%--------------------------------------------------------------------

%% A body that fails the pure phase (bad http_method) but carries a
%% cacertfile_arn must NOT resolve that ARN -- proving ARN resolution is gated
%% behind pure validation. httpc is mocked so even if ordering were wrong no
%% real network call escapes; the load-bearing assertion is the num_calls=0.
http_arn_not_resolved_on_bad_method_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            ok = meck:new(httpc, [unstick, passthrough]),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                {error, {failed_connect, []}}
            end),
            ok
        end,
        fun(_) ->
            meck:unload(httpc),
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            Body = base_body(#{
                <<"http_method">> => <<"put">>,
                <<"ssl_options">> => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"verify">> => <<"verify_peer">>
                }
            }),
            Result = aws_auth_validate_http:validate(Body),
            [
                ?_assertMatch(
                    {error, input_invalid, <<"http_method must be get or post">>}, Result
                ),
                ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%% A body that fails the SSRF guard (denied literal IP) but carries a
%% cacertfile_arn must NOT resolve it -- proves the URL/SSRF guard precedes ARN
%% resolution.
http_arn_not_resolved_on_ssrf_denied_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            ok = meck:new(httpc, [unstick, passthrough]),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                {error, {failed_connect, []}}
            end),
            ok
        end,
        fun(_) ->
            meck:unload(httpc),
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            Body = #{
                <<"user_path">> => <<"http://169.254.169.254/x">>,
                <<"ssl_options">> => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"verify">> => <<"verify_peer">>
                }
            },
            Result = aws_auth_validate_http:validate(Body),
            [
                ?_assertMatch(
                    {error, input_invalid, <<"a configured path targets a disallowed address">>},
                    Result
                ),
                ?_assertEqual(false, meck:called(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%% Positive ordering: a body that PASSES pure validation with a cacertfile_arn
%% DOES reach resolve_arn (at least once). httpc is mocked to short-circuit the
%% probe so no real network occurs and no real ARN resolve is needed.
http_arn_resolved_after_pure_pass_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            ok = meck:new(aws_iam, [no_link]),
            ok = meck:new(httpc, [unstick, passthrough]),
            ok = meck:new(inets, [unstick, passthrough]),
            %% Resolving a cacertfile_arn now requires a configured assume_role
            %% (the instance-role fallback is refused). Configure one and stub the
            %% STS call so resolution proceeds under the assumed state.
            application:set_env(aws, arn_config, [
                {assume_role_arn, "arn:aws:iam::123456789012:role/r"}
            ]),
            meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            %% Let the ephemeral probe profile start/stop so execution reaches
            %% build_client_ssl_opts (which resolves the ARN) -- otherwise an
            %% inets:start failure in the bare eunit node short-circuits to
            %% connection_failed before any resolve.
            meck:expect(inets, start, fun(httpc, _) -> {ok, self()} end),
            meck:expect(inets, stop, fun(httpc, _) -> ok end),
            meck:expect(httpc, set_options, fun(_, _) -> ok end),
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                {error, {failed_connect, []}}
            end),
            ok
        end,
        fun(_) ->
            application:unset_env(aws, arn_config),
            meck:unload(inets),
            meck:unload(httpc),
            catch meck:unload(aws_iam),
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            Body = base_body(#{
                <<"ssl_options">> => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"verify">> => <<"verify_peer">>
                }
            }),
            _Result = aws_auth_validate_http:validate(Body),
            [
                ?_assert(meck:num_calls(aws_arn_util, resolve_arn, '_') >= 1)
            ]
        end}.

%%--------------------------------------------------------------------
%% Option B: the broker's EC2 instance role must be unreachable by
%% construction. A request that references no ARN carries aws_state => none;
%% resolve_arn/2 refuses that sentinel, so an ARN can never be resolved under
%% aws_lib's default credential chain (which would fall back to env/file/IMDS
%% and thus the instance role). Credential-free reachability is preserved.
%%--------------------------------------------------------------------

%% A no-ARN reachability probe needs no credentials and no configured
%% assume_role: it must succeed WITHOUT ever calling aws_arn_util:resolve_arn.
%% arn_config is deliberately unset -- proving the credential-free path does not
%% require (and never touches) the assume_role machinery.
http_no_arn_request_resolves_nothing_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            ok = meck:new(httpc, [unstick, passthrough]),
            ok = meck:new(inets, [unstick, passthrough]),
            application:unset_env(aws, arn_config),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            meck:expect(inets, start, fun(httpc, _) -> {ok, self()} end),
            meck:expect(inets, stop, fun(httpc, _) -> ok end),
            meck:expect(httpc, set_options, fun(_, _) -> ok end),
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                {ok, {{"HTTP/1.1", 200, "OK"}, [], <<"allow">>}}
            end),
            ok
        end,
        fun(_) ->
            meck:unload(inets),
            meck:unload(httpc),
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            %% No ssl_options ARNs -> request_references_arn = false ->
            %% aws_state => none. verify_none avoids needing a trust anchor.
            Body = base_body(#{
                <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
            }),
            Result = aws_auth_validate_http:validate(Body),
            [
                ?_assertEqual(ok, Result),
                ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%% Direct fail-closed check: build_client_ssl_opts/1 with aws_state => none and a
%% cacertfile_arn must refuse via resolve_arn's `none' clause -- mapping to the
%% fixed input_invalid/ARN-resolve category -- and must NOT call the real ARN
%% resolver (i.e. no env/file/IMDS credential lookup ever happens). This proves
%% the instance role is unreachable even if a future code path reached ARN
%% resolution on the no-ARN branch.
http_resolve_arn_fails_closed_on_none_sentinel_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            %% If this is ever called the sentinel would leak into a real
            %% (potentially IMDS-backed) resolve; assert 0 calls below.
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            ok
        end,
        fun(_) ->
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            Params = #{
                ssl_options => #{<<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>},
                aws_state => none
            },
            Result = aws_auth_validate_http:build_client_ssl_opts(Params),
            [
                ?_assertEqual(
                    {error, input_invalid,
                        <<"ARN resolution failed for: ssl_options.cacertfile_arn">>},
                    Result
                ),
                ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%% All THREE ARN fields failing are named in one response. The point of the
%% attribution is that an operator with several broken ARNs learns about all of
%% them in a single call instead of one per round trip, so the resolver must
%% attempt every ARN rather than stopping at the first failure.
http_arn_resolve_failure_names_all_failed_fields_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {error, denied, State} end),
            ok
        end,
        fun(_) -> meck:unload(aws_arn_util) end, fun(_) ->
            Params = #{
                ssl_options => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"certfile_arn">> => <<"arn:aws:s3:::cert">>,
                    <<"keyfile_arn">> => <<"arn:aws:s3:::key">>
                },
                aws_state => fake_state
            },
            Result = aws_auth_validate_http:build_client_ssl_opts(Params),
            [
                ?_assertEqual(
                    {error, input_invalid, <<
                        "ARN resolution failed for: ssl_options.cacertfile_arn, "
                        "ssl_options.certfile_arn, ssl_options.keyfile_arn"
                    >>},
                    Result
                ),
                %% Every ARN must actually be attempted -- that is what makes the
                %% full list possible. Short-circuiting would name only the first.
                ?_assertEqual(3, meck:num_calls(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%% Only the fields that actually failed are named: a request whose CA bundle
%% resolves but whose mTLS key does not must not blame the CA bundle.
http_arn_resolve_failure_names_only_failed_fields_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            meck:expect(aws_arn_util, resolve_arn, fun(Arn, State) ->
                case lists:suffix("key", Arn) of
                    true -> {error, denied, State};
                    false -> {ok, ?SECRET, State}
                end
            end),
            ok
        end,
        fun(_) -> meck:unload(aws_arn_util) end, fun(_) ->
            Params = #{
                ssl_options => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"certfile_arn">> => <<"arn:aws:s3:::cert">>,
                    <<"keyfile_arn">> => <<"arn:aws:s3:::key">>
                },
                aws_state => fake_state
            },
            Result = aws_auth_validate_http:build_client_ssl_opts(Params),
            [
                ?_assertEqual(
                    {error, input_invalid,
                        <<"ARN resolution failed for: ssl_options.keyfile_arn">>},
                    Result
                ),
                %% The resolved material must never appear in the reason.
                ?_assertNot(
                    case Result of
                        {error, _, R} -> binary:match(R, ?SECRET) =/= nomatch;
                        _ -> false
                    end
                )
            ]
        end}.

%% An ARN-referencing request with NO configured assume_role must be refused with
%% config_conflict BEFORE any ARN fetch -- the guardrail that keeps the endpoint
%% off the broker's ambient instance role. resolve_arn must never be called.
http_arn_request_without_role_is_config_conflict_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            application:unset_env(aws, arn_config),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            ok
        end,
        fun(_) ->
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            Body = base_body(#{
                <<"ssl_options">> => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"verify">> => <<"verify_peer">>
                }
            }),
            Result = aws_auth_validate_http:validate(Body),
            [
                ?_assertMatch({error, config_conflict, _}, Result),
                ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%%--------------------------------------------------------------------
%% A resolved secret (CA/cert/key PEM) must never reach a result term,
%% log, or crash report -- even when the probe raises with the secret in scope.
%% Mirrors ldap_bind_raise_does_not_leak_password_test_.
%%--------------------------------------------------------------------

%% The resolved cacert sentinel must not appear in the rendered result, whether
%% the path lands on tls_failed (no-trust-anchor) or connection_failed.
%% NOTE: this is a DEFENSIVE check -- the non-PEM sentinel is discarded at
%% public_key:pem_decode (returns []) so it never enters the ssl opts in
%% practice. The stronger, load-bearing no-leak exercise (where the sentinel
%% reaches the probe and a raise carries it) is http_probe_raise_does_not_leak.
http_cacert_resolution_does_not_leak_test_() ->
    {setup,
        fun() ->
            ok = meck:new(aws_arn_util, [passthrough]),
            ok = meck:new(aws_iam, [no_link]),
            ok = meck:new(httpc, [unstick, passthrough]),
            %% cacertfile_arn resolution requires a configured assume_role.
            application:set_env(aws, arn_config, [
                {assume_role_arn, "arn:aws:iam::123456789012:role/r"}
            ]),
            meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end),
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                {error, {failed_connect, []}}
            end),
            ok
        end,
        fun(_) ->
            application:unset_env(aws, arn_config),
            meck:unload(httpc),
            catch meck:unload(aws_iam),
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            Body = base_body(#{
                <<"ssl_options">> => #{
                    <<"cacertfile_arn">> => <<"arn:aws:s3:::ca">>,
                    <<"verify">> => <<"verify_peer">>
                }
            }),
            Result = aws_auth_validate_http:validate(Body),
            Rendered = lists:flatten(io_lib:format("~p", [Result])),
            [
                ?_assertMatch({error, _, _}, Result),
                ?_assertEqual(nomatch, string_find(Rendered, ?SECRET))
            ]
        end}.

%% Worst case: the probe RAISES with a secret-bearing term (httpc:request/5
%% raises {boom, Sentinel}). do_http_validate's try/catch must collapse ANY
%% raise to {error, connection_failed, ?REASON_CONNECTION}, and the rendered
%% result must not contain the sentinel. Direct HTTP analogue of the LDAP
%% simple_bind raise test.
%%
%% We deliberately omit certfile_arn/keyfile_arn so build_client_ssl_opts
%% succeeds (no PEM-decode of the sentinel) and the probe actually runs,
%% reaching the mocked httpc:request that raises. The sentinel is injected
%% into the raise term itself -- the worst case for a crash-report leak.
http_probe_raise_does_not_leak_test_() ->
    {setup,
        fun() ->
            ok = meck:new(httpc, [unstick, passthrough]),
            ok = meck:new(inets, [unstick, passthrough]),
            ok = meck:new(inet, [unstick, passthrough]),
            %% The probe raises with the sentinel in the raised term.
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                erlang:error({boom, ?SECRET})
            end),
            %% Let the ephemeral profile start/stop succeed.
            meck:expect(inets, start, fun(httpc, _) -> {ok, self()} end),
            meck:expect(inets, stop, fun(httpc, _) -> ok end),
            meck:expect(httpc, set_options, fun(_, _) -> ok end),
            %% Pin the resolved IP so resolve_and_pin succeeds (literal IP
            %% in the URL skips DNS).
            meck:expect(inet, getaddrs, fun(_, _) -> {ok, [{8, 8, 8, 8}]} end),
            ok
        end,
        fun(_) ->
            meck:unload(inet),
            meck:unload(inets),
            meck:unload(httpc)
        end,
        fun(_) ->
            %% Body with a literal-IP URL so resolve_and_pin does not need DNS.
            %% No certfile/keyfile -- build_client_ssl_opts returns {ok, [...]}.
            Body = base_body(#{
                <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
            }),
            Result = aws_auth_validate_http:validate(Body),
            Rendered = lists:flatten(io_lib:format("~p", [Result])),
            [
                ?_assertMatch(
                    {error, connection_failed, <<"could not connect to HTTP auth server">>},
                    Result
                ),
                ?_assertEqual(nomatch, string_find(Rendered, ?SECRET))
            ]
        end}.

%% claim_probe_profile/2 must ADVANCE past a slot that inets:start reports as
%% already_present, not fail the request closed. already_present arises when a
%% concurrent validation is mid-teardown: inets:stop/2 runs terminate_child then
%% delete_child as two supervisor calls, and a permanent httpc-profile child
%% keeps its spec (pid=undefined) between them, so a start landing in that window
%% sees already_present rather than already_started. We mock inets:start to
%% return already_present on the FIRST slot and {ok, _} on the SECOND, and assert
%% validate/1 still reaches the (mocked) probe and returns its ok -- proving the
%% loop advanced. Without the already_present clause this would fall through to
%% the fail-closed catch-all and return connection_failed.
http_claim_profile_advances_on_already_present_test_() ->
    {setup,
        fun() ->
            ok = meck:new(httpc, [unstick, passthrough]),
            ok = meck:new(inets, [unstick, passthrough]),
            ok = meck:new(inet, [unstick, passthrough]),
            %% First inets:start -> already_present (slot mid-teardown); the
            %% next -> a started profile. seq/1 repeats its last element, so any
            %% further starts also succeed.
            meck:expect(
                inets,
                start,
                [httpc, '_'],
                meck:seq([{error, already_present}, {ok, self()}])
            ),
            meck:expect(inets, stop, fun(httpc, _) -> ok end),
            meck:expect(httpc, set_options, fun(_, _) -> ok end),
            %% The probe reaches a well-formed auth response so the outcome is ok
            %% (proving execution got past claim_probe_profile).
            meck:expect(httpc, request, fun(_M, _R, _H, _O, _P) ->
                {ok, {{"HTTP/1.1", 200, "OK"}, [], <<"allow">>}}
            end),
            %% Literal-IP URL still resolves through resolve_and_pin -> getaddrs
            %% is not hit, but mock defensively.
            meck:expect(inet, getaddrs, fun(_, _) -> {ok, [{8, 8, 8, 8}]} end),
            ok
        end,
        fun(_) ->
            meck:unload(inet),
            meck:unload(inets),
            meck:unload(httpc)
        end,
        fun(_) ->
            Body = base_body(#{
                <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
            }),
            Result = aws_auth_validate_http:validate(Body),
            [
                %% Advanced past already_present and completed the probe.
                ?_assertEqual(ok, Result),
                %% Exactly two inets:start calls: the already_present slot, then
                %% the one we claimed.
                ?_assertEqual(2, meck:num_calls(inets, start, [httpc, '_']))
            ]
        end}.

%% Response-contract grammar (classify_response/2). The status code is checked
%% by the probe; this checks the body shape that gates a 2xx onto `ok'. A `deny'
%% (or allow-with-tags on user_path) is a SUCCESS -- we validate the shape, not
%% the verdict. A body matching neither is auth_failed (the false-pass guard).
http_response_contract_test_() ->
    Endpoint = <<"HTTP auth server did not return a usable response">>,
    Cases = [
        %% {Key, Body, Expected}
        %% user_path (authn): deny, allow, and allow-with-tags all pass.
        {<<"user_path">>, <<"allow">>, ok},
        {<<"user_path">>, <<"deny">>, ok},
        {<<"user_path">>, <<"allow administrator management">>, ok},
        %% deny-with-reason passes, mirroring the backend's `"deny " ++ Reason'
        %% clause; a conformant server may explain the refusal.
        {<<"user_path">>, <<"deny insufficient permissions">>, ok},
        {<<"resource_path">>, <<"deny not allowed here">>, ok},
        %% Normalization: leading/trailing whitespace + case are tolerated,
        %% mirroring rabbit_auth_backend_http's lower(strip(Body)).
        {<<"user_path">>, <<"  ALLOW  ">>, ok},
        {<<"user_path">>, <<"Deny\n">>, ok},
        {<<"user_path">>, <<"allow\tmanagement">>, ok},
        {<<"user_path">>, <<"DENY too bad">>, ok},
        %% authz paths only ever return a bare allow or a deny (with or without
        %% a reason).
        {<<"vhost_path">>, <<"allow">>, ok},
        {<<"resource_path">>, <<"deny">>, ok},
        {<<"topic_path">>, <<"ALLOW">>, ok},
        %% Non-auth bodies are rejected even though the status was 2xx.
        {<<"user_path">>, <<"<html>hi</html>">>, {error, auth_failed, Endpoint}},
        {<<"user_path">>, <<"">>, {error, auth_failed, Endpoint}},
        {<<"user_path">>, <<"allowed">>, {error, auth_failed, Endpoint}},
        {<<"user_path">>, <<"denied">>, {error, auth_failed, Endpoint}},
        {<<"user_path">>, <<"{\"result\":\"allow\"}">>, {error, auth_failed, Endpoint}},
        %% allow-with-tags is NOT valid on an authz path (exact match only).
        {<<"vhost_path">>, <<"allow administrator">>, {error, auth_failed, Endpoint}},
        %% A non-binary body (defensive) is not auth-shaped.
        {<<"user_path">>, undefined, {error, auth_failed, Endpoint}}
    ],
    [
        ?_assertEqual(Expected, aws_auth_validate_http:classify_response(Key, Body))
     || {Key, Body, Expected} <- Cases
    ].

%%--------------------------------------------------------------------
%% hostname_verification (broker parity): the http backend must default to
%% STRICT matching (no RFC 6125 https match fun) and only add the wildcard fun
%% when ssl_options.hostname_verification = wildcard -- mirroring
%% rabbit_auth_backend_http, which applies it only on ssl_hostname_verification =
%% wildcard. build_client_ssl_opts/1 is called directly (no network); an OS
%% trust anchor is mocked so verify_peer keeps a trust anchor and the
%% match-fun branch is reachable.
%%--------------------------------------------------------------------

http_hostname_verification_test_() ->
    {setup,
        fun() ->
            ok = meck:new(public_key, [unstick, passthrough]),
            %% Non-empty OS store -> verify_peer has a trust anchor.
            meck:expect(public_key, cacerts_get, fun() -> [<<"der">>] end),
            ok
        end,
        fun(_) -> meck:unload(public_key) end, fun(_) ->
            Build = fun(Ssl) ->
                aws_auth_validate_http:build_client_ssl_opts(#{
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
                %% Unset defaults to strict: no https match fun (the #151 parity
                %% fix, now extended to http).
                ?_assertNot(lists:keymember(customize_hostname_check, 1, Unset)),
                %% none is also strict.
                ?_assertNot(lists:keymember(customize_hostname_check, 1, NoneMode)),
                %% wildcard opts into the RFC 6125 https match fun.
                ?_assert(lists:keymember(customize_hostname_check, 1, Wild))
            ]
        end}.

%% An unknown hostname_verification value is rejected in the pure phase.
http_hostname_verification_bad_value_test() ->
    ?assertMatch(
        {error, input_invalid, _},
        aws_auth_validate_http:validate(
            body_with_ssl(#{<<"hostname_verification">> => <<"bogus">>})
        )
    ).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% A minimally-valid body that PASSES every pure-validation step (paths,
%% http_method, ssl_options, SSRF guard) so the ARN-first / probe path is the
%% next thing reached. Uses a public literal IP host so classify_ip/1 returns
%% allow in the pure phase (mirrors base_body/0 in aws_auth_validate_tests,
%% which used 8.8.8.8 for the same reason). https so ssl_options/ARNs matter.
base_body(Overrides) when is_map(Overrides) ->
    Base = #{<<"user_path">> => <<"https://8.8.8.8/auth/user">>},
    maps:merge(Base, Overrides).

%% Convenience: a body whose user_path is an arbitrary URL string.
body_with_user_path(Url) ->
    #{<<"user_path">> => Url}.

%% Convenience: base body plus an ssl_options object.
body_with_ssl(SslOpts) ->
    base_body(#{<<"ssl_options">> => SslOpts}).

string_find(Haystack, Needle) when is_binary(Needle) ->
    string:find(Haystack, binary_to_list(Needle));
string_find(Haystack, Needle) ->
    string:find(Haystack, Needle).
