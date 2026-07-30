%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for aws_auth_validate_tls: the behaviour callbacks, input
%% validation, the assume_role guardrail, that the ARN is only resolved after
%% the input is valid, that resolved material is not echoed, and the
%% certificate-validity checks.
%%
%% This backend makes no outbound connection, so the whole validate/1 path can
%% be driven by mocking aws_arn_util:resolve_arn and aws_iam:assume_role. The
%% expired/not-yet-valid branches are covered both through classify_validity/3
%% and end to end against openssl-generated fixtures.
-module(aws_auth_validate_tls_tests).

-include_lib("eunit/include/eunit.hrl").

%% Stands in for resolved ARN material; must not appear in any result term.
%% Binary to match aws_arn_util:resolve_arn/2's return type.
-define(SECRET, <<"secret-ca-material-should-not-appear">>).

-define(CACERT_ARN, <<"arn:aws:s3:::test-ca/ca.pem">>).
-define(ROLE_ARN, "arn:aws:iam::123456789012:role/validation").

%%--------------------------------------------------------------------
%% Behaviour callbacks
%%--------------------------------------------------------------------

tls_method_name_test() ->
    ?assertEqual(<<"tls">>, aws_auth_validate_tls:method_name()).

tls_allowed_fields_test() ->
    Fields = aws_auth_validate_tls:allowed_fields(),
    ?assertEqual([<<"target">>, <<"ssl_options">>], Fields).

%% ARN keys live under ssl_options, not at the top level, so the registry's
%% field filter cannot pass a top-level cacertfile_arn.
tls_allowed_fields_excludes_arn_test() ->
    Fields = aws_auth_validate_tls:allowed_fields(),
    ?assertNot(lists:member(<<"cacertfile_arn">>, Fields)).

%%--------------------------------------------------------------------
%% Input validation: target
%%--------------------------------------------------------------------

tls_target_input_test_() ->
    Ssl = #{<<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN}},
    [
        %% target absent.
        ?_assertMatch(
            {error, input_invalid, <<"target must be", _/binary>>},
            aws_auth_validate_tls:validate(Ssl)
        ),
        %% target not a known listener.
        ?_assertMatch(
            {error, input_invalid, <<"target must be", _/binary>>},
            aws_auth_validate_tls:validate(Ssl#{<<"target">> => <<"amqp_client">>})
        ),
        %% target not a binary.
        ?_assertMatch(
            {error, input_invalid, <<"target must be", _/binary>>},
            aws_auth_validate_tls:validate(Ssl#{<<"target">> => 42})
        )
    ].

%%--------------------------------------------------------------------
%% Input validation: ssl_options shape and values
%%--------------------------------------------------------------------

%% cacertfile_arn is required; a request with a well-formed target but no
%% cacertfile_arn (empty or absent ssl_options) is rejected before any network.
tls_cacert_required_test_() ->
    [
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.cacertfile_arn is required">>},
            aws_auth_validate_tls:validate(#{<<"target">> => <<"listener">>})
        ),
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.cacertfile_arn is required">>},
            aws_auth_validate_tls:validate(#{
                <<"target">> => <<"listener">>,
                <<"ssl_options">> => #{<<"verify">> => <<"verify_peer">>}
            })
        )
    ].

tls_ssl_options_shape_test_() ->
    Base = #{<<"target">> => <<"management">>},
    [
        %% ssl_options not an object.
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options must be an object">>},
            aws_auth_validate_tls:validate(Base#{<<"ssl_options">> => <<"nope">>})
        ),
        %% unknown key.
        ?_assertMatch(
            {error, input_invalid, <<"ssl_options contains an unknown key", _/binary>>},
            aws_auth_validate_tls:validate(Base#{
                <<"ssl_options">> => #{
                    <<"cacertfile_arn">> => ?CACERT_ARN,
                    <<"sni">> => <<"example.com">>
                }
            })
        )
    ].

tls_ssl_options_value_test_() ->
    Base = #{<<"target">> => <<"listener">>},
    Mk = fun(Extra) ->
        aws_auth_validate_tls:validate(Base#{
            <<"ssl_options">> => maps:merge(#{<<"cacertfile_arn">> => ?CACERT_ARN}, Extra)
        })
    end,
    [
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.verify must be verify_peer or verify_none">>},
            Mk(#{<<"verify">> => <<"maybe">>})
        ),
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.depth must be a non-negative integer">>},
            Mk(#{<<"depth">> => -1})
        ),
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.versions must be a list of known TLS versions">>},
            Mk(#{<<"versions">> => [<<"sslv3">>]})
        ),
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.fail_if_no_peer_cert must be true or false">>},
            Mk(#{<<"fail_if_no_peer_cert">> => <<"true">>})
        ),
        ?_assertEqual(
            {error, input_invalid, <<"ssl_options.cacertfile_arn must be a non-empty string">>},
            aws_auth_validate_tls:validate(Base#{
                <<"ssl_options">> => #{<<"cacertfile_arn">> => <<>>}
            })
        )
    ].

%% A well-formed ssl_options shape gets past input validation: with no
%% assume_role configured the remaining failure is config_conflict, not an
%% input_invalid shape error.
tls_well_formed_shapes_reach_guardrail_test() ->
    application:unset_env(aws, arn_config),
    Body = #{
        <<"target">> => <<"management">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>,
            <<"fail_if_no_peer_cert">> => true,
            <<"depth">> => 2,
            <<"versions">> => [<<"tlsv1.3">>, <<"tlsv1.2">>]
        }
    },
    ?assertMatch({error, config_conflict, _}, aws_auth_validate_tls:validate(Body)).

%%--------------------------------------------------------------------
%% assume_role guardrail and resolve ordering
%%--------------------------------------------------------------------

%% A cacertfile_arn with no assume_role configured is refused with
%% config_conflict, and the ARN is not resolved.
tls_no_assume_role_refused_test_() ->
    {setup,
        fun() ->
            application:unset_env(aws, arn_config),
            ok = meck:new(aws_arn_util, [passthrough]),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end)
        end,
        fun(_) -> meck:unload(aws_arn_util) end, fun(_) ->
            R = aws_auth_validate_tls:validate(#{
                <<"target">> => <<"listener">>,
                <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN}
            }),
            [
                ?_assertMatch(
                    {error, config_conflict,
                        <<"auth validation requires an assume_role", _/binary>>},
                    R
                ),
                ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_'))
            ]
        end}.

%% A malformed request is rejected before the assume_role or ARN fetch happens.
tls_arn_not_resolved_on_bad_input_test_() ->
    {setup,
        fun() ->
            application:set_env(aws, arn_config, [{assume_role_arn, ?ROLE_ARN}]),
            ok = meck:new(aws_iam, [no_link]),
            ok = meck:new(aws_arn_util, [passthrough]),
            meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, ?SECRET, State} end)
        end,
        fun(_) ->
            application:unset_env(aws, arn_config),
            catch meck:unload(aws_iam),
            meck:unload(aws_arn_util)
        end,
        fun(_) ->
            R = aws_auth_validate_tls:validate(#{
                <<"target">> => <<"bogus">>,
                <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN}
            }),
            [
                ?_assertMatch({error, input_invalid, _}, R),
                ?_assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_')),
                ?_assertEqual(0, meck:num_calls(aws_iam, assume_role, '_'))
            ]
        end}.

%%--------------------------------------------------------------------
%% Material validation via validate/1 (resolve_arn mocked)
%%--------------------------------------------------------------------

%% A well-formed PEM that holds no certificate entries (a private key only)
%% decodes cleanly to zero certificates (the `skip' branch) and maps to
%% input_invalid.
tls_no_certs_in_bundle_test_() ->
    with_resolved_pem(
        <<"-----BEGIN PRIVATE KEY-----\naGVsbG8=\n-----END PRIVATE KEY-----\n">>, fun() ->
            R = validate_ok_body(),
            [
                ?_assertMatch(
                    {error, input_invalid,
                        <<"cacertfile ARN did not resolve to any CA certificates">>},
                    R
                )
            ]
        end
    ).

%% A cert-framed PEM whose body is not valid base64 makes public_key:pem_decode/1
%% raise; the backend must catch it and map to input_invalid rather than crash.
tls_malformed_pem_maps_to_input_invalid_test_() ->
    with_resolved_pem(
        <<"-----BEGIN CERTIFICATE-----\nnot base64\n-----END CERTIFICATE-----">>, fun() ->
            R = validate_ok_body(),
            [
                ?_assertMatch(
                    {error, input_invalid,
                        <<"cacertfile ARN did not resolve to any CA certificates">>},
                    R
                )
            ]
        end
    ).

%% An ARN resolution failure maps to input_invalid and NAMES the field that
%% failed, so the operator knows which ARN to fix. This backend takes only
%% cacertfile_arn, so there is exactly one field to name.
tls_arn_resolve_failure_test_() ->
    {setup, fun setup_role/0, fun cleanup_role/1, fun(_) ->
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {error, not_found, State} end),
        R = validate_ok_body(),
        [
            ?_assertEqual(
                {error, input_invalid, <<"ARN resolution failed for: ssl_options.cacertfile_arn">>},
                R
            ),
            %% The resolved content and the underlying AWS error must never be
            %% echoed, however specific the field attribution gets.
            ?_assertNot(reason_contains(R, ?SECRET)),
            ?_assertNot(reason_contains(R, <<"not_found">>))
        ]
    end}.

%% True when the reason binary of an error result contains Needle.
reason_contains({error, _Category, Reason}, Needle) when is_binary(Reason) ->
    binary:match(Reason, Needle) =/= nomatch;
reason_contains(_Other, _Needle) ->
    false.

%% A valid, in-window CA bundle passes. Generated fresh so it is current.
tls_valid_ca_returns_ok_test_() ->
    CaPem = gen_ca_pem(),
    with_resolved_pem(CaPem, fun() ->
        [?_assertEqual(ok, validate_ok_body())]
    end).

%% The resolved material must not appear in the result term. ?SECRET is not a
%% valid PEM, so this exercises the no-certs path.
tls_secret_never_leaks_test_() ->
    with_resolved_pem(?SECRET, fun() ->
        R = validate_ok_body(),
        [?_assertEqual(nomatch, string_find(R, ?SECRET))]
    end).

%%--------------------------------------------------------------------
%% Certificate-validity classification
%%--------------------------------------------------------------------

%% classify_validity/3 covers all three branches without depending on the clock.
tls_classify_validity_test_() ->
    [
        ?_assertEqual(valid, aws_auth_validate_tls:classify_validity(100, 200, 150)),
        ?_assertEqual(valid, aws_auth_validate_tls:classify_validity(100, 200, 100)),
        ?_assertEqual(valid, aws_auth_validate_tls:classify_validity(100, 200, 200)),
        ?_assertEqual(not_yet_valid, aws_auth_validate_tls:classify_validity(100, 200, 99)),
        ?_assertEqual(expired, aws_auth_validate_tls:classify_validity(100, 200, 201))
    ].

%% check_cert_validity/1 flags a real expired certificate as tls_failed, using a
%% fixture whose validity window is entirely in the past. Skips if this openssl
%% does not support -not_before/-not_after (classify_validity/3 still covers the
%% logic).
tls_expired_cert_returns_tls_failed_test() ->
    case gen_expired_ca_pem() of
        skip ->
            ?debugMsg("skipping expired-cert fixture: openssl lacks -not_before/-not_after"),
            ok;
        CaPem ->
            Ders = aws_auth_validate_ssl:decode_pem_cacerts(CaPem),
            ?assertMatch(
                {error, tls_failed, <<"the CA bundle contains an expired certificate">>},
                aws_auth_validate_tls:check_cert_validity(Ders)
            )
    end.

%% An unparseable DER maps to input_invalid rather than crashing.
tls_unparseable_der_returns_bad_cert_test() ->
    ?assertEqual(
        {error, input_invalid, <<"a certificate in the CA bundle could not be parsed">>},
        aws_auth_validate_tls:check_cert_validity([<<0, 1, 2, 3>>])
    ).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

validate_ok_body() ->
    aws_auth_validate_tls:validate(#{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        }
    }).

%% Run Fun with a configured assume_role and resolve_arn mocked to return Pem.
with_resolved_pem(Pem, Fun) ->
    {setup,
        fun() ->
            R = setup_role(),
            meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, Pem, State} end),
            R
        end,
        fun cleanup_role/1, Fun}.

setup_role() ->
    application:set_env(aws, arn_config, [{assume_role_arn, ?ROLE_ARN}]),
    ok = meck:new(aws_iam, [no_link]),
    ok = meck:new(aws_arn_util, [passthrough]),
    meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
    ok.

cleanup_role(_) ->
    application:unset_env(aws, arn_config),
    catch meck:unload(aws_iam),
    meck:unload(aws_arn_util).

%% Scan a rendered term for a binary substring.
string_find(Term, Needle) ->
    string_find_bin(iolist_to_binary(io_lib:format("~p", [Term])), Needle).

string_find_bin(Hay, Needle) ->
    case binary:match(Hay, Needle) of
        nomatch -> nomatch;
        _ -> found
    end.

%% Generate a fresh, in-window self-signed CA PEM via openssl.
gen_ca_pem() ->
    Dir = tmp_dir(),
    Key = filename:join(Dir, "ca-key.pem"),
    Cert = filename:join(Dir, "ca-cert.pem"),
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
            "-days 2 -subj /CN=AwsAuthValidateTlsTestCA 2>/dev/null",
            [Key, Cert]
        )
    ),
    _ = os:cmd(Cmd),
    {ok, Pem} = file:read_file(Cert),
    Pem.

%% Generate a self-signed CA whose validity window is entirely in the past.
%% Returns `skip' if this openssl lacks -not_before/-not_after.
gen_expired_ca_pem() ->
    Dir = tmp_dir(),
    Key = filename:join(Dir, "exp-key.pem"),
    Cert = filename:join(Dir, "exp-cert.pem"),
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
            "-not_before 20200101000000Z -not_after 20200102000000Z "
            "-subj /CN=AwsAuthValidateTlsExpiredCA 2>&1",
            [Key, Cert]
        )
    ),
    Out = os:cmd(Cmd),
    case filelib:is_regular(Cert) andalso not has_error(Out) of
        true ->
            {ok, Pem} = file:read_file(Cert),
            Pem;
        false ->
            skip
    end.

has_error(Out) ->
    string:find(Out, "error") =/= nomatch orelse
        string:find(Out, "usage") =/= nomatch orelse
        string:find(Out, "unknown option") =/= nomatch.

tmp_dir() ->
    Base = filename:join(["/tmp", "aws_auth_validate_tls_tests"]),
    ok = filelib:ensure_dir(filename:join(Base, "x")),
    Base.
