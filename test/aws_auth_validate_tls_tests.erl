%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Unit tests for aws_auth_validate_tls: the behaviour callbacks, input
%% validation, the assume_role guardrail, that the ARN is only resolved after
%% the input is valid, that resolved material is not echoed, the
%% certificate-validity checks, and the client-certificate chain/extract/resolve
%% layers.
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
    ?assertEqual([<<"target">>, <<"ssl_options">>, <<"client_cert">>, <<"cert_login">>], Fields).

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

%% An ARN resolution failure maps to input_invalid.
tls_arn_resolve_failure_test_() ->
    {setup, fun setup_role/0, fun cleanup_role/1, fun(_) ->
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {error, not_found, State} end),
        R = validate_ok_body(),
        [
            ?_assertEqual({error, input_invalid, <<"failed to resolve ARN">>}, R)
        ]
    end}.

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

%%====================================================================
%% client_cert parse tests
%%====================================================================

tls_client_cert_absent_passes_test() ->
    Body = #{
        <<"target">> => <<"listener">>, <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN}
    },
    {ok, Acc} = aws_auth_validate_tls:parse_input(Body),
    ?assertNot(maps:is_key(client_cert_ders, Acc)).

tls_client_cert_empty_binary_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"client_cert">> => <<>>
    },
    ?assertMatch(
        {error, input_invalid, <<"client_cert must be a non-empty PEM", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_client_cert_not_binary_rejected_test_() ->
    Body = fun(V) ->
        #{
            <<"target">> => <<"listener">>,
            <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
            <<"client_cert">> => V
        }
    end,
    [
        ?_assertMatch(
            {error, input_invalid, <<"client_cert must be a non-empty PEM", _/binary>>},
            aws_auth_validate_tls:parse_input(Body(42))
        ),
        ?_assertMatch(
            {error, input_invalid, <<"client_cert must be a non-empty PEM", _/binary>>},
            aws_auth_validate_tls:parse_input(Body([1, 2, 3]))
        )
    ].

tls_client_cert_private_key_rejected_test() ->
    %% A PEM containing a private key must be rejected (R6).
    KeyPem = gen_private_key_pem(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"client_cert">> => KeyPem
    },
    ?assertMatch(
        {error, input_invalid, <<"client_cert must not contain private key material", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_client_cert_mixed_key_and_cert_rejected_test() ->
    %% A PEM with both cert and key entries is rejected because of the key.
    {_CaKey, _CaCert, CaPem} = gen_ca(),
    KeyPem = gen_private_key_pem(),
    Mixed = <<CaPem/binary, KeyPem/binary>>,
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"client_cert">> => Mixed
    },
    ?assertMatch(
        {error, input_invalid, <<"client_cert must not contain private key material", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_client_cert_valid_pem_passes_test() ->
    {_CaKey, _CaCert, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"client_cert">> => CaPem
    },
    {ok, Acc} = aws_auth_validate_tls:parse_input(Body),
    ?assert(maps:is_key(client_cert_ders, Acc)),
    ?assert(length(maps:get(client_cert_ders, Acc)) >= 1).

tls_client_cert_garbage_pem_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"client_cert">> => <<"not a pem at all">>
    },
    ?assertMatch(
        {error, input_invalid, <<"client_cert must be a non-empty PEM", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

%%====================================================================
%% cert_login parse tests
%%====================================================================

tls_cert_login_not_map_rejected_test_() ->
    Mk = fun(V) ->
        #{
            <<"target">> => <<"listener">>,
            <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
            <<"cert_login">> => V
        }
    end,
    [
        ?_assertEqual(
            {error, input_invalid, <<"cert_login must be an object">>},
            aws_auth_validate_tls:parse_input(Mk(<<"string">>))
        ),
        ?_assertEqual(
            {error, input_invalid, <<"cert_login must be an object">>},
            aws_auth_validate_tls:parse_input(Mk(42))
        )
    ].

tls_cert_login_unknown_key_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{<<"from">> => <<"common_name">>, <<"extra">> => true}
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login contains an unknown key", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_missing_from_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{}
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login.from must be", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_bad_from_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{<<"from">> => <<"serial_number">>}
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login.from must be", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_subject_alt_name_alias_test() ->
    %% subject_alt_name is an accepted alias for subject_alternative_name.
    %% client_cert + verify_peer required to pass cross_field_checks.
    {_CaKeyFile, _CaDer, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"client_cert">> => CaPem,
        <<"cert_login">> => #{
            <<"from">> => <<"subject_alt_name">>,
            <<"san_type">> => <<"dns">>
        }
    },
    {ok, Acc} = aws_auth_validate_tls:parse_input(Body),
    #{cert_login := #{from := From}} = Acc,
    ?assertEqual(subject_alternative_name, From).

tls_cert_login_san_type_without_san_from_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{<<"from">> => <<"common_name">>, <<"san_type">> => <<"dns">>}
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login.san_type is only valid", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_san_index_without_san_from_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{<<"from">> => <<"distinguished_name">>, <<"san_index">> => 0}
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login.san_index is only valid", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_bad_san_type_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{
            <<"from">> => <<"subject_alternative_name">>,
            <<"san_type">> => <<"x500">>
        }
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login.san_type must be", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_negative_san_index_rejected_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"cert_login">> => #{
            <<"from">> => <<"subject_alternative_name">>,
            <<"san_type">> => <<"dns">>,
            <<"san_index">> => -1
        }
    },
    ?assertMatch(
        {error, input_invalid, <<"cert_login.san_index must be a non-negative", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_valid_san_config_test() ->
    {_CaKeyFile, _CaDer, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"client_cert">> => CaPem,
        <<"cert_login">> => #{
            <<"from">> => <<"subject_alternative_name">>,
            <<"san_type">> => <<"email">>,
            <<"san_index">> => 2
        }
    },
    {ok, Acc} = aws_auth_validate_tls:parse_input(Body),
    ?assertEqual(
        #{from => subject_alternative_name, san_type => email, san_index => 2},
        maps:get(cert_login, Acc)
    ).

tls_cert_login_san_index_defaults_to_zero_test() ->
    {_CaKeyFile, _CaDer, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"client_cert">> => CaPem,
        <<"cert_login">> => #{
            <<"from">> => <<"subject_alternative_name">>,
            <<"san_type">> => <<"dns">>
        }
    },
    {ok, Acc} = aws_auth_validate_tls:parse_input(Body),
    #{cert_login := #{san_index := Index}} = Acc,
    ?assertEqual(0, Index).

%%====================================================================
%% Cross-field conflict tests
%%====================================================================

tls_cert_login_without_client_cert_conflict_test() ->
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"cert_login">> => #{<<"from">> => <<"common_name">>}
    },
    ?assertEqual(
        {error, config_conflict, <<"cert_login requires client_cert to be present">>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_with_verify_none_conflict_test() ->
    {_CaKey, _CaCert, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_none">>
        },
        <<"client_cert">> => CaPem,
        <<"cert_login">> => #{<<"from">> => <<"common_name">>}
    },
    ?assertMatch(
        {error, config_conflict, <<"cert-based login requires ssl_options.verify", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_cert_login_with_verify_absent_conflict_test() ->
    {_CaKey, _CaCert, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{<<"cacertfile_arn">> => ?CACERT_ARN},
        <<"client_cert">> => CaPem,
        <<"cert_login">> => #{<<"from">> => <<"common_name">>}
    },
    ?assertMatch(
        {error, config_conflict, <<"cert-based login requires ssl_options.verify", _/binary>>},
        aws_auth_validate_tls:parse_input(Body)
    ).

tls_client_cert_without_cert_login_ok_test() ->
    %% client_cert alone (no cert_login) is the chain-only mTLS case -- should pass.
    {_CaKey, _CaCert, CaPem} = gen_ca(),
    Body = #{
        <<"target">> => <<"listener">>,
        <<"ssl_options">> => #{
            <<"cacertfile_arn">> => ?CACERT_ARN,
            <<"verify">> => <<"verify_peer">>
        },
        <<"client_cert">> => CaPem
    },
    {ok, Acc} = aws_auth_validate_tls:parse_input(Body),
    ?assert(maps:is_key(client_cert_ders, Acc)),
    ?assertNot(maps:is_key(cert_login, Acc)).

%%====================================================================
%% Chain validation tests (Layer 1)
%%====================================================================

tls_validate_chain_ok_test() ->
    {_CaKey, CaDer, LeafDer} = gen_ca_and_leaf(),
    ?assertEqual(ok, aws_auth_validate_tls:validate_chain([LeafDer], [CaDer], undefined)).

tls_validate_chain_wrong_ca_test() ->
    %% A leaf signed by CA-A does not chain to CA-B.
    {_CaKeyA, _CaDerA, LeafDer} = gen_ca_and_leaf(),
    {_CaKeyB, CaDerB, _LeafDerB} = gen_ca_and_leaf(),
    ?assertMatch(
        {error, auth_failed, <<"the client certificate does not chain", _/binary>>},
        aws_auth_validate_tls:validate_chain([LeafDer], [CaDerB], undefined)
    ).

tls_validate_chain_malformed_der_test() ->
    ?assertMatch(
        {error, input_invalid, <<"a certificate in the client chain could not be parsed">>},
        aws_auth_validate_tls:validate_chain([<<0, 1, 2, 3>>], [<<4, 5, 6, 7>>], undefined)
    ).

tls_validate_chain_depth_zero_single_leaf_test() ->
    %% depth=0 means no intermediates allowed; a direct CA->leaf should pass.
    {_CaKey, CaDer, LeafDer} = gen_ca_and_leaf(),
    ?assertEqual(ok, aws_auth_validate_tls:validate_chain([LeafDer], [CaDer], 0)).

tls_validate_chain_with_intermediate_test() ->
    %% Leaf signed by an intermediate CA, presented leaf-first with the
    %% intermediate ([leaf, int]); the root is the bundle anchor. This is the
    %% real client-chain shape and requires the chain to be reordered
    %% anchor-closest-first for pkix_path_validation.
    {RootDer, IntDer, LeafDer} = gen_root_int_leaf(),
    ?assertEqual(
        ok,
        aws_auth_validate_tls:validate_chain([LeafDer, IntDer], [RootDer], undefined)
    ).

tls_validate_chain_missing_intermediate_test() ->
    %% The same leaf without its intermediate cannot build a path to the root
    %% anchor, so it must be rejected.
    {RootDer, _IntDer, LeafDer} = gen_root_int_leaf(),
    ?assertMatch(
        {error, auth_failed, <<"the client certificate does not chain", _/binary>>},
        aws_auth_validate_tls:validate_chain([LeafDer], [RootDer], undefined)
    ).

tls_validate_chain_intermediate_depth_zero_rejected_test() ->
    %% depth=0 forbids any intermediate; the leaf+intermediate chain exceeds it.
    {RootDer, IntDer, LeafDer} = gen_root_int_leaf(),
    ?assertMatch(
        {error, auth_failed,
            <<"the client certificate chain exceeds the configured path length (depth)">>},
        aws_auth_validate_tls:validate_chain([LeafDer, IntDer], [RootDer], 0)
    ).

tls_validate_chain_intermediate_only_bundle_rejected_test() ->
    %% Finding 1: A CA bundle containing only an intermediate (not self-signed)
    %% must be rejected. The broker's ssl listener rejects this with unknown_ca.
    {_RootDer, IntDer, LeafDer} = gen_root_int_leaf(),
    ?assertMatch(
        {error, auth_failed, <<"the client certificate does not chain", _/binary>>},
        aws_auth_validate_tls:validate_chain([LeafDer], [IntDer], undefined)
    ).

tls_validate_chain_key_rollover_old_new_order_test() ->
    %% Finding 2: Two root CAs sharing the same subject DN (key rollover),
    %% bundle order [OldRoot, NewRoot], leaf chaining to NewRoot. Must pass.
    {OldRootDer, NewRootDer, LeafDer} = gen_two_roots_same_dn_and_leaf(),
    ?assertEqual(
        ok,
        aws_auth_validate_tls:validate_chain([LeafDer], [OldRootDer, NewRootDer], undefined)
    ).

tls_validate_chain_key_rollover_new_old_order_test() ->
    %% Finding 2: Same as above but bundle order [NewRoot, OldRoot]. Must pass
    %% regardless of bundle order.
    {OldRootDer, NewRootDer, LeafDer} = gen_two_roots_same_dn_and_leaf(),
    ?assertEqual(
        ok,
        aws_auth_validate_tls:validate_chain([LeafDer], [NewRootDer, OldRootDer], undefined)
    ).

tls_validate_chain_unordered_tail_passes_test() ->
    %% Finding 7: A leaf-first chain with an arbitrarily ordered tail must pass.
    %% The broker's ssl_certificate:paths/2 rebuilds the chain from the peer cert
    %% outward, so the tail order does not matter. Here we present [leaf, root, int]
    %% with bundle=[root]: the tail is out of issuer order, but the code relinks it
    %% as [leaf, int, root] and then strips root (the anchor) from the path.
    {RootDer, IntDer, LeafDer} = gen_root_int_leaf(),
    ?assertEqual(
        ok,
        aws_auth_validate_tls:validate_chain([LeafDer, RootDer, IntDer], [RootDer], undefined)
    ).

tls_validate_chain_root_first_rejected_test() ->
    %% Finding 7 narrowing: A fully root-first chain (leaf is NOT the first
    %% element) is rejected by the broker and must remain rejected here. This
    %% pins the shared rejection so a future refactoring does not accidentally
    %% loosen the leaf-first requirement.
    {RootDer, IntDer, LeafDer} = gen_root_int_leaf(),
    ?assertMatch(
        {error, _, _},
        aws_auth_validate_tls:validate_chain([RootDer, IntDer, LeafDer], [RootDer], undefined)
    ).

%%====================================================================
%% Username extraction tests (Layer 2)
%%====================================================================

tls_extract_dn_test() ->
    {_CaKey, _CaDer, LeafDer} = gen_ca_and_leaf_with_cn("TestUser"),
    {ok, Name} = aws_auth_validate_tls:extract_username(
        LeafDer, #{from => distinguished_name}
    ),
    %% The DN should contain CN=TestUser somewhere.
    ?assertMatch({_, _}, binary:match(Name, <<"TestUser">>)).

tls_extract_cn_test() ->
    {_CaKey, _CaDer, LeafDer} = gen_ca_and_leaf_with_cn("MyCN"),
    {ok, Name} = aws_auth_validate_tls:extract_username(
        LeafDer, #{from => common_name}
    ),
    ?assertEqual(<<"MyCN">>, Name).

tls_extract_cn_missing_test() ->
    %% A cert with only O= (no CN) should fail extraction.
    {_CaKey, _CaDer, LeafDer} = gen_ca_and_leaf_with_subject("/O=NoCN"),
    ?assertMatch(
        {error, auth_failed, <<"no username could be extracted", _/binary>>},
        aws_auth_validate_tls:extract_username(LeafDer, #{from => common_name})
    ).

tls_extract_san_dns_test() ->
    {_CaKey, _CaDer, LeafDer} = gen_ca_and_leaf_with_san("DNS:host.example.com"),
    {ok, Name} = aws_auth_validate_tls:extract_username(
        LeafDer, #{from => subject_alternative_name, san_type => dns, san_index => 0}
    ),
    ?assertEqual(<<"host.example.com">>, Name).

tls_extract_san_index_past_end_test() ->
    {_CaKey, _CaDer, LeafDer} = gen_ca_and_leaf_with_san("DNS:only.one.com"),
    ?assertMatch(
        {error, auth_failed, <<"no username could be extracted", _/binary>>},
        aws_auth_validate_tls:extract_username(
            LeafDer, #{from => subject_alternative_name, san_type => dns, san_index => 5}
        )
    ).

%%====================================================================
%% User resolution tests (Layer 3)
%%====================================================================

tls_resolve_user_found_test_() ->
    {setup,
        fun() ->
            ok = meck:new(rabbit_auth_backend_internal, [non_strict, no_link]),
            meck:expect(rabbit_auth_backend_internal, exists, fun(_) -> true end)
        end,
        fun(_) -> meck:unload(rabbit_auth_backend_internal) end, fun(_) ->
            [?_assertEqual(ok, aws_auth_validate_tls:resolve_user(<<"alice">>))]
        end}.

tls_resolve_user_not_found_test_() ->
    {setup,
        fun() ->
            ok = meck:new(rabbit_auth_backend_internal, [non_strict, no_link]),
            meck:expect(rabbit_auth_backend_internal, exists, fun(_) -> false end)
        end,
        fun(_) -> meck:unload(rabbit_auth_backend_internal) end, fun(_) ->
            R = aws_auth_validate_tls:resolve_user(<<"bob">>),
            [
                ?_assertMatch({error, auth_failed, _}, R),
                ?_assertMatch(
                    {error, auth_failed, <<"no internal user named bob exists">>}, R
                )
            ]
        end}.

tls_resolve_user_module_unavailable_test_() ->
    {setup,
        fun() ->
            %% Mock rabbit_auth_backend_internal without defining exists/1, so
            %% function_exported(rabbit_auth_backend_internal, exists, 1) returns
            %% false and internal_backend_available() fails.
            ok = meck:new(rabbit_auth_backend_internal, [non_strict, no_link]),
            ok
        end,
        fun(_) -> meck:unload(rabbit_auth_backend_internal) end, fun(_) ->
            [
                ?_assertMatch(
                    {error, config_conflict,
                        <<"user-resolution check unavailable on this broker series">>},
                    aws_auth_validate_tls:resolve_user(<<"charlie">>)
                )
            ]
        end}.

%%====================================================================
%% sanitize_username tests
%%====================================================================

tls_sanitize_username_strips_control_chars_test() ->
    ?assertEqual(<<"hello">>, aws_auth_validate_tls:sanitize_username(<<"he\x01llo">>)).

tls_sanitize_username_caps_length_test() ->
    Long = binary:copy(<<"A">>, 300),
    ?assertEqual(256, byte_size(aws_auth_validate_tls:sanitize_username(Long))).

tls_sanitize_username_preserves_normal_test() ->
    ?assertEqual(<<"alice">>, aws_auth_validate_tls:sanitize_username(<<"alice">>)).

%%====================================================================
%% Backward compatibility: requests with no new fields unchanged
%%====================================================================

tls_no_new_fields_backward_compat_test_() ->
    %% A request with only target + ssl_options should behave exactly as before.
    CaPem = gen_ca_pem(),
    with_resolved_pem(CaPem, fun() ->
        [?_assertEqual(ok, validate_ok_body())]
    end).

%%====================================================================
%% Certificate generation helpers (openssl-based, proven reliable)
%%====================================================================

%% Generate a fresh CA key + self-signed certificate via openssl. Returns
%% {CaKeyFile, CaDerBinary, CaPemBinary}.
gen_ca() ->
    Dir = tmp_dir(),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    CaKeyFile = filename:join(Dir, "ca-key-" ++ Suffix ++ ".pem"),
    CaCertFile = filename:join(Dir, "ca-cert-" ++ Suffix ++ ".pem"),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
                "-days 2 -subj /CN=TestCA~s 2>/dev/null",
                [CaKeyFile, CaCertFile, Suffix]
            )
        )
    ),
    {ok, CaPem} = file:read_file(CaCertFile),
    [{'Certificate', CaDer, not_encrypted}] = public_key:pem_decode(CaPem),
    {CaKeyFile, CaDer, CaPem}.

%% Generate a CA and a leaf cert signed by it. Returns {CaKeyFile, CaDer, LeafDer}.
gen_ca_and_leaf() ->
    gen_ca_and_leaf_with_cn("LeafCert").

%% Generate CA + leaf with a specific CN. Returns {CaKeyFile, CaDer, LeafDer}.
gen_ca_and_leaf_with_cn(CN) ->
    Dir = tmp_dir(),
    {CaKeyFile, CaDer, _CaPem} = gen_ca(),
    CaCertFile = ca_cert_file_from_key(CaKeyFile),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    LeafKeyFile = filename:join(Dir, "leaf-key-" ++ Suffix ++ ".pem"),
    LeafCsrFile = filename:join(Dir, "leaf-" ++ Suffix ++ ".csr"),
    LeafCertFile = filename:join(Dir, "leaf-cert-" ++ Suffix ++ ".pem"),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
                "-subj '/CN=~s' 2>/dev/null",
                [LeafKeyFile, LeafCsrFile, CN]
            )
        )
    ),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
                "-out ~ts -days 2 2>/dev/null",
                [LeafCsrFile, CaCertFile, CaKeyFile, LeafCertFile]
            )
        )
    ),
    {ok, LeafPem} = file:read_file(LeafCertFile),
    [{'Certificate', LeafDer, not_encrypted}] = public_key:pem_decode(LeafPem),
    {CaKeyFile, CaDer, LeafDer}.

%% Generate CA + leaf with a custom subject (may lack CN). Returns {CaKeyFile, CaDer, LeafDer}.
gen_ca_and_leaf_with_subject(Subject) ->
    Dir = tmp_dir(),
    {CaKeyFile, CaDer, _CaPem} = gen_ca(),
    CaCertFile = ca_cert_file_from_key(CaKeyFile),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    LeafKeyFile = filename:join(Dir, "leaf-key-" ++ Suffix ++ ".pem"),
    LeafCsrFile = filename:join(Dir, "leaf-" ++ Suffix ++ ".csr"),
    LeafCertFile = filename:join(Dir, "leaf-cert-" ++ Suffix ++ ".pem"),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
                "-subj '~s' 2>/dev/null",
                [LeafKeyFile, LeafCsrFile, Subject]
            )
        )
    ),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
                "-out ~ts -days 2 2>/dev/null",
                [LeafCsrFile, CaCertFile, CaKeyFile, LeafCertFile]
            )
        )
    ),
    {ok, LeafPem} = file:read_file(LeafCertFile),
    [{'Certificate', LeafDer, not_encrypted}] = public_key:pem_decode(LeafPem),
    {CaKeyFile, CaDer, LeafDer}.

%% Generate CA + leaf with SAN extension. SanSpec is an openssl-style string
%% like "DNS:host.example.com". Returns {CaKeyFile, CaDer, LeafDer}.
gen_ca_and_leaf_with_san(SanSpec) ->
    Dir = tmp_dir(),
    {CaKeyFile, CaDer, _CaPem} = gen_ca(),
    CaCertFile = ca_cert_file_from_key(CaKeyFile),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    LeafKeyFile = filename:join(Dir, "san-leaf-key-" ++ Suffix ++ ".pem"),
    LeafCsrFile = filename:join(Dir, "san-leaf-" ++ Suffix ++ ".csr"),
    LeafCertFile = filename:join(Dir, "san-leaf-cert-" ++ Suffix ++ ".pem"),
    ExtFile = filename:join(Dir, "san-ext-" ++ Suffix ++ ".cnf"),
    ok = file:write_file(
        ExtFile,
        io_lib:format("[san]\nsubjectAltName=~s\n", [SanSpec])
    ),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
                "-subj /CN=SanLeaf 2>/dev/null",
                [LeafKeyFile, LeafCsrFile]
            )
        )
    ),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
                "-out ~ts -days 2 -extfile ~ts -extensions san 2>/dev/null",
                [LeafCsrFile, CaCertFile, CaKeyFile, LeafCertFile, ExtFile]
            )
        )
    ),
    {ok, LeafPem} = file:read_file(LeafCertFile),
    [{'Certificate', LeafDer, not_encrypted}] = public_key:pem_decode(LeafPem),
    {CaKeyFile, CaDer, LeafDer}.

%% Generate a private key PEM (for the rejection test).
gen_private_key_pem() ->
    Dir = tmp_dir(),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    KeyFile = filename:join(Dir, "privkey-" ++ Suffix ++ ".pem"),
    _ = os:cmd(
        lists:flatten(
            io_lib:format(
                "openssl genrsa -out ~ts 2048 2>/dev/null", [KeyFile]
            )
        )
    ),
    {ok, Pem} = file:read_file(KeyFile),
    Pem.

%% Derive the cert filename from the key filename (matching gen_ca's naming).
ca_cert_file_from_key(CaKeyFile) ->
    re:replace(CaKeyFile, "ca-key-", "ca-cert-", [{return, list}]).

%% Generate a root CA, an intermediate CA signed by the root, and a leaf signed
%% by the intermediate. Returns {RootCaDer, IntDer, LeafDer}. Exercises the
%% multi-cert chain path (leaf + intermediate presented, root is the anchor).
gen_root_int_leaf() ->
    Dir = tmp_dir(),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    F = fun(Name) -> filename:join(Dir, Name ++ "-" ++ Suffix ++ ".pem") end,
    RootKey = F("ri-root-key"),
    RootCert = F("ri-root-cert"),
    IntKey = F("ri-int-key"),
    IntCsr = F("ri-int-csr"),
    IntCert = F("ri-int-cert"),
    IntExt = F("ri-int-ext"),
    LeafKey = F("ri-leaf-key"),
    LeafCsr = F("ri-leaf-csr"),
    LeafCert = F("ri-leaf-cert"),
    ok = file:write_file(
        IntExt,
        "basicConstraints=critical,CA:TRUE,pathlen:0\n"
        "keyUsage=critical,keyCertSign,cRLSign\n"
    ),
    Sh = fun(Fmt, Args) -> os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    _ = Sh(
        "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-days 2 -subj /CN=TestRootCA~s 2>/dev/null",
        [RootKey, RootCert, Suffix]
    ),
    _ = Sh(
        "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj /CN=TestIntCA~s 2>/dev/null",
        [IntKey, IntCsr, Suffix]
    ),
    _ = Sh(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 2 -extfile ~ts 2>/dev/null",
        [IntCsr, RootCert, RootKey, IntCert, IntExt]
    ),
    _ = Sh(
        "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj /CN=TestChainLeaf~s 2>/dev/null",
        [LeafKey, LeafCsr, Suffix]
    ),
    _ = Sh(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 2 2>/dev/null",
        [LeafCsr, IntCert, IntKey, LeafCert]
    ),
    {ok, RootPem} = file:read_file(RootCert),
    {ok, IntPem} = file:read_file(IntCert),
    {ok, LeafPem} = file:read_file(LeafCert),
    [{'Certificate', RootDer, not_encrypted}] = public_key:pem_decode(RootPem),
    [{'Certificate', IntDer, not_encrypted}] = public_key:pem_decode(IntPem),
    [{'Certificate', LeafDer, not_encrypted}] = public_key:pem_decode(LeafPem),
    {RootDer, IntDer, LeafDer}.

%% Generate two self-signed root CAs with the SAME subject DN but different keys,
%% plus a leaf signed by the second (new) root. Exercises the key-rollover
%% scenario where a bundle contains both old and new roots and the code must try
%% each candidate until one validates.
%% Returns {OldRootDer, NewRootDer, LeafDer}.
gen_two_roots_same_dn_and_leaf() ->
    Dir = tmp_dir(),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    F = fun(Name) -> filename:join(Dir, Name ++ "-" ++ Suffix ++ ".pem") end,
    OldRootKey = F("kr-old-root-key"),
    OldRootCert = F("kr-old-root-cert"),
    NewRootKey = F("kr-new-root-key"),
    NewRootCert = F("kr-new-root-cert"),
    LeafKey = F("kr-leaf-key"),
    LeafCsr = F("kr-leaf-csr"),
    LeafCert = F("kr-leaf-cert"),
    %% Both roots share the SAME CN (simulating key rollover).
    CommonCN = "TestRolloverRoot" ++ Suffix,
    Sh = fun(Fmt, Args) -> os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    _ = Sh(
        "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-days 2 -subj '/CN=~s' 2>/dev/null",
        [OldRootKey, OldRootCert, CommonCN]
    ),
    _ = Sh(
        "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-days 2 -subj '/CN=~s' 2>/dev/null",
        [NewRootKey, NewRootCert, CommonCN]
    ),
    %% Sign the leaf with the NEW root.
    _ = Sh(
        "openssl req -new -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj '/CN=RolloverLeaf~s' 2>/dev/null",
        [LeafKey, LeafCsr, Suffix]
    ),
    _ = Sh(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 2 2>/dev/null",
        [LeafCsr, NewRootCert, NewRootKey, LeafCert]
    ),
    {ok, OldRootPem} = file:read_file(OldRootCert),
    {ok, NewRootPem} = file:read_file(NewRootCert),
    {ok, LeafPem} = file:read_file(LeafCert),
    [{'Certificate', OldRootDer, not_encrypted}] = public_key:pem_decode(OldRootPem),
    [{'Certificate', NewRootDer, not_encrypted}] = public_key:pem_decode(NewRootPem),
    [{'Certificate', LeafDer, not_encrypted}] = public_key:pem_decode(LeafPem),
    {OldRootDer, NewRootDer, LeafDer}.
