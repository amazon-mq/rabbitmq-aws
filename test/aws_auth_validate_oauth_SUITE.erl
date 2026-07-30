%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Integration tests for the OAuth validation backend against a real, local
%% stub HTTPS server. These exercise the parts of aws_auth_validate_oauth that
%% the unit tests cannot: the actual httpc probe -> connect -> TLS handshake ->
%% JWKS/OIDC-discovery fetch -> response-classification path.
%%
%% Like the HTTP suite, the stub server is an in-process `inets' httpd, so there
%% is no external dependency. The suite does, however, need `openssl' on PATH to
%% mint the TLS material (OAuth is https-only, so every probe is over TLS); if
%% openssl or the stub cannot start, init_per_suite returns {skip, _} rather than
%% failing -- the same graceful-skip posture the LDAP suite uses when no slapd is
%% reachable.
%%
%% Two HTTPS listeners are started, both on loopback:
%%   * a SELF-SIGNED listener (drives verify_none success and verify_peer ->
%%     tls_failed), and
%%   * a CA-SIGNED listener whose leaf carries SAN IP:127.0.0.1, so a test can
%%     point a cacertfile_arn at the generated CA and have verify_peer succeed.
%% A closed port (nothing listening) drives connection_failed.
%%
%% SSRF note: the OAuth backend hard-denies loopback (127.0.0.0/8) in production;
%% the test-only auth_validation_allow_private_networks flag relaxes ONLY
%% loopback (never IMDS/link-local) so the probe can reach the local stub. This
%% is the OAuth analogue of the HTTP/LDAP suites' identical flag. Production
%% classify_ip is unchanged.
-module(aws_auth_validate_oauth_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/httpd.hrl").

all() ->
    [
        {group, oauth}
    ].

groups() ->
    [
        {oauth, [], [
            direct_jwks_valid_returns_ok,
            issuer_discovery_valid_returns_ok,
            jwks_empty_keys_returns_auth_failed,
            jwks_non_jwks_body_returns_auth_failed,
            jwks_non_200_returns_auth_failed,
            discovery_missing_jwks_uri_returns_auth_failed,
            unreachable_port_returns_connection_failed,
            self_signed_verify_peer_returns_tls_failed,
            custom_ca_verify_peer_returns_ok,
            error_reason_leaks_no_target_detail,
            sts_direct_jwks_uri_returns_ok,
            sts_issuer_only_returns_auth_failed
        ]}
    ].

%%--------------------------------------------------------------------
%% Suite setup / teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    %% run_setup_steps gives us a priv_dir and the standard rabbit test config
    %% without starting a broker -- the backend runs in THIS node (as in the HTTP
    %% suite), so validate/1 and meck operate in-process.
    Config = rabbit_ct_helpers:run_setup_steps(Config0),
    {ok, StartedInets} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    PrivDir = ?config(priv_dir, Config),
    try
        {SelfPid, SelfPort} = start_self_signed_stub(PrivDir),
        {CaPid, CaPort, CaPem} = start_ca_signed_stub(PrivDir),
        [
            {self_stub_pid, SelfPid},
            {self_port, SelfPort},
            {ca_stub_pid, CaPid},
            {ca_port, CaPort},
            {ca_pem, CaPem},
            {started_inets, lists:member(inets, StartedInets)}
            | Config
        ]
    catch
        _Class:Reason ->
            %% Could not mint TLS material or start a stub (e.g. no openssl on a
            %% minimal CI image): skip rather than fail, matching the LDAP suite.
            {skip, {oauth_stub_unavailable, Reason}}
    end.

end_per_suite(Config) ->
    catch inets:stop(httpd, ?config(self_stub_pid, Config)),
    catch inets:stop(httpd, ?config(ca_stub_pid, Config)),
    case ?config(started_inets, Config) of
        true -> application:stop(inets);
        _ -> ok
    end,
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_group(_Group, Config) ->
    %% Relax ONLY loopback so the probe reaches the local stub (see header).
    application:set_env(aws, auth_validation_allow_private_networks, true),
    %% Resolving any ssl_options ARN (cacertfile_arn / certfile_arn / keyfile_arn)
    %% now requires a configured assume_role -- the instance-role fallback is
    %% refused with config_conflict. Configure one for the whole group and stub
    %% the STS call so it threads through unchanged (resolve_arn is itself mecked
    %% per case). The no-ARN reachability cases are unaffected (they never
    %% resolve, so they take the default state and no assume_role is required).
    application:set_env(aws, arn_config, [
        {assume_role_arn, "arn:aws:iam::123456789012:role/validation"}
    ]),
    ok = meck:new(aws_iam, [no_link]),
    ok = meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
    Config.

end_per_group(_Group, Config) ->
    application:unset_env(aws, auth_validation_allow_private_networks),
    application:unset_env(aws, arn_config),
    catch meck:unload(aws_iam),
    Config.

init_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TC).

end_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, TC).

%%--------------------------------------------------------------------
%% Functional tests -- the probe outcomes
%%--------------------------------------------------------------------

%% A direct jwks_uri that serves a well-formed JWKS over TLS -> ok. verify_none
%% accepts the self-signed cert, so this isolates the fetch + JWKS-shape check.
direct_jwks_valid_returns_ok(Config) ->
    Url = self_url(Config, "/jwks"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertEqual(ok, validate(Body)).

%% issuer-only: the backend fetches /.well-known/openid-configuration, extracts
%% jwks_uri, re-guards it, fetches it, and validates the JWKS -> ok.
issuer_discovery_valid_returns_ok(Config) ->
    Issuer = self_url(Config, ""),
    Body = #{
        <<"issuer">> => Issuer,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertEqual(ok, validate(Body)).

%% A 200 JWKS whose "keys" array is empty is not a usable signing-key source ->
%% auth_failed (endpoint reachable but not serving a usable JWKS).
jwks_empty_keys_returns_auth_failed(Config) ->
    Url = self_url(Config, "/jwks-empty"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertMatch({error, auth_failed, _}, validate(Body)).

%% A 200 whose body is valid JSON but has no "keys" field -> auth_failed.
jwks_non_jwks_body_returns_auth_failed(Config) ->
    Url = self_url(Config, "/jwks-notjwks"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertMatch({error, auth_failed, _}, validate(Body)).

%% A non-200 status from the JWKS endpoint -> auth_failed.
jwks_non_200_returns_auth_failed(Config) ->
    Url = self_url(Config, "/notfound"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertMatch({error, auth_failed, _}, validate(Body)).

%% An issuer whose discovery doc lacks jwks_uri -> auth_failed (reachable but not
%% OIDC-shaped). The "/no-jwks" issuer prefix drives the stub to omit jwks_uri.
discovery_missing_jwks_uri_returns_auth_failed(Config) ->
    Issuer = self_url(Config, "/no-jwks"),
    Body = #{
        <<"issuer">> => Issuer,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertMatch({error, auth_failed, _}, validate(Body)).

%% Nothing listening on loopback port 1 -> the connect fails fast ->
%% connection_failed.
unreachable_port_returns_connection_failed(_Config) ->
    Body = #{
        <<"jwks_uri">> => <<"https://127.0.0.1:1/jwks">>,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertMatch({error, connection_failed, _}, validate(Body)).

%% The self-signed stub under verify_peer (against the OS trust store, which
%% does not trust it) must fail the handshake -> tls_failed.
self_signed_verify_peer_returns_tls_failed(Config) ->
    Url = self_url(Config, "/jwks"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_peer">>}
    },
    ?assertMatch({error, tls_failed, _}, validate(Body)).

%% verify_peer SUCCESS with a customer CA supplied via cacertfile_arn. This is
%% the highest-value integration case: it exercises end to end BOTH the
%% assume_role gate (an ARN is referenced, so a role must be configured and
%% assumed) AND the cacerts DER fix (the CA must be handed to ssl as raw DER; if
%% it were pem_entry_decode/1 records, ssl would ignore them -> unknown_ca ->
%% a spurious tls_failed). resolve_arn is mecked to return the generated CA PEM;
%% the real httpc probe runs against the CA-signed stub.
custom_ca_verify_peer_returns_ok(Config) ->
    CaPem = ?config(ca_pem, Config),
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, CaPem, State} end),
        Url = ca_url(Config, "/jwks"),
        Body = #{
            <<"jwks_uri">> => Url,
            <<"ssl_options">> => #{
                <<"verify">> => <<"verify_peer">>,
                <<"cacertfile_arn">> => <<"arn:aws:s3:::test-ca/ca.pem">>
            }
        },
        ?assertEqual(ok, validate(Body))
    after
        meck:unload(aws_arn_util)
    end.

%% Fixed-category responses must not echo the target host, port, or URL (doing so
%% would enable SSRF reconnaissance). Drive an auth_failed (a 404 JWKS) and assert
%% the rendered result contains neither the loopback address nor the stub port.
error_reason_leaks_no_target_detail(Config) ->
    Port = ?config(self_port, Config),
    Url = self_url(Config, "/notfound"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    Result = validate(Body),
    Rendered = lists:flatten(io_lib:format("~p", [Result])),
    [
        ?assertMatch({error, auth_failed, _}, Result),
        ?assertEqual(nomatch, string:find(Rendered, "127.0.0.1")),
        ?assertEqual(nomatch, string:find(Rendered, integer_to_list(Port)))
    ].

%%--------------------------------------------------------------------
%% IAM-over-OAuth2 contract regression tests
%%--------------------------------------------------------------------
%%
%% IAM authentication for Amazon MQ for RabbitMQ is OAuth2 under the hood: AWS
%% STS outbound web identity federation issues JWTs that rabbitmq_auth_backend_
%% oauth2 verifies against the account's STS JWKS. There is no separate `iam'
%% validation backend -- an operator validates an IAM auth config by pointing
%% THIS oauth method at the STS JWKS URL (see the IAM validation guidance in the
%% customer docs). These two cases pin the STS-specific contract so a future
%% oauth refactor cannot silently break the documented IAM recipe.
%%
%% The STS JWKS layout differs from generic OIDC in one contract-critical way:
%% STS serves its signing keys at `{issuer}/.well-known/jwks.json' but does NOT
%% publish an OIDC discovery document at `{issuer}/.well-known/openid-
%% configuration'. Consequently:
%%   * the documented recipe -- pass the explicit `jwks_uri' (the STS issuer
%%     identifier + /.well-known/jwks.json) -- MUST succeed, and
%%   * the `issuer'-only path (which drives OIDC discovery) MUST fail against an
%%     STS-shaped endpoint, because discovery is unavailable.
%% The second case is the regression guard for the doc clarification "for IAM,
%% always pass jwks_uri, never issuer".
%%
%% The stub models the STS shape via the "/sts" path prefix (see do/1): it serves
%% a valid JWKS at /sts/.well-known/jwks.json but returns 404 for the OIDC
%% discovery path under the same prefix.

%% Documented IAM recipe: an explicit STS jwks_uri
%% ({issuer}/.well-known/jwks.json) serving a well-formed JWKS -> ok.
sts_direct_jwks_uri_returns_ok(Config) ->
    Url = self_url(Config, "/sts/.well-known/jwks.json"),
    Body = #{
        <<"jwks_uri">> => Url,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertEqual(ok, validate(Body)).

%% The STS issuer identifier passed as `issuer' drives OIDC discovery, which STS
%% does not serve (no /.well-known/openid-configuration) -> auth_failed. This is
%% why the IAM recipe requires jwks_uri, not issuer.
sts_issuer_only_returns_auth_failed(Config) ->
    Issuer = self_url(Config, "/sts"),
    Body = #{
        <<"issuer">> => Issuer,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertMatch({error, auth_failed, _}, validate(Body)).

%%--------------------------------------------------------------------
%% Stub HTTPS server (in-process inets httpd)
%%--------------------------------------------------------------------

start_self_signed_stub(PrivDir) ->
    {CertFile, KeyFile} = gen_self_signed(PrivDir),
    {ok, Pid} = inets:start(httpd, [
        {port, 0},
        {server_name, "aws_oauth_self_stub"},
        {server_root, PrivDir},
        {document_root, PrivDir},
        {bind_address, {127, 0, 0, 1}},
        {socket_type, {ssl, [{certfile, CertFile}, {keyfile, KeyFile}]}},
        {modules, [?MODULE]}
    ]),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid, Port}.

start_ca_signed_stub(PrivDir) ->
    {CaPem, CertFile, KeyFile} = gen_ca_signed(PrivDir),
    {ok, Pid} = inets:start(httpd, [
        {port, 0},
        {server_name, "aws_oauth_ca_stub"},
        {server_root, PrivDir},
        {document_root, PrivDir},
        {bind_address, {127, 0, 0, 1}},
        {socket_type, {ssl, [{certfile, CertFile}, {keyfile, KeyFile}]}},
        {modules, [?MODULE]}
    ]),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid, Port, CaPem}.

%% inets httpd callback: respond purely from the request path. The backend does
%% NOT append a query string, so match on the raw path.
%%
%% The OIDC discovery document's jwks_uri must point back at THIS listener, whose
%% port is dynamic ({port, 0}); we read the bound port from the server config_db
%% (httpd_util:lookup/2) so the doc is self-consistent regardless of port.
do(Info) ->
    Path = hd(string:split(Info#mod.request_uri, "?")),
    Port = httpd_util:lookup(Info#mod.config_db, port),
    Base = "https://127.0.0.1:" ++ integer_to_list(Port),
    {Status, Body, CT} =
        case Path of
            "/.well-known/openid-configuration" ->
                %% Root issuer -> a discovery doc that DOES carry jwks_uri.
                Doc = rabbit_json:encode(#{
                    <<"issuer">> => list_to_binary(Base),
                    <<"jwks_uri">> => list_to_binary(Base ++ "/jwks")
                }),
                {200, Doc, "application/json"};
            "/no-jwks/.well-known/openid-configuration" ->
                %% Issuer whose discovery doc OMITS jwks_uri -> auth_failed.
                Doc = rabbit_json:encode(#{
                    <<"issuer">> => list_to_binary(Base ++ "/no-jwks"),
                    <<"authorization_endpoint">> => list_to_binary(Base ++ "/authorize")
                }),
                {200, Doc, "application/json"};
            "/sts/.well-known/jwks.json" ->
                %% AWS STS serves signing keys here (the documented IAM recipe's
                %% jwks_uri). Same well-formed JWKS as /jwks.
                Jwks = rabbit_json:encode(#{
                    <<"keys">> => [#{<<"kty">> => <<"RSA">>, <<"kid">> => <<"sts1">>}]
                }),
                {200, Jwks, "application/json"};
            "/sts/.well-known/openid-configuration" ->
                %% STS does NOT publish an OIDC discovery document; the issuer-only
                %% path must therefore fail. 404 drives auth_failed.
                {404, <<"not found">>, "text/plain"};
            "/jwks" ->
                Jwks = rabbit_json:encode(#{
                    <<"keys">> => [#{<<"kty">> => <<"RSA">>, <<"kid">> => <<"k1">>}]
                }),
                {200, Jwks, "application/json"};
            "/jwks-empty" ->
                {200, rabbit_json:encode(#{<<"keys">> => []}), "application/json"};
            "/jwks-notjwks" ->
                {200, rabbit_json:encode(#{<<"something">> => <<"else">>}), "application/json"};
            _ ->
                {404, <<"not found">>, "text/plain"}
        end,
    Head = [
        {code, Status},
        {content_type, CT},
        {content_length, integer_to_list(byte_size(Body))}
    ],
    {proceed, [{response, {response, Head, Body}}]}.

%%--------------------------------------------------------------------
%% Certificate generation (shells out to openssl, as the HTTP suite does)
%%--------------------------------------------------------------------

%% Self-signed RSA cert + key. Returns their file paths.
gen_self_signed(PrivDir) ->
    CertFile = filename:join(PrivDir, "oauth-stub-cert.pem"),
    KeyFile = filename:join(PrivDir, "oauth-stub-key.pem"),
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes "
            "-keyout ~ts -out ~ts -days 1 "
            "-subj /CN=127.0.0.1 2>/dev/null",
            [KeyFile, CertFile]
        )
    ),
    _ = os:cmd(Cmd),
    true = filelib:is_regular(CertFile) andalso filelib:is_regular(KeyFile),
    {CertFile, KeyFile}.

%% Test CA + a leaf signed by it, leaf SAN=IP:127.0.0.1 so hostname verification
%% passes against the pinned loopback IP. Returns {CaPem, LeafCertFile, LeafKeyFile}.
gen_ca_signed(PrivDir) ->
    J = fun(Name) -> filename:join(PrivDir, Name) end,
    CaKey = J("oauth-ca-key.pem"),
    CaCert = J("oauth-ca-cert.pem"),
    LeafKey = J("oauth-leaf-key.pem"),
    LeafCsr = J("oauth-leaf.csr"),
    LeafCert = J("oauth-leaf-cert.pem"),
    ExtFile = J("oauth-leaf-ext.cnf"),
    ok = file:write_file(ExtFile, "subjectAltName=IP:127.0.0.1\n"),
    Run = fun(Fmt, Args) -> _ = os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    Run(
        "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-days 1 -subj /CN=AwsAuthValidateOauthTestCA 2>/dev/null",
        [CaKey, CaCert]
    ),
    Run(
        "openssl req -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj /CN=127.0.0.1 2>/dev/null",
        [LeafKey, LeafCsr]
    ),
    Run(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 1 -extfile ~ts 2>/dev/null",
        [LeafCsr, CaCert, CaKey, LeafCert, ExtFile]
    ),
    true = filelib:is_regular(LeafCert) andalso filelib:is_regular(LeafKey),
    {ok, CaPem} = file:read_file(CaCert),
    {CaPem, LeafCert, LeafKey}.

%%--------------------------------------------------------------------
%% Request bodies + driver
%%--------------------------------------------------------------------

self_url(Config, Path) ->
    Port = ?config(self_port, Config),
    iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), Path]).

ca_url(Config, Path) ->
    Port = ?config(ca_port, Config),
    iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), Path]).

%% Drive the backend exactly as the registry would: validate/1 on a body already
%% filtered to allowed_fields. The full request pipeline (auth, body cap,
%% dispatch, opt-in gate) is covered by aws_auth_validate_mgmt_SUITE.
validate(Body) ->
    aws_auth_validate_oauth:validate(Body).
