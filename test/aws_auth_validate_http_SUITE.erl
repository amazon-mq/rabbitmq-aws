%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% Integration tests for the HTTP validation backend against a real, local
%% stub HTTP(S) server. These exercise the parts of aws_auth_validate_http
%% that the unit tests cannot: the actual httpc probe -> connect -> TLS
%% handshake -> response-classification path.
%%
%% Unlike the LDAP suite (which needs an external slapd), the stub server here
%% is an in-process `inets' httpd, so there is no external dependency and the
%% suite does not skip -- a green run always means the probe path executed.
%% Two listeners are started:
%%   * a plaintext HTTP listener that returns a configurable status per path
%%     (200/201 -> ok, 4xx/5xx -> auth_failed), and
%%   * an HTTPS listener with a SELF-SIGNED cert (for verify_peer -> tls_failed).
%% A closed port (nothing listening) drives connection_failed.
%%
%% SSRF note: the HTTP backend hard-denies loopback (127.0.0.0/8) with no
%% allow-private knob (its policy only ever permits customer VPC ranges, never
%% broker-local loopback). The stub necessarily listens on loopback, so this
%% suite mecks aws_auth_validate_http:classify_ip/1 to allow loopback for the
%% duration of the run -- the HTTP analogue of the LDAP suite's
%% auth_validation_allow_private_networks. Production classify_ip is unchanged.
-module(aws_auth_validate_http_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/httpd.hrl").

%% Named public ETS set the stub's do/1 records the received "host" header into,
%% keyed by request path (created in init_per_suite, deleted in end_per_suite).
%% Lets a test prove the pin connected to the IP but preserved the original
%% hostname in the Host header.
-define(HOST_HEADER_TABLE, aws_auth_validate_http_SUITE_host_headers).

all() ->
    [
        {group, http}
    ].

groups() ->
    [
        {http, [], [
            ok_200_returns_ok,
            ok_201_returns_ok,
            deny_body_returns_ok,
            non_auth_200_body_returns_auth_failed,
            client_error_404_returns_auth_failed,
            server_error_500_returns_auth_failed,
            unreachable_port_returns_connection_failed,
            self_signed_under_verify_peer_returns_tls_failed,
            self_signed_under_verify_none_returns_ok,
            custom_ca_under_verify_peer_returns_ok,
            mtls_client_cert_presented_returns_ok,
            mtls_client_cert_missing_returns_tls_failed,
            hostname_pin_preserves_host_ok,
            hostname_resolving_to_infra_denied,
            %% Credentialed-probe path integration tests (F4/F5).
            credentialed_allow_returns_ok,
            credentialed_deny_returns_auth_denied,
            credentialed_non_auth_returns_endpoint,
            credentialed_no_assume_role_returns_config_conflict,
            credentialed_assume_role_failure_returns_input_invalid,
            credentialed_password_never_in_error
        ]}
    ].

%%--------------------------------------------------------------------
%% Suite setup / teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    %% run_setup_steps/1 installs the long_running_testsuite_monitor that
    %% init_per_testcase's testcase_started/2 sends to (see the LDAP suite's
    %% note: without it every case auto-skips and `make ct' exits non-zero).
    Config = rabbit_ct_helpers:run_setup_steps(Config0),
    %% Track whether WE start the inets application so end_per_suite can leave
    %% the node as it found it. CT runs all suites in one node: if we leave
    %% inets running, a later suite that does `ok = inets:start()' (e.g.
    %% aws_auth_validate_mgmt_SUITE) hits {error,{already_started,inets}} and
    %% fails its init_per_suite. ensure_all_started returns the apps IT started.
    {ok, StartedInets} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    PrivDir = ?config(priv_dir, Config),
    %% Start the plaintext stub: /ok200, /ok201, /denied404, /error500.
    {HttpPid, HttpPort} = start_http_stub(PrivDir),
    %% Start the HTTPS stub with a freshly-generated self-signed cert.
    {HttpsPid, HttpsPort} = start_https_stub(PrivDir),
    %% A second HTTPS stub serving a CA-signed leaf; the CA PEM is returned so a
    %% test can resolve a cacertfile_arn to it (the verify_peer success path).
    {CaHttpsPid, CaHttpsPort, CaPem} = start_ca_https_stub(PrivDir),
    %% A mutual-TLS stub that REQUIRES a client cert; returns the CA PEM (to
    %% verify the server) and the client cert+key PEMs (for the validator to
    %% present) so the mTLS path can be exercised end to end.
    {MtlsPid, MtlsPort, MtlsCaPem, ClientCertPem, ClientKeyPem} = start_mtls_https_stub(PrivDir),
    [
        {http_stub_pid, HttpPid},
        {http_port, HttpPort},
        {https_stub_pid, HttpsPid},
        {https_port, HttpsPort},
        {ca_https_stub_pid, CaHttpsPid},
        {ca_https_port, CaHttpsPort},
        {ca_pem, CaPem},
        {mtls_stub_pid, MtlsPid},
        {mtls_port, MtlsPort},
        {mtls_ca_pem, MtlsCaPem},
        {client_cert_pem, ClientCertPem},
        {client_key_pem, ClientKeyPem},
        {started_inets, lists:member(inets, StartedInets)}
        | Config
    ].

end_per_suite(Config) ->
    catch inets:stop(httpd, ?config(http_stub_pid, Config)),
    catch inets:stop(httpd, ?config(https_stub_pid, Config)),
    catch inets:stop(httpd, ?config(ca_https_stub_pid, Config)),
    catch inets:stop(httpd, ?config(mtls_stub_pid, Config)),
    %% Only stop the inets application if this suite started it, so we leave
    %% the CT node exactly as we found it for later suites.
    case ?config(started_inets, Config) of
        true -> application:stop(inets);
        _ -> ok
    end,
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_group(_Group, Config) ->
    %% Allow loopback so the probe reaches the local stub (see module header).
    %% auth_validation_allow_private_networks relaxes ONLY loopback in
    %% classify_ip -- the HTTP analogue of the LDAP suite's local-slapd flag.
    application:set_env(aws, auth_validation_allow_private_networks, true),
    %% Resolving any ARN (cacertfile_arn / certfile_arn / keyfile_arn) now
    %% requires a configured assume_role -- the instance-role fallback is refused
    %% with config_conflict. Configure one for the whole group and stub the STS
    %% call so it threads through unchanged (resolve_arn is itself mecked per
    %% case). The no-ARN reachability cases are unaffected (they never resolve).
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
%% Functional tests -- the five probe outcomes
%%--------------------------------------------------------------------

ok_200_returns_ok(Config) ->
    ?assertEqual(ok, validate(http_body(Config, "/ok200"))).

ok_201_returns_ok(Config) ->
    ?assertEqual(ok, validate(http_body(Config, "/ok201"))).

deny_body_returns_ok(Config) ->
    %% A 200 with a well-formed `deny' body is a SUCCESS: the synthetic probe
    %% principal is expected to be denied, and a clean deny still proves the
    %% endpoint speaks the auth protocol.
    ?assertEqual(ok, validate(http_body(Config, "/denyok"))).

non_auth_200_body_returns_auth_failed(Config) ->
    %% A 200 whose body is neither allow nor deny (e.g. an HTML health page)
    %% means the path points at something that is not an auth backend. The
    %% response-contract guard rejects it even though the status is 200 --
    %% the false-pass this feature closes.
    ?assertMatch({error, auth_failed, _}, validate(http_body(Config, "/notauth"))).

client_error_404_returns_auth_failed(Config) ->
    ?assertMatch({error, auth_failed, _}, validate(http_body(Config, "/denied404"))).

server_error_500_returns_auth_failed(Config) ->
    ?assertMatch({error, auth_failed, _}, validate(http_body(Config, "/error500"))).

unreachable_port_returns_connection_failed(_Config) ->
    %% Port 1 on loopback: nothing listens, so the connect fails fast.
    Body = #{
        <<"user_path">> => <<"http://127.0.0.1:1/ok200">>,
        <<"http_method">> => <<"get">>
    },
    ?assertMatch({error, connection_failed, _}, validate(Body)).

self_signed_under_verify_peer_returns_tls_failed(Config) ->
    %% The HTTPS stub serves a self-signed cert. Under verify_peer (with the
    %% host OS trust store, which does not trust it) the handshake must fail.
    Port = ?config(https_port, Config),
    Url = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/ok200"]),
    Body = #{
        <<"user_path">> => Url,
        <<"http_method">> => <<"get">>,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_peer">>}
    },
    ?assertMatch({error, tls_failed, _}, validate(Body)).

self_signed_under_verify_none_returns_ok(Config) ->
    %% Same self-signed HTTPS stub, but verify_none accepts any cert, so the
    %% 200 from /ok200 classifies as ok. Proves the TLS path itself works.
    Port = ?config(https_port, Config),
    Url = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/ok200"]),
    Body = #{
        <<"user_path">> => Url,
        <<"http_method">> => <<"get">>,
        <<"ssl_options">> => #{<<"verify">> => <<"verify_none">>}
    },
    ?assertEqual(ok, validate(Body)).

custom_ca_under_verify_peer_returns_ok(Config) ->
    %% verify_peer SUCCESS with a customer CA supplied via cacertfile_arn.
    %% Regression guard: if cacerts is built from pem_entry_decode/1 records,
    %% ssl ignores them (unknown_ca -> tls_failed); the CA must be passed as
    %% raw DER. resolve_arn is mecked (aws_lib threads aws_state(), so the mock
    %% takes the state and returns it unchanged in the {ok, Data, State} tuple);
    %% the real httpc probe runs.
    CaPem = ?config(ca_pem, Config),
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, CaPem, State} end),
        Port = ?config(ca_https_port, Config),
        Url = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/ok200"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>,
            <<"ssl_options">> => #{
                <<"verify">> => <<"verify_peer">>,
                <<"cacertfile_arn">> => <<"arn:aws:s3:::test-ca/ca.pem">>
            }
        },
        ?assertEqual(ok, validate(Body))
    after
        meck:unload(aws_arn_util)
    end.

mtls_client_cert_presented_returns_ok(Config) ->
    %% End-to-end mTLS SUCCESS. The stub requires a client cert (verify_peer +
    %% fail_if_no_peer_cert). The validator resolves cacertfile_arn (to verify
    %% the server), certfile_arn, and keyfile_arn (to PRESENT a client cert+key
    %% signed by the same CA). The handshake completes only because the client
    %% cert is actually wired through certfile_arn/keyfile_arn -> ssl
    %% {cert,_}/{key,_} -> httpc. resolve_arn is mecked per-ARN; the real httpc
    %% probe runs against the mTLS stub.
    CaPem = ?config(mtls_ca_pem, Config),
    CertPem = ?config(client_cert_pem, Config),
    KeyPem = ?config(client_key_pem, Config),
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(Arn, State) ->
            {ok, mtls_pem_for(Arn, CaPem, CertPem, KeyPem), State}
        end),
        Port = ?config(mtls_port, Config),
        Url = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/ok200"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>,
            <<"ssl_options">> => #{
                <<"verify">> => <<"verify_peer">>,
                <<"cacertfile_arn">> => <<"arn:aws:s3:::test-ca/ca.pem">>,
                <<"certfile_arn">> => <<"arn:aws:s3:::test-client/cert.pem">>,
                <<"keyfile_arn">> =>
                    <<"arn:aws:secretsmanager:us-east-1:111122223333:secret:client-key">>
            }
        },
        ?assertEqual(ok, validate(Body))
    after
        meck:unload(aws_arn_util)
    end.

mtls_client_cert_missing_returns_tls_failed(Config) ->
    %% End-to-end mTLS NEGATIVE. Same stub (requires a client cert), but the
    %% request supplies ONLY cacertfile_arn -- no certfile_arn/keyfile_arn -- so
    %% the validator presents no client cert. The server rejects the handshake
    %% (fail_if_no_peer_cert), which must surface as tls_failed (not a false
    %% success and not connection_failed). This proves the client cert is
    %% load-bearing: without it, the same target that passes above now fails.
    CaPem = ?config(mtls_ca_pem, Config),
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, CaPem, State} end),
        Port = ?config(mtls_port, Config),
        Url = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/ok200"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>,
            <<"ssl_options">> => #{
                <<"verify">> => <<"verify_peer">>,
                <<"cacertfile_arn">> => <<"arn:aws:s3:::test-ca/ca.pem">>
            }
        },
        ?assertMatch({error, tls_failed, _}, validate(Body))
    after
        meck:unload(aws_arn_util)
    end.

%%--------------------------------------------------------------------
%% Hostname resolve+pin+rewrite (DNS-rebinding defense)
%%--------------------------------------------------------------------
%%
%% The 12 cases above all target the literal 127.0.0.1, which takes the
%% literal-IP branch of resolve_and_pin/1. These two exercise the hostname
%% branch (resolve_hostname_and_pin/2), the central SSRF/DNS-rebinding control:
%% resolve to ALL A/AAAA, DENY if ANY resolved address is broker infra, else pin
%% to the resolved IP while keeping the original hostname in the Host header/SNI.

hostname_pin_preserves_host_ok(Config) ->
    %% Positive: a HOSTNAME (localhost, not a literal IP) resolves to loopback,
    %% which auth_validation_allow_private_networks (set for the group) permits,
    %% so the probe reaches the plaintext stub on 127.0.0.1 and returns ok. We
    %% meck inet:getaddrs/2 so localhost resolves DETERMINISTICALLY to the stub's
    %% 127.0.0.1 (inet) with no AAAA (inet6 nxdomain) -- otherwise a host that
    %% returned ::1 first could pin to a loopback the stub is not bound to. The
    %% host stays a NAME, so resolve_hostname_and_pin/2 runs: it pins the connect
    %% target to the IP but build_request/4 keeps "localhost" in the Host header.
    %% We then assert the stub RECEIVED Host: localhost, proving the rewrite kept
    %% the original hostname while connecting to the pinned IP.
    Port = ?config(http_port, Config),
    %% Own the capture table in THIS (test-case) process. A named_table created
    %% in init_per_suite would not survive: CT runs init_per_suite in a separate
    %% process that terminates on return, taking its ETS tables with it. The
    %% stub's do/1 (a distinct httpd process) writes to it by name while this
    %% process -- the owner -- is alive; public access permits the cross-process
    %% write. Deleted in the `after' clause.
    ?HOST_HEADER_TABLE = ets:new(?HOST_HEADER_TABLE, [named_table, public, set]),
    ok = meck:new(inet, [passthrough, unstick]),
    try
        meck:expect(inet, getaddrs, fun
            ("localhost", inet) -> {ok, [{127, 0, 0, 1}]};
            ("localhost", inet6) -> {error, nxdomain};
            (H, Family) -> meck:passthrough([H, Family])
        end),
        Url = iolist_to_binary(["http://localhost:", integer_to_list(Port), "/ok200"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>
        },
        ?assertEqual(ok, validate(Body)),
        %% The stub saw the original hostname in Host, not the pinned IP. Host is
        %% carried without the port (build_request/4 sends the bare hostname).
        ?assertEqual("localhost", ets:lookup_element(?HOST_HEADER_TABLE, "/ok200", 2))
    after
        meck:unload(inet),
        catch ets:delete(?HOST_HEADER_TABLE)
    end.

hostname_resolving_to_infra_denied(_Config) ->
    %% Negative (DNS-rebinding deny): a benign-looking hostname resolves to a
    %% DENIED infra address (IMDS 169.254.169.254), so the "deny if ANY resolved
    %% address is broker infra" branch must fire -- even with allow_private_
    %% networks on, which relaxes ONLY loopback. We meck inet:getaddrs/2 for both
    %% families: the denied v4 for inet, nxdomain for inet6, so resolve_all/1
    %% yields exactly the denied address and the deny is deterministic. The host
    %% is a NAME (not a literal IP) so it passes the pure url_allowed/1 check and
    %% is only caught in the probe-layer resolve+classify.
    ok = meck:new(inet, [passthrough, unstick]),
    try
        meck:expect(inet, getaddrs, fun
            ("rebind.test", inet) -> {ok, [{169, 254, 169, 254}]};
            ("rebind.test", inet6) -> {error, nxdomain};
            (H, Family) -> meck:passthrough([H, Family])
        end),
        Body = #{
            <<"user_path">> => <<"http://rebind.test/ok200">>,
            <<"http_method">> => <<"get">>
        },
        ?assertEqual(
            {error, input_invalid, <<"a configured path targets a disallowed address">>},
            validate(Body)
        )
    after
        meck:unload(inet)
    end.

%%--------------------------------------------------------------------
%% Credentialed-probe integration tests (F4: end-to-end wiring, F5: guardrails)
%%--------------------------------------------------------------------
%%
%% These exercise the full validate/1 -> probe_query_and_classifier ->
%% credentialed_query_for -> classify_credentialed_response wiring through the
%% in-process httpd stub. They mock ARN resolution (aws_arn_util:resolve_arn)
%% to return a fixed password, since no real Secrets Manager is available, but
%% the rest of the path -- including the POST override, the response classifier,
%% and the assume-role guardrails -- runs for real.

credentialed_allow_returns_ok(Config) ->
    %% Stub returns "allow" on user_path -> credentialed probe should succeed.
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
            {ok, <<"test-password">>, State}
        end),
        Port = ?config(http_port, Config),
        Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/ok200"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>,
            <<"username">> => <<"alice">>,
            <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
        },
        ?assertEqual(ok, validate(Body))
    after
        meck:unload(aws_arn_util)
    end.

credentialed_deny_returns_auth_denied(Config) ->
    %% Stub returns "deny" on user_path -> credentialed probe reports auth_denied.
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
            {ok, <<"test-password">>, State}
        end),
        Port = ?config(http_port, Config),
        Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/denyok"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>,
            <<"username">> => <<"alice">>,
            <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
        },
        ?assertEqual(
            {error, auth_failed, <<"HTTP auth server denied the supplied credentials">>},
            validate(Body)
        )
    after
        meck:unload(aws_arn_util)
    end.

credentialed_non_auth_returns_endpoint(Config) ->
    %% Stub returns non-auth body ("hello") on user_path -> report REASON_ENDPOINT
    %% (not an auth server), distinct from a credential denial. Tests F3's fix.
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
            {ok, <<"test-password">>, State}
        end),
        Port = ?config(http_port, Config),
        Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/notauth"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"get">>,
            <<"username">> => <<"alice">>,
            <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
        },
        ?assertEqual(
            {error, auth_failed, <<"HTTP auth server did not return a usable response">>},
            validate(Body)
        )
    after
        meck:unload(aws_arn_util)
    end.

credentialed_no_assume_role_returns_config_conflict(_Config) ->
    %% F5: credentialed mode with NO assume_role_arn configured -> config_conflict.
    %% Temporarily remove the arn_config env (set in init_per_group) and unmeck
    %% aws_iam so the real code path runs.
    OldArn = application:get_env(aws, arn_config),
    application:unset_env(aws, arn_config),
    try
        Body = #{
            <<"user_path">> => <<"http://8.8.8.8/auth">>,
            <<"http_method">> => <<"get">>,
            <<"username">> => <<"alice">>,
            <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
        },
        ?assertMatch(
            {error, config_conflict, _},
            validate(Body)
        )
    after
        case OldArn of
            {ok, V} -> application:set_env(aws, arn_config, V);
            undefined -> ok
        end
    end.

credentialed_assume_role_failure_returns_input_invalid(_Config) ->
    %% F5: assume-role failure -> input_invalid. Temporarily override the aws_iam
    %% mock to return an error.
    meck:expect(aws_iam, assume_role, fun(_RoleArn, _State) -> {error, sts_error} end),
    try
        Body = #{
            <<"user_path">> => <<"http://8.8.8.8/auth">>,
            <<"http_method">> => <<"get">>,
            <<"username">> => <<"alice">>,
            <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
        },
        ?assertMatch(
            {error, input_invalid, _},
            validate(Body)
        )
    after
        %% Restore the success mock for remaining tests.
        meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end)
    end.

credentialed_password_never_in_error(Config) ->
    %% F5: the resolved password must NEVER appear in any error tuple returned by
    %% the validator (security invariant R6). Drive a credentialed request whose
    %% endpoint returns "deny" (an auth_failed error) and assert the password
    %% binary does not appear as a substring of the reason.
    ResolvedPassword = <<"SuperS3cret!P@ssword#123">>,
    ok = meck:new(aws_arn_util, [passthrough]),
    try
        meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
            {ok, ResolvedPassword, State}
        end),
        Port = ?config(http_port, Config),
        Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/denyok"]),
        Body = #{
            <<"user_path">> => Url,
            <<"http_method">> => <<"post">>,
            <<"username">> => <<"alice">>,
            <<"password_arn">> => <<"arn:aws:secretsmanager:us-east-1:111:secret:pw">>
        },
        Result = validate(Body),
        %% Result is {error, auth_failed, Reason}; assert password not in Reason.
        {error, _, Reason} = Result,
        ?assertEqual(nomatch, binary:match(Reason, ResolvedPassword))
    after
        meck:unload(aws_arn_util)
    end.

%% Route a resolve_arn call to the right PEM by ARN. The validator passes the
%% ARN as a string (binary_to_list); match on the resource portion. certfile is
%% S3-hosted, keyfile is Secrets Manager, cacert is the CA bundle.
mtls_pem_for(Arn, CaPem, CertPem, KeyPem) ->
    A = lists:flatten(Arn),
    case {string:find(A, "client/cert"), string:find(A, "client-key")} of
        {nomatch, nomatch} -> CaPem;
        {nomatch, _} -> KeyPem;
        {_, _} -> CertPem
    end.

%%--------------------------------------------------------------------
%% Request bodies + driver
%%--------------------------------------------------------------------

http_body(Config, Path) ->
    Port = ?config(http_port, Config),
    Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), Path]),
    #{
        <<"user_path">> => Url,
        <<"http_method">> => <<"get">>
    }.

%% Drive the backend exactly as the registry would: validate/1 on a body
%% already filtered to allowed_fields. The full HTTP request pipeline (auth,
%% body cap, dispatch) is covered by aws_auth_validate_mgmt_SUITE.
validate(Body) ->
    aws_auth_validate_http:validate(Body).

%%--------------------------------------------------------------------
%% Stub HTTP(S) servers (in-process inets httpd)
%%--------------------------------------------------------------------

%% A mod that maps the request path to a fixed status code, so each test can
%% target a path that yields the outcome it asserts.
start_http_stub(PrivDir) ->
    {ok, Pid} = inets:start(httpd, [
        {port, 0},
        {server_name, "aws_http_stub"},
        {server_root, PrivDir},
        {document_root, PrivDir},
        {bind_address, {127, 0, 0, 1}},
        {modules, [?MODULE]}
    ]),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid, Port}.

start_https_stub(PrivDir) ->
    {CertFile, KeyFile} = gen_self_signed(PrivDir),
    {ok, Pid} = inets:start(httpd, [
        {port, 0},
        {server_name, "aws_https_stub"},
        {server_root, PrivDir},
        {document_root, PrivDir},
        {bind_address, {127, 0, 0, 1}},
        {socket_type, {ssl, [{certfile, CertFile}, {keyfile, KeyFile}]}},
        {modules, [?MODULE]}
    ]),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid, Port}.

%% inets httpd callback module: respond purely from the request path. The probe
%% appends a query string (e.g. /ok200?username=probe) for GET, so match on the
%% path component only, before the "?".
%%
%% Bodies mirror rabbit_auth_backend_http's allow/deny grammar so the backend's
%% response-contract check passes for the auth-shaped paths. /notauth returns a
%% 200 with a non-auth body to exercise the false-pass guard (200 but not an
%% auth backend -> auth_failed).
do(Info) ->
    Path = hd(string:split(Info#mod.request_uri, "?")),
    %% Record the received "host" header (parsed_header keys are lowercased) so a
    %% test can assert the pin kept the original hostname in Host even though the
    %% connect target was rewritten to the IP. Guarded by ets:info so the write
    %% is a no-op if the table is absent -- keeps do/1 backward-compatible for
    %% the existing cases (which never read it) and cannot alter the response.
    case ets:info(?HOST_HEADER_TABLE) of
        undefined ->
            ok;
        _ ->
            HostHdr = proplists:get_value("host", Info#mod.parsed_header, undefined),
            true = ets:insert(?HOST_HEADER_TABLE, {Path, HostHdr})
    end,
    {Status, Body} =
        case Path of
            "/ok200" -> {200, "allow"};
            "/ok201" -> {201, "allow administrator management"};
            "/denyok" -> {200, "deny"};
            "/notauth" -> {200, "<html>hello</html>"};
            "/denied404" -> {404, "deny"};
            "/error500" -> {500, "error"};
            _ -> {404, "deny"}
        end,
    Head = [
        {code, Status},
        {content_type, "text/plain"},
        {content_length, integer_to_list(length(Body))}
    ],
    {proceed, [{response, {response, Head, Body}}]}.

%%--------------------------------------------------------------------
%% Self-signed certificate generation (for the TLS listener)
%%--------------------------------------------------------------------

%% Generate a self-signed RSA cert + key into PrivDir and return their paths.
%% Uses the same approach rabbitmq test suites use: shell out to openssl, which
%% is available wherever the broker test toolchain runs.
gen_self_signed(PrivDir) ->
    CertFile = filename:join(PrivDir, "stub-cert.pem"),
    KeyFile = filename:join(PrivDir, "stub-key.pem"),
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

%% HTTPS stub whose leaf cert is signed by a generated test CA. Returns the CA
%% PEM so a test can point a cacertfile_arn at it and have verify_peer succeed.
start_ca_https_stub(PrivDir) ->
    {CaPem, CertFile, KeyFile} = gen_ca_signed(PrivDir),
    {ok, Pid} = inets:start(httpd, [
        {port, 0},
        {server_name, "aws_https_ca_stub"},
        {server_root, PrivDir},
        {document_root, PrivDir},
        {bind_address, {127, 0, 0, 1}},
        {socket_type, {ssl, [{certfile, CertFile}, {keyfile, KeyFile}]}},
        {modules, [?MODULE]}
    ]),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid, Port, CaPem}.

%% HTTPS stub that REQUIRES a client certificate (mutual TLS). It presents a
%% CA-signed server leaf AND demands a client cert signed by the same test CA
%% (verify_peer + fail_if_no_peer_cert). The handshake therefore only completes
%% when the validator actually presents a client cert+key -- the end-to-end
%% proof that the mTLS path (certfile_arn/keyfile_arn -> ssl {cert,_}/{key,_})
%% is wired through to httpc. Returns the CA PEM (so the client can verify the
%% server) plus the client cert+key PEMs (so the client can present them).
%% Pinned to tlsv1.2 so the client-cert check fails synchronously at handshake
%% (TLS 1.3 can defer the alert), keeping the negative case deterministic.
start_mtls_https_stub(PrivDir) ->
    {CaPem, ServerCert, ServerKey, ClientCertPem, ClientKeyPem} = gen_mtls_certs(PrivDir),
    CaFile = filename:join(PrivDir, "mtls-ca-cert.pem"),
    {ok, Pid} = inets:start(httpd, [
        {port, 0},
        {server_name, "aws_https_mtls_stub"},
        {server_root, PrivDir},
        {document_root, PrivDir},
        {bind_address, {127, 0, 0, 1}},
        {socket_type,
            {ssl, [
                {certfile, ServerCert},
                {keyfile, ServerKey},
                {cacertfile, CaFile},
                {verify, verify_peer},
                {fail_if_no_peer_cert, true},
                {versions, ['tlsv1.2']}
            ]}},
        {modules, [?MODULE]}
    ]),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid, Port, CaPem, ClientCertPem, ClientKeyPem}.

%% Generate a test CA, a server leaf (SAN IP:127.0.0.1 for hostname verification
%% against the pinned loopback), and a client leaf+key -- both leaves signed by
%% the same CA. Returns {CaPem, ServerCertFile, ServerKeyFile, ClientCertPem,
%% ClientKeyPem}; the CA PEM is also written to mtls-ca-cert.pem for the server's
%% cacertfile. The client cert+key are returned as PEM binaries so a test can
%% meck resolve_arn to hand them to the validator.
gen_mtls_certs(PrivDir) ->
    J = fun(Name) -> filename:join(PrivDir, Name) end,
    CaKey = J("mtls-ca-key.pem"),
    CaCert = J("mtls-ca-cert.pem"),
    SrvKey = J("mtls-srv-key.pem"),
    SrvCsr = J("mtls-srv.csr"),
    SrvCert = J("mtls-srv-cert.pem"),
    SrvExt = J("mtls-srv-ext.cnf"),
    CliKey = J("mtls-cli-key.pem"),
    CliCsr = J("mtls-cli.csr"),
    CliCert = J("mtls-cli-cert.pem"),
    ok = file:write_file(SrvExt, "subjectAltName=IP:127.0.0.1\n"),
    Run = fun(Fmt, Args) -> _ = os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    %% Test CA.
    Run(
        "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-days 1 -subj /CN=AwsAuthValidateMtlsCA 2>/dev/null",
        [CaKey, CaCert]
    ),
    %% Server leaf (SAN IP:127.0.0.1) signed by the CA.
    Run(
        "openssl req -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj /CN=127.0.0.1 2>/dev/null",
        [SrvKey, SrvCsr]
    ),
    Run(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 1 -extfile ~ts 2>/dev/null",
        [SrvCsr, CaCert, CaKey, SrvCert, SrvExt]
    ),
    %% Client leaf signed by the same CA (no SAN needed; the server verifies the
    %% chain, not a hostname).
    Run(
        "openssl req -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-subj /CN=rabbitmq-validation-probe 2>/dev/null",
        [CliKey, CliCsr]
    ),
    Run(
        "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts -CAcreateserial "
        "-out ~ts -days 1 2>/dev/null",
        [CliCsr, CaCert, CaKey, CliCert]
    ),
    true =
        filelib:is_regular(SrvCert) andalso filelib:is_regular(SrvKey) andalso
            filelib:is_regular(CliCert) andalso filelib:is_regular(CliKey),
    {ok, CaPem} = file:read_file(CaCert),
    {ok, ClientCertPem} = file:read_file(CliCert),
    {ok, ClientKeyPem} = file:read_file(CliKey),
    {CaPem, SrvCert, SrvKey, ClientCertPem, ClientKeyPem}.

%% Generate a test CA + a leaf signed by it. The leaf has subjectAltName=
%% IP:127.0.0.1 so hostname verification passes against the pinned loopback IP.
gen_ca_signed(PrivDir) ->
    J = fun(Name) -> filename:join(PrivDir, Name) end,
    CaKey = J("ca-key.pem"),
    CaCert = J("ca-cert.pem"),
    LeafKey = J("leaf-key.pem"),
    LeafCsr = J("leaf.csr"),
    LeafCert = J("leaf-cert.pem"),
    ExtFile = J("leaf-ext.cnf"),
    ok = file:write_file(ExtFile, "subjectAltName=IP:127.0.0.1\n"),
    Run = fun(Fmt, Args) -> _ = os:cmd(lists:flatten(io_lib:format(Fmt, Args))) end,
    Run(
        "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~ts -out ~ts "
        "-days 1 -subj /CN=AwsAuthValidateTestCA 2>/dev/null",
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
