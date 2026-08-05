%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

%% End-to-end CT suite verifying that the ARN-resolution boot pass
%% (aws_arn_config:process_arn_config/1) reuses a single TCP connection when
%% multiple ARNs resolve to the same host:port, and opens distinct connections
%% only when the target host:port changes.
%%
%% This suite exercises REAL gun connections against a local HTTP listener
%% (via the AWS_ENDPOINT_URL override seam from PR #137). No real AWS
%% credentials or external network access are needed.
%%
%% Minimal boot path: no full broker start. Only the `aws` and `rabbit`
%% OTP applications need their env configured; gun and ssl are available at
%% test time as deps.
-module(aws_arn_config_reuse_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [
        multi_arn_same_host_single_connection,
        multi_arn_different_hosts_multiple_connections,
        connection_closed_after_pass,
        resolved_values_in_app_env
    ].

init_per_suite(Config0) ->
    Config = rabbit_ct_helpers:run_setup_steps(Config0),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(gun),
    PrivDir = ?config(priv_dir, Config),
    %% Generate test PEM files using openssl (same pattern as the HTTP SUITE).
    {CaCertFile, LeafCertFile, LeafKeyFile} = gen_test_certs(PrivDir),
    CaCertPem = read_file_bin(CaCertFile),
    LeafCertPem = read_file_bin(LeafCertFile),
    LeafKeyPem = read_file_bin(LeafKeyFile),
    %% Spawn a long-lived server process that owns the listen socket, the ETS
    %% tables, AND runs the accept loop. CT's init_per_suite runs in a temporary
    %% process -- everything owned by that process is destroyed when it exits.
    %% Putting all state in a single long-lived process avoids that issue.
    Self = self(),
    Server = spawn(fun() ->
        %% Create ETS tables in this process so it owns them.
        ets:new(conn_counter, [public, set, named_table]),
        ets:insert(conn_counter, {count, 0}),
        ets:new(pem_data, [public, set, named_table]),
        ets:insert(pem_data, {cacert_pem, CaCertPem}),
        ets:insert(pem_data, {cert_pem, LeafCertPem}),
        ets:insert(pem_data, {key_pem, LeafKeyPem}),
        %% Create the listen socket in this process.
        {ok, LSock} = gen_tcp:listen(0, [
            binary,
            {ip, {127, 0, 0, 1}},
            {active, false},
            {reuseaddr, true},
            {backlog, 128}
        ]),
        {ok, LPort} = inet:port(LSock),
        Self ! {server_ready, self(), LPort, LSock},
        %% Run the acceptor loop until killed.
        acceptor_loop(LSock, conn_counter)
    end),
    {Port, ListenSock} =
        receive
            {server_ready, Server, P, LS} -> {P, LS}
        after 5000 ->
            ct:fail("Server process did not start in time")
        end,
    ct:pal("Local listener started on 127.0.0.1:~b", [Port]),
    %% Set the endpoint override to redirect ALL AWS service calls locally.
    EndpointUrl = "http://127.0.0.1:" ++ integer_to_list(Port),
    true = os:putenv("AWS_ENDPOINT_URL", EndpointUrl),
    %% Mock credentials and region so no IMDS/metadata calls happen.
    %% no_link: the meck gen_server must outlive the init_per_suite process.
    ok = meck:new(aws_lib_config, [passthrough, no_link]),
    meck:expect(aws_lib_config, credentials, fun(Cfg) ->
        Creds =
            {aws_credentials, "AKIAIOSFODNN7EXAMPLE", "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
                undefined, undefined},
        {ok, Creds, Cfg}
    end),
    meck:expect(aws_lib_config, region, fun(Cfg) ->
        {ok, "us-east-1", Cfg}
    end),
    %% Mock assume_role to succeed trivially.
    ok = meck:new(aws_iam, [no_link]),
    meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
    [
        {listen_sock, ListenSock},
        {port, Port},
        {server, Server},
        {endpoint_url, EndpointUrl},
        {ca_cert_pem, CaCertPem},
        {leaf_cert_pem, LeafCertPem},
        {leaf_key_pem, LeafKeyPem}
        | Config
    ].

end_per_suite(Config) ->
    os:unsetenv("AWS_ENDPOINT_URL"),
    os:unsetenv("AWS_ENDPOINT_URL_S3"),
    os:unsetenv("AWS_ENDPOINT_URL_SECRETSMANAGER"),
    catch meck:unload(aws_lib_config),
    catch meck:unload(aws_iam),
    %% Kill the server process (owns listen socket + ETS tables).
    Server = ?config(server, Config),
    exit(Server, kill),
    application:unset_env(rabbit, ssl_options),
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TC),
    %% Reset connection counter before each test.
    ets:insert(conn_counter, {count, 0}),
    %% Clean application env.
    application:unset_env(rabbit, ssl_options),
    %% Restore the global endpoint override unconditionally. A prior test case
    %% may have unset it (e.g. to test per-service overrides) and then crashed
    %% before restoring it.
    true = os:putenv("AWS_ENDPOINT_URL", ?config(endpoint_url, Config)),
    os:unsetenv("AWS_ENDPOINT_URL_S3"),
    os:unsetenv("AWS_ENDPOINT_URL_SECRETSMANAGER"),
    Config.

end_per_testcase(TC, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, TC),
    Config.

%%====================================================================
%% Test cases
%%====================================================================

%% Three S3 ARNs all targeting the same host:port (same endpoint override).
%% The boot pass should open exactly 1 TCP connection and reuse it for all
%% three resolves.
multi_arn_same_host_single_connection(_Config) ->
    Tab = conn_counter,
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/test-role"},
        {arns, [
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/cacert.pem", ssl_cacertfile, [
                ssl_cacertfile, ssl_options, cacertfile
            ]},
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/cert.pem", ssl_certfile, [
                ssl_certfile, ssl_options, certfile
            ]},
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/key.pem", ssl_keyfile, [
                ssl_keyfile, ssl_options, keyfile
            ]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ct:pal("process_arn_config result: ~p", [Result]),
    ?assertMatch({ok, {iam_role_result, assumed}}, Result),
    %% Accept fires synchronously before gun can proceed; counter is stable.
    [{count, ConnCount}] = ets:lookup(Tab, count),
    ct:pal("TCP connections accepted: ~b (expected 1)", [ConnCount]),
    ?assertEqual(1, ConnCount).

%% Use per-service endpoint overrides pointing to different ports on localhost.
%% The S3 ARN goes to one port and the SecretsManager ARN goes to another.
%% This forces gun to see different host:port targets, resulting in 2 connections.
multi_arn_different_hosts_multiple_connections(Config) ->
    Tab = conn_counter,
    MainPort = ?config(port, Config),
    %% Start a second listener for the SecretsManager service.
    {ok, ListenSock2} = gen_tcp:listen(0, [
        binary,
        {ip, {127, 0, 0, 1}},
        {active, false},
        {reuseaddr, true},
        {backlog, 128}
    ]),
    {ok, Port2} = inet:port(ListenSock2),
    ct:pal("Second listener for SecretsManager on 127.0.0.1:~b", [Port2]),
    Acceptor2 = spawn(fun() -> acceptor_loop(ListenSock2, Tab) end),
    %% Per-service endpoint overrides.
    true = os:putenv("AWS_ENDPOINT_URL_S3", "http://127.0.0.1:" ++ integer_to_list(MainPort)),
    true = os:putenv(
        "AWS_ENDPOINT_URL_SECRETSMANAGER", "http://127.0.0.1:" ++ integer_to_list(Port2)
    ),
    %% Remove the global override so per-service ones take effect.
    os:unsetenv("AWS_ENDPOINT_URL"),
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/test-role"},
        {arns, [
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/cacert.pem", ssl_cacertfile, [
                ssl_cacertfile, ssl_options, cacertfile
            ]},
            {aws_arn_config_rabbit,
                "arn:aws:secretsmanager:us-east-1:123456789012:secret:test-secret", ssl_keyfile, [
                    ssl_keyfile, ssl_options, keyfile
                ]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ct:pal("process_arn_config result: ~p", [Result]),
    ?assertMatch({ok, {iam_role_result, assumed}}, Result),
    [{count, ConnCount}] = ets:lookup(Tab, count),
    ct:pal("TCP connections accepted: ~b (expected 2)", [ConnCount]),
    ?assertEqual(2, ConnCount),
    %% Clean up second listener.
    exit(Acceptor2, shutdown),
    gen_tcp:close(ListenSock2),
    %% Restore global override for remaining tests.
    true = os:putenv("AWS_ENDPOINT_URL", ?config(endpoint_url, Config)),
    os:unsetenv("AWS_ENDPOINT_URL_S3"),
    os:unsetenv("AWS_ENDPOINT_URL_SECRETSMANAGER").

%% After process_arn_config returns, the reuse connection is torn down.
%% The connection count should be exactly 1 (opened and closed cleanly).
connection_closed_after_pass(_Config) ->
    Tab = conn_counter,
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/test-role"},
        {arns, [
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/cacert.pem", ssl_cacertfile, [
                ssl_cacertfile, ssl_options, cacertfile
            ]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertMatch({ok, {iam_role_result, assumed}}, Result),
    [{count, ConnCount}] = ets:lookup(Tab, count),
    ct:pal("TCP connections accepted: ~b (expected 1)", [ConnCount]),
    ?assertEqual(1, ConnCount),
    %% After close_reuse_connection, the gun process should be terminated.
    %% Give a moment for the close to propagate, then verify no gun pids
    %% are connected to our port. We do this by checking that the gun:close
    %% call (which is invoked by close_reuse_connection -> aws_lib_httpc:close)
    %% actually killed the process. Since we cannot easily enumerate which gun
    %% processes are ours, we verify indirectly: a second call to process_arn_config
    %% with the same ARN should open a NEW connection (count goes from 1 to 2).
    ets:insert(Tab, {count, 0}),
    _ = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    [{count, ConnCount2}] = ets:lookup(Tab, count),
    ct:pal("TCP connections for second pass: ~b (expected 1 -- new connection)", [ConnCount2]),
    ?assertEqual(1, ConnCount2).

%% Verify the actual resolved values land in the rabbit application env.
resolved_values_in_app_env(_Config) ->
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/test-role"},
        {arns, [
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/cacert.pem", ssl_cacertfile, [
                ssl_cacertfile, ssl_options, cacertfile
            ]},
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/cert.pem", ssl_certfile, [
                ssl_certfile, ssl_options, certfile
            ]},
            {aws_arn_config_rabbit, "arn:aws:s3:::test-bucket/key.pem", ssl_keyfile, [
                ssl_keyfile, ssl_options, keyfile
            ]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertMatch({ok, {iam_role_result, assumed}}, Result),
    %% Verify ssl_options is set with the decoded PEM data.
    {ok, SslOpts} = application:get_env(rabbit, ssl_options),
    ct:pal("ssl_options after resolve: ~p", [SslOpts]),
    %% cacerts should contain DER-decoded certificates from the PEM we served.
    Cacerts = proplists:get_value(cacerts, SslOpts),
    ?assertNotEqual(undefined, Cacerts),
    ?assert(is_list(Cacerts)),
    ?assert(length(Cacerts) > 0),
    %% certs_keys should contain the cert and key entries.
    CertsKeys = proplists:get_value(certs_keys, SslOpts),
    ?assertNotEqual(undefined, CertsKeys),
    ?assert(is_list(CertsKeys)),
    [CertKeyMap | _] = CertsKeys,
    ?assert(maps:is_key(cert, CertKeyMap)),
    ?assert(maps:is_key(key, CertKeyMap)).

%%====================================================================
%% Internal -- cert generation
%%====================================================================

%% Generate a CA cert, leaf cert, and leaf key using openssl.
gen_test_certs(PrivDir) ->
    CaKeyFile = filename:join(PrivDir, "ca-key.pem"),
    CaCertFile = filename:join(PrivDir, "ca-cert.pem"),
    LeafKeyFile = filename:join(PrivDir, "leaf-key.pem"),
    LeafCsrFile = filename:join(PrivDir, "leaf.csr"),
    LeafCertFile = filename:join(PrivDir, "leaf-cert.pem"),
    %% Generate CA key and self-signed cert.
    CaCmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes "
            "-keyout ~ts -out ~ts -days 365 "
            "-subj '/CN=Test CA' 2>/dev/null",
            [CaKeyFile, CaCertFile]
        )
    ),
    _ = os:cmd(CaCmd),
    %% Generate leaf key and CSR.
    LeafKeyCmd = lists:flatten(
        io_lib:format(
            "openssl req -newkey rsa:2048 -nodes "
            "-keyout ~ts -out ~ts "
            "-subj '/CN=Test Leaf' 2>/dev/null",
            [LeafKeyFile, LeafCsrFile]
        )
    ),
    _ = os:cmd(LeafKeyCmd),
    %% Sign the leaf CSR with the CA.
    SignCmd = lists:flatten(
        io_lib:format(
            "openssl x509 -req -in ~ts -CA ~ts -CAkey ~ts "
            "-CAcreateserial -out ~ts -days 365 2>/dev/null",
            [LeafCsrFile, CaCertFile, CaKeyFile, LeafCertFile]
        )
    ),
    _ = os:cmd(SignCmd),
    %% Verify files exist.
    true = filelib:is_regular(CaCertFile),
    true = filelib:is_regular(LeafCertFile),
    true = filelib:is_regular(LeafKeyFile),
    {CaCertFile, LeafCertFile, LeafKeyFile}.

read_file_bin(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

%%====================================================================
%% Internal -- HTTP listener
%%====================================================================

%% Acceptor loop: for each incoming TCP connection, increment the ETS counter
%% and spawn a handler that speaks just enough HTTP/1.1 to satisfy gun.
acceptor_loop(ListenSock, Tab) ->
    case gen_tcp:accept(ListenSock, 5000) of
        {ok, Sock} ->
            ets:update_counter(Tab, count, 1),
            spawn(fun() -> http_handler_loop(Sock) end),
            acceptor_loop(ListenSock, Tab);
        {error, timeout} ->
            acceptor_loop(ListenSock, Tab);
        {error, closed} ->
            ok;
        {error, _Reason} ->
            ok
    end.

%% Minimal HTTP/1.1 handler. Reads requests and responds based on the path
%% and headers. Supports keep-alive so gun can reuse the connection.
http_handler_loop(Sock) ->
    inet:setopts(Sock, [{active, false}, {packet, http_bin}]),
    case read_http_request(Sock) of
        {ok, Method, Path, Headers} ->
            Body = maybe_read_body(Sock, Headers),
            Response = build_response(Method, Path, Headers, Body),
            gen_tcp:send(Sock, Response),
            %% Keep the connection open for HTTP keep-alive.
            http_handler_loop(Sock);
        {error, closed} ->
            gen_tcp:close(Sock);
        {error, _} ->
            gen_tcp:close(Sock)
    end.

read_http_request(Sock) ->
    case gen_tcp:recv(Sock, 0, 30000) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            Headers = read_headers(Sock, []),
            {ok, Method, Path, Headers};
        {ok, {http_request, Method, {absoluteURI, _Scheme, _Host, _Port, Path}, _Version}} ->
            Headers = read_headers(Sock, []),
            {ok, Method, Path, Headers};
        {error, Reason} ->
            {error, Reason}
    end.

read_headers(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 10000) of
        {ok, {http_header, _, Name, _, Value}} ->
            read_headers(Sock, [{header_name(Name), Value} | Acc]);
        {ok, http_eoh} ->
            Acc;
        {error, _} ->
            Acc
    end.

header_name(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
header_name(Name) when is_binary(Name) -> Name.

maybe_read_body(Sock, Headers) ->
    case content_length(Headers) of
        0 ->
            <<>>;
        Len when Len > 0 ->
            inet:setopts(Sock, [{packet, raw}]),
            {ok, Body} = gen_tcp:recv(Sock, Len, 10000),
            inet:setopts(Sock, [{packet, http_bin}]),
            Body
    end.

content_length(Headers) ->
    case lists:keyfind(<<"Content-Length">>, 1, Headers) of
        {_, Val} -> binary_to_integer(Val);
        false -> 0
    end.

%% Build an HTTP response based on the request. S3 GET returns PEM data
%% (determined by the S3 path). SecretsManager POST returns JSON with
%% SecretString.
build_response(_Method, Path, Headers, _Body) ->
    case is_secretsmanager_request(Headers) of
        true ->
            json_response();
        false ->
            s3_response(Path)
    end.

%% Determine which PEM to serve based on the S3 object path.
%% The path looks like /test-bucket/cacert.pem or /test-bucket/key.pem.
s3_response(Path) ->
    PathStr = binary_to_list(Path),
    PemData =
        case
            {
                string:find(PathStr, "cacert"),
                string:find(PathStr, "key"),
                string:find(PathStr, "cert")
            }
        of
            {S, _, _} when S =/= nomatch -> lookup_pem(cacert_pem);
            {_, S, _} when S =/= nomatch -> lookup_pem(key_pem);
            {_, _, S} when S =/= nomatch -> lookup_pem(cert_pem);
            _ -> lookup_pem(cacert_pem)
        end,
    make_http_response(200, <<"application/octet-stream">>, PemData).

json_response() ->
    %% The SecretString must be a valid PEM private key since the handler
    %% (aws_arn_config_rabbit) passes it through aws_pem_util:decode_key_data/1.
    KeyPem = lookup_pem(key_pem),
    EscapedKey = json_escape_binary(KeyPem),
    Body = iolist_to_binary([
        <<"{\"SecretString\":\"">>,
        EscapedKey,
        <<"\",\"Name\":\"test\",\"VersionId\":\"1\",\"VersionStages\":[\"AWSCURRENT\"]}">>
    ]),
    make_http_response(200, <<"application/x-amz-json-1.1">>, Body).

make_http_response(Status, ContentType, Body) ->
    StatusLine = status_line(Status),
    ContentLength = integer_to_list(byte_size(Body)),
    iolist_to_binary([
        <<"HTTP/1.1 ">>,
        StatusLine,
        <<"\r\n">>,
        <<"Content-Type: ">>,
        ContentType,
        <<"\r\n">>,
        <<"Content-Length: ">>,
        list_to_binary(ContentLength),
        <<"\r\n">>,
        <<"Connection: keep-alive\r\n">>,
        <<"\r\n">>,
        Body
    ]).

status_line(200) -> <<"200 OK">>;
status_line(404) -> <<"404 Not Found">>;
status_line(500) -> <<"500 Internal Server Error">>.

lookup_pem(Key) ->
    case ets:lookup(pem_data, Key) of
        [{_, Pem}] -> Pem;
        [] -> <<"no-pem-data">>
    end.

is_secretsmanager_request(Headers) ->
    case lists:keyfind(<<"X-Amz-Target">>, 1, Headers) of
        {_, Target} ->
            binary:match(Target, <<"secretsmanager">>) =/= nomatch;
        false ->
            false
    end.

%% Escape a binary for inclusion in a JSON string value.
%% Handles newlines, carriage returns, tabs, backslashes, and double quotes.
json_escape_binary(Bin) ->
    json_escape_binary(Bin, <<>>).

json_escape_binary(<<>>, Acc) ->
    Acc;
json_escape_binary(<<$\n, Rest/binary>>, Acc) ->
    json_escape_binary(Rest, <<Acc/binary, $\\, $n>>);
json_escape_binary(<<$\r, Rest/binary>>, Acc) ->
    json_escape_binary(Rest, <<Acc/binary, $\\, $r>>);
json_escape_binary(<<$\t, Rest/binary>>, Acc) ->
    json_escape_binary(Rest, <<Acc/binary, $\\, $t>>);
json_escape_binary(<<$\\, Rest/binary>>, Acc) ->
    json_escape_binary(Rest, <<Acc/binary, $\\, $\\>>);
json_escape_binary(<<$", Rest/binary>>, Acc) ->
    json_escape_binary(Rest, <<Acc/binary, $\\, $">>);
json_escape_binary(<<C, Rest/binary>>, Acc) ->
    json_escape_binary(Rest, <<Acc/binary, C>>).
