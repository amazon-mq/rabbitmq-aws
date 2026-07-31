%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0

%% Unit tests for ECS/EKS container credentials support in aws_lib_config.
%% These test URL construction, SSRF allowlist validation, token handling,
%% and the hard-error vs fallthrough semantics.
-module(aws_lib_config_container_creds_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aws_lib.hrl").

%% ---------------------------------------------------------------------------
%% Test setup / teardown helpers
%% ---------------------------------------------------------------------------

clear_container_env() ->
    os:unsetenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
    os:unsetenv("AWS_CONTAINER_CREDENTIALS_FULL_URI"),
    os:unsetenv("AWS_CONTAINER_AUTHORIZATION_TOKEN"),
    os:unsetenv("AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE"),
    application:unset_env(aws, container_credentials_host_override).

%% ---------------------------------------------------------------------------
%% container_credentials_url/0 tests
%% ---------------------------------------------------------------------------

container_url_test_() ->
    {foreach, fun() -> clear_container_env() end, fun(_) -> clear_container_env() end, [
        {"not_configured when no env vars set", fun() ->
            ?assertEqual(not_configured, aws_lib_config:container_credentials_url())
        end},
        {"RELATIVE_URI builds URL with ECS host", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
                "/v2/credentials/abc-123"
            ),
            ?assertEqual(
                {ok, "http://169.254.170.2/v2/credentials/abc-123"},
                aws_lib_config:container_credentials_url()
            )
        end},
        {"RELATIVE_URI with host override uses override", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
                "/v2/credentials/abc-123"
            ),
            application:set_env(aws, container_credentials_host_override, "127.0.0.1:9911"),
            ?assertEqual(
                {ok, "http://127.0.0.1:9911/v2/credentials/abc-123"},
                aws_lib_config:container_credentials_url()
            )
        end},
        {"RELATIVE_URI takes precedence over FULL_URI", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
                "/v2/credentials/relative"
            ),
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_FULL_URI",
                "http://127.0.0.1:1234/full"
            ),
            {ok, URL} = aws_lib_config:container_credentials_url(),
            ?assertMatch("http://169.254.170.2/v2/credentials/relative", URL)
        end},
        {"FULL_URI with http loopback is allowed", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_FULL_URI",
                "http://127.0.0.1:1234/creds"
            ),
            ?assertEqual(
                {ok, "http://127.0.0.1:1234/creds"},
                aws_lib_config:container_credentials_url()
            )
        end},
        {"FULL_URI with https any host is allowed", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_FULL_URI",
                "https://sts.amazonaws.com/creds"
            ),
            ?assertEqual(
                {ok, "https://sts.amazonaws.com/creds"},
                aws_lib_config:container_credentials_url()
            )
        end},
        {"FULL_URI with http public IP is rejected", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_FULL_URI",
                "http://203.0.113.1/creds"
            ),
            ?assertMatch(
                {error, uri_not_allowed, _},
                aws_lib_config:container_credentials_url()
            )
        end},
        {"FULL_URI with http RFC1918 IP is rejected", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_FULL_URI",
                "http://10.0.0.1/creds"
            ),
            ?assertMatch(
                {error, uri_not_allowed, _},
                aws_lib_config:container_credentials_url()
            )
        end},
        {"FULL_URI with unsupported scheme is rejected", fun() ->
            os:putenv(
                "AWS_CONTAINER_CREDENTIALS_FULL_URI",
                "ftp://127.0.0.1/creds"
            ),
            ?assertMatch(
                {error, uri_not_allowed, {unsupported_scheme, "ftp"}},
                aws_lib_config:container_credentials_url()
            )
        end}
    ]}.

%% ---------------------------------------------------------------------------
%% validate_full_uri/1 SSRF allowlist tests
%% ---------------------------------------------------------------------------

validate_full_uri_test_() ->
    [
        {"loopback 127.0.0.1 allowed for http", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("http://127.0.0.1/path"))
        end},
        {"loopback 127.0.0.255 allowed for http", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("http://127.0.0.255/path"))
        end},
        {"ECS link-local 169.254.170.2 allowed for http", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("http://169.254.170.2/path"))
        end},
        {"EKS link-local 169.254.170.23 allowed for http", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("http://169.254.170.23/path"))
        end},
        {"IPv6 loopback ::1 allowed for http", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("http://[::1]/path"))
        end},
        {"IPv6 EKS fd00:ec2::23 allowed for http", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("http://[fd00:ec2::23]/path"))
        end},
        {"https allows any host", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("https://203.0.113.1/path"))
        end},
        {"https allows public hostname", fun() ->
            ?assertEqual(ok, aws_lib_config:validate_full_uri("https://sts.amazonaws.com/path"))
        end},
        {"http to IMDS 169.254.169.254 rejected", fun() ->
            ?assertMatch(
                {error, {full_uri_not_allowed, _}},
                aws_lib_config:validate_full_uri("http://169.254.169.254/path")
            )
        end},
        {"http to public IP rejected", fun() ->
            ?assertMatch(
                {error, {full_uri_not_allowed, _}},
                aws_lib_config:validate_full_uri("http://8.8.8.8/path")
            )
        end},
        {"http to RFC1918 rejected", fun() ->
            ?assertMatch(
                {error, {full_uri_not_allowed, _}},
                aws_lib_config:validate_full_uri("http://192.168.1.1/path")
            )
        end},
        {"http to hostname rejected (not a literal IP)", fun() ->
            ?assertMatch(
                {error, {full_uri_not_allowed, _}},
                aws_lib_config:validate_full_uri("http://my-host.local/path")
            )
        end},
        {"malformed URI rejected", fun() ->
            ?assertMatch(
                {error, {malformed_full_uri, _}},
                aws_lib_config:validate_full_uri("not-a-uri")
            )
        end}
    ].

%% ---------------------------------------------------------------------------
%% container_auth_token/0 tests
%% ---------------------------------------------------------------------------

container_auth_token_test_() ->
    {foreach, fun() -> clear_container_env() end, fun(_) -> clear_container_env() end, [
        {"undefined when no token env vars set", fun() ->
            ?assertEqual(undefined, aws_lib_config:container_auth_token())
        end},
        {"reads token from AWS_CONTAINER_AUTHORIZATION_TOKEN", fun() ->
            os:putenv("AWS_CONTAINER_AUTHORIZATION_TOKEN", "Bearer my-secret-token"),
            ?assertEqual("Bearer my-secret-token", aws_lib_config:container_auth_token())
        end},
        {"reads token from file (TOKEN_FILE takes precedence)", fun() ->
            %% Write a temp file
            TmpDir = filename:basedir(user_cache, "aws_test"),
            ok = filelib:ensure_dir(filename:join(TmpDir, "x")),
            TokenPath = filename:join(TmpDir, "test_token"),
            ok = file:write_file(TokenPath, <<"file-token-value\n">>),
            os:putenv("AWS_CONTAINER_AUTHORIZATION_TOKEN", "env-token"),
            os:putenv("AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE", TokenPath),
            ?assertEqual("file-token-value", aws_lib_config:container_auth_token()),
            file:delete(TokenPath)
        end},
        {"missing token file raises error", fun() ->
            os:putenv(
                "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE",
                "/nonexistent/path/token"
            ),
            ?assertError(
                {container_token_file_not_found, _},
                aws_lib_config:container_auth_token()
            )
        end},
        {"oversized token file raises error", fun() ->
            TmpDir = filename:basedir(user_cache, "aws_test"),
            ok = filelib:ensure_dir(filename:join(TmpDir, "x")),
            BigPath = filename:join(TmpDir, "big_token"),
            %% Write a file larger than 8KB
            BigContent = list_to_binary(lists:duplicate(?CONTAINER_AUTH_TOKEN_MAX_SIZE + 1, $A)),
            ok = file:write_file(BigPath, BigContent),
            os:putenv("AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE", BigPath),
            ?assertError(
                {container_token_file_too_large, _},
                aws_lib_config:container_auth_token()
            ),
            file:delete(BigPath)
        end}
    ]}.

%% ---------------------------------------------------------------------------
%% is_allowed_plaintext_host/1 tests
%% ---------------------------------------------------------------------------

is_allowed_plaintext_host_test_() ->
    [
        {"127.0.0.1 is loopback - allowed", fun() ->
            ?assert(aws_lib_config:is_allowed_plaintext_host("127.0.0.1"))
        end},
        {"127.255.255.254 is loopback - allowed", fun() ->
            ?assert(aws_lib_config:is_allowed_plaintext_host("127.255.255.254"))
        end},
        {"169.254.170.2 is ECS link-local - allowed", fun() ->
            ?assert(aws_lib_config:is_allowed_plaintext_host("169.254.170.2"))
        end},
        {"169.254.170.23 is EKS link-local - allowed", fun() ->
            ?assert(aws_lib_config:is_allowed_plaintext_host("169.254.170.23"))
        end},
        {"::1 is IPv6 loopback - allowed", fun() ->
            ?assert(aws_lib_config:is_allowed_plaintext_host("::1"))
        end},
        {"fd00:ec2::23 is EKS IPv6 - allowed", fun() ->
            ?assert(aws_lib_config:is_allowed_plaintext_host("fd00:ec2::23"))
        end},
        {"10.0.0.1 is RFC1918 - rejected", fun() ->
            ?assertNot(aws_lib_config:is_allowed_plaintext_host("10.0.0.1"))
        end},
        {"192.168.1.1 is RFC1918 - rejected", fun() ->
            ?assertNot(aws_lib_config:is_allowed_plaintext_host("192.168.1.1"))
        end},
        {"169.254.169.254 is IMDS - rejected", fun() ->
            ?assertNot(aws_lib_config:is_allowed_plaintext_host("169.254.169.254"))
        end},
        {"8.8.8.8 is public - rejected", fun() ->
            ?assertNot(aws_lib_config:is_allowed_plaintext_host("8.8.8.8"))
        end},
        {"hostname is rejected (not a literal IP)", fun() ->
            ?assertNot(aws_lib_config:is_allowed_plaintext_host("localhost"))
        end}
    ].

%% ---------------------------------------------------------------------------
%% fetch_container_credentials/2 integration test (with mocked gun)
%% ---------------------------------------------------------------------------

fetch_container_credentials_test_() ->
    {foreach,
        fun() ->
            meck:new(gun, []),
            [gun]
        end,
        fun meck:unload/1, [
            {"successful credential fetch", fun() ->
                CredsBody = <<
                    "{\"AccessKeyId\":\"AKID123\","
                    "\"SecretAccessKey\":\"secret456\","
                    "\"Token\":\"sessiontoken789\","
                    "\"Expiration\":\"2026-08-01T12:00:00Z\"}"
                >>,
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) ->
                    {response, nofin, 200, []}
                end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, CredsBody} end),
                {ok, Creds} = aws_lib_config:fetch_container_credentials(
                    "http://169.254.170.2/v2/creds/abc", undefined
                ),
                ?assertEqual("AKID123", Creds#aws_credentials.access_key),
                ?assertEqual("secret456", Creds#aws_credentials.secret_key),
                ?assertEqual("sessiontoken789", Creds#aws_credentials.security_token),
                ?assertEqual({{2026, 8, 1}, {12, 0, 0}}, Creds#aws_credentials.expiration)
            end},
            {"credential fetch with authorization token", fun() ->
                CredsBody = <<
                    "{\"AccessKeyId\":\"AKID\","
                    "\"SecretAccessKey\":\"secret\","
                    "\"Token\":\"tok\","
                    "\"Expiration\":\"2026-01-01T00:00:00Z\"}"
                >>,
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, Path, Headers) ->
                    %% Path is a list string (gun receives it from aws_lib_httpc)
                    ?assertEqual("/v2/creds/xyz", Path),
                    %% Headers are normalized to {binary(), binary()} tuples
                    AuthHdr = proplists:get_value(<<"Authorization">>, Headers),
                    ?assertEqual(<<"Bearer my-token">>, AuthHdr),
                    stream_ref
                end),
                meck:expect(gun, await, fun(_, _, _) ->
                    {response, nofin, 200, []}
                end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, CredsBody} end),
                {ok, Creds} = aws_lib_config:fetch_container_credentials(
                    "http://169.254.170.2/v2/creds/xyz", "Bearer my-token"
                ),
                ?assertEqual("AKID", Creds#aws_credentials.access_key)
            end},
            {"HTTP error response returns error tuple", fun() ->
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) ->
                    {response, nofin, 500, []}
                end),
                meck:expect(gun, await_body, fun(_, _, _) ->
                    {ok, <<"Internal Server Error">>}
                end),
                ?assertMatch(
                    {error, {http_error, 500, _}},
                    aws_lib_config:fetch_container_credentials(
                        "http://169.254.170.2/v2/creds/abc", undefined
                    )
                )
            end},
            {"transport error returns error tuple", fun() ->
                meck:expect(gun, open, fun(_, _, _) -> {error, timeout} end),
                ?assertMatch(
                    {error, {transport_error, _}},
                    aws_lib_config:fetch_container_credentials(
                        "http://169.254.170.2/v2/creds/abc", undefined
                    )
                )
            end},
            {"response body too large returns error", fun() ->
                %% Build a body larger than CONTAINER_CREDS_MAX_BODY
                BigBody = list_to_binary(lists:duplicate(?CONTAINER_CREDS_MAX_BODY + 1, $x)),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) ->
                    {response, nofin, 200, []}
                end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, BigBody} end),
                ?assertEqual(
                    {error, response_body_too_large},
                    aws_lib_config:fetch_container_credentials(
                        "http://169.254.170.2/v2/creds/abc", undefined
                    )
                )
            end},
            {"missing credential fields in JSON returns error", fun() ->
                %% Missing SecretAccessKey
                Body = <<"{\"AccessKeyId\":\"AKID\"}">>,
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) ->
                    {response, nofin, 200, []}
                end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, Body} end),
                ?assertEqual(
                    {error, missing_credential_fields},
                    aws_lib_config:fetch_container_credentials(
                        "http://169.254.170.2/v2/creds/abc", undefined
                    )
                )
            end}
        ]}.

%% ---------------------------------------------------------------------------
%% lookup_credentials_from_container_or_imds/1 integration tests
%% ---------------------------------------------------------------------------

container_or_imds_test_() ->
    {foreach,
        fun() ->
            meck:new(gun, []),
            meck:new(aws_lib, [passthrough]),
            clear_container_env(),
            %% Disable IMDSv2 for simplicity in these tests
            application:set_env(aws, aws_prefer_imdsv2, false),
            [gun, aws_lib]
        end,
        fun(Mods) ->
            clear_container_env(),
            application:unset_env(aws, aws_prefer_imdsv2),
            meck:unload(Mods)
        end,
        [
            {"falls through to IMDS when no container env vars set", fun() ->
                %% Mock IMDS to return 404 so we get {error, undefined}
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, fin, 404, []} end),
                S = #aws_config{},
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:lookup_credentials_from_container_or_imds(S)
                )
            end},
            {"uses container endpoint when RELATIVE_URI is set", fun() ->
                os:putenv(
                    "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
                    "/v2/credentials/task-id"
                ),
                CredsBody = <<
                    "{\"AccessKeyId\":\"CONTAINER_KEY\","
                    "\"SecretAccessKey\":\"CONTAINER_SECRET\","
                    "\"Token\":\"CONTAINER_TOKEN\","
                    "\"Expiration\":\"2026-12-31T23:59:59Z\"}"
                >>,
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) ->
                    {response, nofin, 200, []}
                end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, CredsBody} end),
                S = #aws_config{},
                {ok, Creds, S1} =
                    aws_lib_config:lookup_credentials_from_container_or_imds(S),
                ?assertEqual("CONTAINER_KEY", Creds#aws_credentials.access_key),
                ?assertEqual("CONTAINER_SECRET", Creds#aws_credentials.secret_key),
                ?assertEqual("CONTAINER_TOKEN", Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"hard error when container env set but fetch fails (no IMDS fallthrough)", fun() ->
                os:putenv(
                    "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
                    "/v2/credentials/task-id"
                ),
                %% Simulate transport failure
                meck:expect(gun, open, fun(_, _, _) -> {error, econnrefused} end),
                S = #aws_config{},
                Result = aws_lib_config:lookup_credentials_from_container_or_imds(S),
                ?assertMatch(
                    {error, {container_credentials_failed, _}},
                    Result
                )
            end}
        ]}.
