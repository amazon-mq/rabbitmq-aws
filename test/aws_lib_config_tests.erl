-module(aws_lib_config_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper function to mock gun for IMDSv2 failure scenarios
mock_gun_imdsv2_failure() ->
    meck:expect(gun, open, fun(_, _, _) -> {ok, fake_conn} end),
    meck:expect(gun, await_up, fun(_, _) -> {ok, http} end),
    meck:expect(gun, put, fun(_, _, _, _, _) -> fake_stream end),
    meck:expect(gun, get, fun(_, _, _) -> fake_stream end),
    meck:expect(gun, await, fun(_, _, _) -> {response, fin, 404, []} end),
    meck:expect(gun, close, fun(_) -> ok end).

-include("aws_lib.hrl").

%% endpoint_url/1 resolves the local-development endpoint override: the
%% per-service AWS_ENDPOINT_URL_<SERVICE> variable takes precedence over the
%% global AWS_ENDPOINT_URL, and the service name maps to the suffix by
%% upper-casing and turning hyphens into underscores. The foreach clears both
%% variables before and after each case so they never leak between tests.
endpoint_url_test_() ->
    Clear = fun() ->
        os:unsetenv("AWS_ENDPOINT_URL"),
        os:unsetenv("AWS_ENDPOINT_URL_S3"),
        os:unsetenv("AWS_ENDPOINT_URL_SECRETSMANAGER"),
        os:unsetenv("AWS_ENDPOINT_URL_ACM_PCA")
    end,
    {foreach,
        fun() ->
            Clear(),
            ok
        end,
        fun(_) ->
            Clear(),
            ok
        end,
        [
            {"undefined when no override is set", fun() ->
                ?assertEqual(undefined, aws_lib_config:endpoint_url("s3"))
            end},
            {"the global override applies to any service", fun() ->
                os:putenv("AWS_ENDPOINT_URL", "http://localhost:4566"),
                ?assertEqual("http://localhost:4566", aws_lib_config:endpoint_url("s3")),
                ?assertEqual(
                    "http://localhost:4566", aws_lib_config:endpoint_url("secretsmanager")
                )
            end},
            {"a per-service override wins over the global one", fun() ->
                os:putenv("AWS_ENDPOINT_URL", "http://localhost:4566"),
                os:putenv("AWS_ENDPOINT_URL_S3", "http://localhost:9000"),
                ?assertEqual("http://localhost:9000", aws_lib_config:endpoint_url("s3")),
                %% A service without its own override still falls back to the global.
                ?assertEqual(
                    "http://localhost:4566", aws_lib_config:endpoint_url("secretsmanager")
                )
            end},
            {"the service name maps hyphens to underscores", fun() ->
                os:putenv("AWS_ENDPOINT_URL_ACM_PCA", "http://localhost:4567"),
                ?assertEqual("http://localhost:4567", aws_lib_config:endpoint_url("acm-pca"))
            end},
            {"an empty override value is treated as unset", fun() ->
                os:putenv("AWS_ENDPOINT_URL", ""),
                ?assertEqual(undefined, aws_lib_config:endpoint_url("s3"))
            end}
        ]}.

config_file_test_() ->
    [
        {"from environment variable", fun() ->
            os:putenv("AWS_CONFIG_FILE", "/etc/aws/config"),
            ?assertEqual("/etc/aws/config", aws_lib_config:config_file())
        end},
        {"default without environment variable", fun() ->
            os:unsetenv("AWS_CONFIG_FILE"),
            os:putenv("HOME", "/home/rrabbit"),
            ?assertEqual(
                "/home/rrabbit/.aws/config",
                aws_lib_config:config_file()
            )
        end}
    ].

config_file_data_test_() ->
    [
        {"successfully parses ini", fun() ->
            setup_test_config_env_var(),
            Expectation = [
                {"default", [
                    {aws_access_key_id, "default-key"},
                    {aws_secret_access_key, "default-access-key"},
                    {region, "us-east-4"}
                ]},
                {"profile testing", [
                    {aws_access_key_id, "foo1"},
                    {aws_secret_access_key, "bar2"},
                    {s3, [
                        {max_concurrent_requests, 10},
                        {max_queue_size, 1000}
                    ]},
                    {region, "us-west-5"}
                ]},
                {"profile no-region", [
                    {aws_access_key_id, "foo2"},
                    {aws_secret_access_key, "bar3"}
                ]},
                {"profile only-key", [{aws_access_key_id, "foo3"}]},
                {"profile only-secret", [{aws_secret_access_key, "foo4"}]},
                {"profile bad-entry", [{aws_secret_access, "foo5"}]},
                {"profile chained", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {source_profile, "default"}
                ]},
                {"profile chained-with-external-id", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {source_profile, "default"},
                    {external_id, "test-external-id-12345"}
                ]},
                {"profile deep-chain", [
                    {role_arn, "arn:aws:iam::123456789012:role/DeepRole"},
                    {source_profile, "intermediate"}
                ]},
                {"profile intermediate", [
                    {role_arn, "arn:aws:iam::123456789012:role/IntermediateRole"},
                    {source_profile, "default"}
                ]},
                {"profile cycle-a", [
                    {role_arn, "arn:aws:iam::123456789012:role/CycleA"},
                    {source_profile, "cycle-b"}
                ]},
                {"profile cycle-b", [
                    {role_arn, "arn:aws:iam::123456789012:role/CycleB"},
                    {source_profile, "cycle-a"}
                ]},
                {"profile credential-source-test", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {credential_source, "Ec2InstanceMetadata"}
                ]},
                {"profile invalid-session-name", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {source_profile, "default"},
                    {role_session_name, "invalid name with spaces!!!"}
                ]},
                {"profile missing-source", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {source_profile, "nonexistent_profile"}
                ]},
                {"profile role-no-source", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {aws_access_key_id, "AKIAIOSFODNN7EXAMPLE"},
                    {aws_secret_access_key, "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"}
                ]},
                {"profile custom-session-name", [
                    {role_arn, "arn:aws:iam::123456789012:role/TestRole"},
                    {source_profile, "default"},
                    {role_session_name, "my-custom-session"}
                ]}
            ],
            ?assertEqual(
                Expectation,
                aws_lib_config:config_file_data()
            )
        end},
        {"file does not exist", fun() ->
            ?assertEqual(
                {error, enoent},
                aws_lib_config:ini_file_data(
                    filename:join([filename:absname("."), "bad_path"]), false
                )
            )
        end},
        {"file exists but path is invalid", fun() ->
            ?assertEqual(
                {error, enoent},
                aws_lib_config:ini_file_data(
                    filename:join([filename:absname("."), "bad_path"]), true
                )
            )
        end}
    ].

instance_metadata_test_() ->
    [
        {"instance role URL", fun() ->
            ?assertEqual(
                "http://169.254.169.254/latest/meta-data/iam/security-credentials",
                aws_lib_config:instance_role_url()
            )
        end},
        {"availability zone URL", fun() ->
            ?assertEqual(
                "http://169.254.169.254/latest/meta-data/placement/availability-zone",
                aws_lib_config:instance_availability_zone_url()
            )
        end},
        {"instance id URL", fun() ->
            ?assertEqual(
                "http://169.254.169.254/latest/meta-data/instance-id",
                aws_lib_config:instance_id_url()
            )
        end},
        {"arbitrary paths", fun() ->
            ?assertEqual(
                "http://169.254.169.254/a/b/c", aws_lib_config:instance_metadata_url("a/b/c")
            ),
            ?assertEqual(
                "http://169.254.169.254/a/b/c", aws_lib_config:instance_metadata_url("/a/b/c")
            )
        end}
    ].

credentials_file_test_() ->
    [
        {"from environment variable", fun() ->
            os:putenv("AWS_SHARED_CREDENTIALS_FILE", "/etc/aws/credentials"),
            ?assertEqual("/etc/aws/credentials", aws_lib_config:credentials_file())
        end},
        {"default without environment variable", fun() ->
            os:unsetenv("AWS_SHARED_CREDENTIALS_FILE"),
            os:putenv("HOME", "/home/rrabbit"),
            ?assertEqual(
                "/home/rrabbit/.aws/credentials",
                aws_lib_config:credentials_file()
            )
        end}
    ].

credentials_test_() ->
    {
        foreach,
        fun() ->
            meck:new(gun, []),
            meck:new(aws_lib, [passthrough]),
            reset_environment(),
            application:set_env(aws, aws_prefer_imdsv2, false),
            [gun, aws_lib]
        end,
        fun(Mods) ->
            application:unset_env(aws, aws_prefer_imdsv2),
            meck:unload(Mods)
        end,
        [
            {"from environment variables", fun() ->
                os:putenv("AWS_ACCESS_KEY_ID", "Sésame"),
                os:putenv("AWS_SECRET_ACCESS_KEY", "ouvre-toi"),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials(S),
                ?assertEqual("Sésame", Creds#aws_credentials.access_key),
                ?assertEqual("ouvre-toi", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                ?assertEqual(undefined, Creds#aws_credentials.expiration),
                ?assertEqual(S, S1)
            end},
            {"from environment variables with session token", fun() ->
                os:putenv("AWS_ACCESS_KEY_ID", "Sésame"),
                os:putenv("AWS_SECRET_ACCESS_KEY", "ouvre-toi"),
                os:putenv("AWS_SESSION_TOKEN", "session42"),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials(S),
                ?assertEqual("Sésame", Creds#aws_credentials.access_key),
                ?assertEqual("ouvre-toi", Creds#aws_credentials.secret_key),
                ?assertEqual("session42", Creds#aws_credentials.security_token),
                ?assertEqual(undefined, Creds#aws_credentials.expiration),
                ?assertEqual(S, S1)
            end},
            {"from config file with default profile", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials(S),
                ?assertEqual("default-key", Creds#aws_credentials.access_key),
                ?assertEqual("default-access-key", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"with missing environment variable", fun() ->
                os:putenv("AWS_ACCESS_KEY_ID", "Sésame"),
                S = #aws_config{},
                meck:sequence(aws_lib, ensure_imdsv2_token_valid, 1, [
                    {ok, "secret_imdsv2_token", S}
                ]),
                mock_gun_imdsv2_failure(),
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:credentials(S)
                )
            end},
            {"from config file with default profile", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials(S),
                ?assertEqual("default-key", Creds#aws_credentials.access_key),
                ?assertEqual("default-access-key", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"from config file with profile", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials("testing", S),
                ?assertEqual("foo1", Creds#aws_credentials.access_key),
                ?assertEqual("bar2", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"from config file with bad profile", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                mock_gun_imdsv2_failure(),
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:credentials("bad-profile-name", S)
                )
            end},
            {"from credentials file with default profile", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials(S),
                ?assertEqual("foo1", Creds#aws_credentials.access_key),
                ?assertEqual("bar1", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"from credentials file with profile", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials("development", S),
                ?assertEqual("foo2", Creds#aws_credentials.access_key),
                ?assertEqual("bar2", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"from credentials file with session token", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                {ok, Creds, S1} = aws_lib_config:credentials("with-session-token", S),
                ?assertEqual("foo3", Creds#aws_credentials.access_key),
                ?assertEqual("bar3", Creds#aws_credentials.secret_key),
                ?assertEqual("session42", Creds#aws_credentials.security_token),
                ?assertEqual(S, S1)
            end},
            {"from credentials file with bad profile", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                mock_gun_imdsv2_failure(),
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:credentials("bad-profile-name", S)
                )
            end},
            {"from credentials file with only the key in profile", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                mock_gun_imdsv2_failure(),
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:credentials("only-key", S)
                )
            end},
            {"from credentials file with only the value in profile", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                mock_gun_imdsv2_failure(),
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:credentials("only-value", S)
                )
            end},
            {"from credentials file with missing keys in profile", fun() ->
                setup_test_credentials_env_var(),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                mock_gun_imdsv2_failure(),
                ?assertEqual(
                    {error, undefined},
                    aws_lib_config:credentials("bad-entry", S)
                )
            end},
            {"from instance metadata service", fun() ->
                CredsBody =
                    "{\n  \"Code\" : \"Success\",\n  \"LastUpdated\" : \"2016-03-31T21:51:49Z\",\n  \"Type\" : \"AWS-HMAC\",\n  \"AccessKeyId\" : \"ASIAIMAFAKEACCESSKEY\",\n  \"SecretAccessKey\" : \"2+t64tZZVaz0yp0x1G23ZRYn+FAKEyVALUEs/4qh\",\n  \"Token\" : \"FAKE//////////wEAK/TOKEN/VALUE=\",\n  \"Expiration\" : \"2016-04-01T04:13:28Z\"\n}",
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:sequence(gun, get, 3, [stream_ref1, stream_ref2]),
                meck:sequence(
                    gun,
                    await,
                    3,
                    [
                        {response, nofin, 200, headers},
                        {response, nofin, 200, headers}
                    ]
                ),
                meck:sequence(
                    gun,
                    await_body,
                    3,
                    [
                        {ok, <<"Bob">>},
                        {ok, list_to_binary(CredsBody)}
                    ]
                ),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                {ok, Creds, S1} = aws_lib_config:credentials(S),
                ?assertEqual("ASIAIMAFAKEACCESSKEY", Creds#aws_credentials.access_key),
                ?assertEqual(
                    "2+t64tZZVaz0yp0x1G23ZRYn+FAKEyVALUEs/4qh", Creds#aws_credentials.secret_key
                ),
                ?assertEqual(
                    "FAKE//////////wEAK/TOKEN/VALUE=", Creds#aws_credentials.security_token
                ),
                ?assertEqual({{2016, 4, 1}, {4, 13, 28}}, Creds#aws_credentials.expiration),
                ?assertEqual(S, S1)
            end},
            {"with instance metadata service role error", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                mock_gun_imdsv2_failure(),
                ?assertEqual({error, undefined}, aws_lib_config:credentials(S))
            end},
            {"with instance metadata service role http error", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 500, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"Internal Server Error">>} end),
                ?assertEqual({error, undefined}, aws_lib_config:credentials(S))
            end},
            {"with instance metadata service credentials error", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:sequence(gun, get, 3, [stream_ref1, stream_ref2]),
                meck:sequence(
                    gun,
                    await,
                    3,
                    [
                        {response, nofin, 200, headers},
                        {error, timeout}
                    ]
                ),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"Bob">>} end),
                ?assertEqual({error, timeout}, aws_lib_config:credentials(S))
            end},
            {"with instance metadata service credentials not found", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:sequence(gun, get, 3, [stream_ref1, stream_ref2]),
                meck:sequence(
                    gun,
                    await,
                    3,
                    [
                        {response, nofin, 200, headers},
                        {response, nofin, 404, headers}
                    ]
                ),
                meck:sequence(
                    gun,
                    await_body,
                    3,
                    [
                        {ok, <<"Bob">>},
                        {ok, <<"File Not Found">>}
                    ]
                ),
                ?assertEqual({error, undefined}, aws_lib_config:credentials(S))
            end},
            {"with instance metadata body read timeout", fun() ->
                %% Headers arrive (200) but the body read times out. This must
                %% surface as {error, timeout}, not crash with a badmatch.
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 200, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {error, timeout} end),
                ?assertEqual({error, timeout}, aws_lib_config:credentials(S))
            end}
        ]
    }.

home_path_test_() ->
    [
        {"with HOME", fun() ->
            os:putenv("HOME", "/home/rrabbit"),
            ?assertEqual(
                "/home/rrabbit",
                aws_lib_config:home_path()
            )
        end},
        {"without HOME", fun() ->
            os:unsetenv("HOME"),
            ?assertEqual(
                filename:absname("."),
                aws_lib_config:home_path()
            )
        end}
    ].

ini_format_key_test_() ->
    [
        {"when value is list", fun() ->
            ?assertEqual(test_key, aws_lib_config:ini_format_key("test_key"))
        end},
        {"when value is binary", fun() ->
            ?assertEqual({error, type}, aws_lib_config:ini_format_key(<<"test_key">>))
        end}
    ].

ini_split_line_test_() ->
    [
        {"simple key=value", fun() ->
            ?assertEqual(
                ["region", "us-east-1"], aws_lib_config:ini_split_line(<<"region=us-east-1">>)
            )
        end},
        {"key = value with surrounding spaces stripped at the ends", fun() ->
            ?assertEqual(
                ["aws_access_key_id ", " foo1"],
                aws_lib_config:ini_split_line(<<"aws_access_key_id = foo1">>)
            )
        end},
        {"section/parent line with empty value collapses to [Key]", fun() ->
            ?assertEqual(["s3 "], aws_lib_config:ini_split_line(<<"s3 =">>))
        end},
        {"line with no '=' is a single token", fun() ->
            ?assertEqual(
                ["aws_secret_access"], aws_lib_config:ini_split_line(<<"aws_secret_access">>)
            )
        end},
        {"value containing '=' is preserved (only the first '=' splits)", fun() ->
            ?assertEqual(
                ["credential_process ", " aws sso ... --account=123 --role=admin"],
                aws_lib_config:ini_split_line(
                    <<"credential_process = aws sso ... --account=123 --role=admin">>
                )
            )
        end},
        {"blank line is a single empty token", fun() ->
            ?assertEqual([""], aws_lib_config:ini_split_line(<<>>)),
            ?assertEqual([""], aws_lib_config:ini_split_line(<<" ">>))
        end}
    ].

ini_parse_line_parts_does_not_crash_on_value_with_equals_test_() ->
    [
        {"a credential_process value with '=' parses as a single string value", fun() ->
            Line = <<"credential_process = cred-helper print --account=123456789012">>,
            {NewSection, none} = aws_lib_config:ini_parse_line(
                [], none, Line
            ),
            ?assertEqual(
                "cred-helper print --account=123456789012",
                proplists:get_value(credential_process, NewSection)
            )
        end}
    ].

maybe_convert_number_test_() ->
    [
        {"when string contains an integer", fun() ->
            ?assertEqual(123, aws_lib_config:maybe_convert_number("123"))
        end},
        {"when string contains a float", fun() ->
            ?assertEqual(123.456, aws_lib_config:maybe_convert_number("123.456"))
        end},
        {"when string does not contain a number", fun() ->
            ?assertEqual("hello, world", aws_lib_config:maybe_convert_number("hello, world"))
        end}
    ].

parse_iso8601_test_() ->
    [
        {"parse test", fun() ->
            Value = "2016-05-19T18:25:23Z",
            Expectation = {{2016, 5, 19}, {18, 25, 23}},
            ?assertEqual(Expectation, aws_lib_config:parse_iso8601_timestamp(Value))
        end}
    ].

profile_test_() ->
    [
        {"from environment variable", fun() ->
            os:putenv("AWS_DEFAULT_PROFILE", "httpc-aws test"),
            ?assertEqual("httpc-aws test", aws_lib_config:profile())
        end},
        {"default without environment variable", fun() ->
            os:unsetenv("AWS_DEFAULT_PROFILE"),
            ?assertEqual("default", aws_lib_config:profile())
        end}
    ].

read_file_test_() ->
    [
        {"file does not exist", fun() ->
            ?assertEqual(
                {error, enoent},
                aws_lib_config:read_file(filename:join([filename:absname("."), "bad_path"]))
            )
        end}
    ].

region_test_() ->
    {
        foreach,
        fun() ->
            meck:new(gun, []),
            meck:new(aws_lib, [passthrough]),
            reset_environment(),
            application:set_env(aws, aws_prefer_imdsv2, false),
            [gun, aws_lib]
        end,
        fun(Mods) ->
            application:unset_env(aws, aws_prefer_imdsv2),
            meck:unload(Mods)
        end,
        [
            {"with environment variable", fun() ->
                os:putenv("AWS_DEFAULT_REGION", "us-west-1"),
                S = #aws_config{},
                ?assertEqual({ok, "us-west-1", S}, aws_lib_config:region(S))
            end},
            {"with config file and specified profile", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                ?assertEqual({ok, "us-west-5", S}, aws_lib_config:region("testing", S))
            end},
            {"with config file using default profile", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                ?assertEqual({ok, "us-east-4", S}, aws_lib_config:region(S))
            end},
            {"missing profile in config", fun() ->
                setup_test_config_env_var(),
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                ?assertEqual({ok, ?DEFAULT_REGION, S}, aws_lib_config:region("no-region", S))
            end},
            {"from instance metadata service", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 200, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"us-west-1a">>} end),
                ?assertEqual({ok, "us-west-1", S}, aws_lib_config:region(S))
            end},
            {"full lookup failure", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                ?assertEqual({ok, ?DEFAULT_REGION, S}, aws_lib_config:region(S))
            end},
            {"http error failure", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 500, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"Internal Server Error">>} end),
                ?assertEqual({ok, ?DEFAULT_REGION, S}, aws_lib_config:region(S))
            end}
        ]
    }.

instance_id_test_() ->
    {
        foreach,
        fun() ->
            meck:new(gun, []),
            meck:new(aws_lib, [passthrough]),
            reset_environment(),
            application:set_env(aws, aws_prefer_imdsv2, false),
            [gun, aws_lib]
        end,
        fun(Mods) ->
            application:unset_env(aws, aws_prefer_imdsv2),
            meck:unload(Mods)
        end,
        [
            {"get instance id successfully", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, undefined, S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 200, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"instance-id">>} end),
                ?assertEqual({ok, "instance-id", S}, aws_lib_config:instance_id(S))
            end},
            {"getting instance id is rejected with invalid token error", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, "invalid", S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 401, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"Invalid token">>} end),
                ?assertEqual({error, undefined}, aws_lib_config:instance_id(S))
            end},
            {"getting instance id is rejected with access denied error", fun() ->
                S = #aws_config{},
                meck:expect(aws_lib, ensure_imdsv2_token_valid, 1, {ok, "expired token", S}),
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 403, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, <<"access denied">>} end),
                ?assertEqual({error, undefined}, aws_lib_config:instance_id(S))
            end}
        ]
    }.

load_imdsv2_token_test_() ->
    {
        foreach,
        fun() ->
            meck:new(gun, []),
            [gun]
        end,
        fun meck:unload/1,
        [
            {"fail to get imdsv2 token - timeout", fun() ->
                meck:expect(gun, open, fun(_, _, _) -> {error, timeout} end),
                ?assertEqual(undefined, aws_lib_config:load_imdsv2_token())
            end},
            {"fail to get imdsv2 token - PUT request is not valid", fun() ->
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, put, fun(_, _, _, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 400, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) ->
                    {ok, <<"Missing or Invalid Parameters – The PUT request is not valid.">>}
                end),
                ?assertEqual(undefined, aws_lib_config:load_imdsv2_token())
            end},
            {"fail to get imdsv2 token - body read timeout", fun() ->
                %% The 200 response headers arrive but the body read times out;
                %% this must fall back to undefined, not crash with a badmatch.
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, put, fun(_, _, _, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 200, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {error, timeout} end),
                ?assertEqual(undefined, aws_lib_config:load_imdsv2_token())
            end},
            {"successfully get imdsv2 token from instance metadata service", fun() ->
                IMDSv2Token = "super_secret_token_value",
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, put, fun(_, _, _, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 200, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, list_to_binary(IMDSv2Token)} end),
                ?assertEqual(IMDSv2Token, aws_lib_config:load_imdsv2_token())
            end}
        ]
    }.

maybe_imdsv2_token_headers_test_() ->
    {
        foreach,
        fun() ->
            meck:new(gun, []),
            meck:new(aws_lib, [passthrough]),
            [gun, aws_lib]
        end,
        fun meck:unload/1,
        [
            {"imdsv2 token is not available", fun() ->
                S = #aws_config{},
                % Mock Gun to simulate IMDSv2 token loading failure
                meck:expect(gun, open, fun(_, _, _) -> {error, timeout} end),
                ?assertEqual({ok, [], S}, aws_lib_config:maybe_imdsv2_token_headers(S))
            end},

            {"imdsv2 is available", fun() ->
                S = #aws_config{},
                IMDSv2Token = "super_secret_token_value ;)",
                % Mock Gun to simulate successful IMDSv2 token loading
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, put, fun(_, _, _, _, _) -> stream_ref end),
                meck:expect(gun, await, fun(_, _, _) -> {response, nofin, 200, headers} end),
                meck:expect(gun, await_body, fun(_, _, _) -> {ok, list_to_binary(IMDSv2Token)} end),
                {ok, Headers, _S1} = aws_lib_config:maybe_imdsv2_token_headers(S),
                ?assertEqual([{"X-aws-ec2-metadata-token", IMDSv2Token}], Headers)
            end}
        ]
    }.

%% ============================================================================
%% Credential chain resolution tests (role_arn / source_profile)
%% ============================================================================

credential_chain_test_() ->
    {
        foreach,
        fun() ->
            meck:new(gun, []),
            meck:new(aws_iam, [no_link]),
            reset_environment(),
            setup_test_config_env_var(),
            setup_test_credentials_env_var(),
            application:set_env(aws, aws_prefer_imdsv2, false),
            [gun, aws_iam]
        end,
        fun(Mods) ->
            application:unset_env(aws, aws_prefer_imdsv2),
            meck:unload(Mods)
        end,
        [
            {"role_arn + source_profile resolves via STS", fun() ->
                %% Mock aws_iam:assume_role/3 to return creds
                meck:expect(aws_iam, assume_role, fun(_RoleArn, _Opts, _State) ->
                    {ok, #aws_credentials{
                        access_key = "ASSUMED_KEY",
                        secret_key = "ASSUMED_SECRET",
                        security_token = "ASSUMED_TOKEN",
                        expiration = {{2026, 12, 31}, {23, 59, 59}}
                    }}
                end),
                S = #aws_config{},
                {ok, Creds, _Config} = aws_lib_config:credentials("chained", S),
                ?assertEqual("ASSUMED_KEY", Creds#aws_credentials.access_key),
                ?assertEqual("ASSUMED_SECRET", Creds#aws_credentials.secret_key),
                ?assertEqual("ASSUMED_TOKEN", Creds#aws_credentials.security_token),
                ?assertEqual(
                    {{2026, 12, 31}, {23, 59, 59}}, Creds#aws_credentials.expiration
                ),
                %% Verify assume_role/3 was called with the correct RoleArn
                [{_Pid, {aws_iam, assume_role, [RoleArn, Opts, _St]}, _Ret}] =
                    meck:history(aws_iam),
                ?assertEqual("arn:aws:iam::123456789012:role/TestRole", RoleArn),
                %% Opts should have role_session_name derived from source profile
                ?assert(maps:is_key(role_session_name, Opts)),
                ?assertNot(maps:is_key(external_id, Opts))
            end},
            {"role_arn + source_profile + external_id passes ExternalId", fun() ->
                meck:expect(aws_iam, assume_role, fun(_RoleArn, Opts, _State) ->
                    %% Verify external_id is passed
                    ?assertEqual(
                        "test-external-id-12345", maps:get(external_id, Opts)
                    ),
                    {ok, #aws_credentials{
                        access_key = "AK",
                        secret_key = "SK",
                        security_token = "ST",
                        expiration = undefined
                    }}
                end),
                S = #aws_config{},
                {ok, _Creds, _Config} = aws_lib_config:credentials(
                    "chained-with-external-id", S
                ),
                ok
            end},
            {"role_arn without source_profile uses section's own creds", fun() ->
                meck:expect(aws_iam, assume_role, fun(RoleArn, _Opts, State) ->
                    %% Verify the State has the inline credentials
                    {ok, InlineCreds} = aws_lib:get_credentials(State),
                    ?assertEqual(
                        "AKIAIOSFODNN7EXAMPLE",
                        InlineCreds#aws_credentials.access_key
                    ),
                    ?assertEqual(
                        "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
                        InlineCreds#aws_credentials.secret_key
                    ),
                    ?assertEqual(
                        "arn:aws:iam::123456789012:role/TestRole", RoleArn
                    ),
                    {ok, #aws_credentials{
                        access_key = "ROLE_KEY",
                        secret_key = "ROLE_SECRET",
                        security_token = "ROLE_TOKEN",
                        expiration = undefined
                    }}
                end),
                S = #aws_config{},
                {ok, Creds, _Config} = aws_lib_config:credentials("role-no-source", S),
                ?assertEqual("ROLE_KEY", Creds#aws_credentials.access_key)
            end},
            {"cycle detection returns error", fun() ->
                S = #aws_config{},
                Result = aws_lib_config:credentials("cycle-a", S),
                ?assertMatch({error, {credential_chain_cycle, _, _}}, Result)
            end},
            {"missing source_profile returns error (NOT fallthrough to IMDS)", fun() ->
                S = #aws_config{},
                Result = aws_lib_config:credentials("missing-source", S),
                ?assertMatch({error, {source_profile_not_found, "nonexistent_profile"}}, Result)
            end},
            {"credential_source returns unsupported error", fun() ->
                S = #aws_config{},
                Result = aws_lib_config:credentials("credential-source-test", S),
                ?assertMatch(
                    {error, {credential_source_not_supported, _, "Ec2InstanceMetadata"}},
                    Result
                )
            end},
            {"invalid session name returns error", fun() ->
                S = #aws_config{},
                Result = aws_lib_config:credentials("invalid-session-name", S),
                ?assertMatch(
                    {error, {invalid_role_session_name, _}}, Result
                )
            end},
            {"custom session name is passed through", fun() ->
                meck:expect(aws_iam, assume_role, fun(_RoleArn, Opts, _State) ->
                    ?assertEqual(
                        "my-custom-session", maps:get(role_session_name, Opts)
                    ),
                    {ok, #aws_credentials{
                        access_key = "AK",
                        secret_key = "SK",
                        security_token = "ST",
                        expiration = undefined
                    }}
                end),
                S = #aws_config{},
                {ok, _Creds, _Config} = aws_lib_config:credentials(
                    "custom-session-name", S
                ),
                ok
            end},
            {"deep chain resolves through intermediate profile", fun() ->
                %% deep-chain -> intermediate -> default
                %% Each hop calls assume_role/3
                CallCount = counters:new(1, []),
                meck:expect(aws_iam, assume_role, fun(_RoleArn, _Opts, _State) ->
                    counters:add(CallCount, 1, 1),
                    {ok, #aws_credentials{
                        access_key = "DEEP_KEY",
                        secret_key = "DEEP_SECRET",
                        security_token = "DEEP_TOKEN",
                        expiration = undefined
                    }}
                end),
                S = #aws_config{},
                {ok, Creds, _Config} = aws_lib_config:credentials("deep-chain", S),
                ?assertEqual("DEEP_KEY", Creds#aws_credentials.access_key),
                %% Should have been called twice: once for intermediate, once for
                %% deep-chain
                ?assertEqual(2, counters:get(CallCount, 1))
            end},
            {"env vars still win over profile with role_arn", fun() ->
                os:putenv("AWS_ACCESS_KEY_ID", "ENV_KEY"),
                os:putenv("AWS_SECRET_ACCESS_KEY", "ENV_SECRET"),
                S = #aws_config{},
                {ok, Creds, _Config} = aws_lib_config:credentials("chained", S),
                %% Environment variables take precedence -- no AssumeRole call
                ?assertEqual("ENV_KEY", Creds#aws_credentials.access_key),
                ?assertEqual("ENV_SECRET", Creds#aws_credentials.secret_key),
                %% aws_iam:assume_role should NOT have been called
                ?assertEqual(0, meck:num_calls(aws_iam, assume_role, '_'))
            end},
            {"assume_role failure propagates error", fun() ->
                meck:expect(aws_iam, assume_role, fun(_RoleArn, _Opts, _State) ->
                    {error, "Access Denied"}
                end),
                S = #aws_config{},
                Result = aws_lib_config:credentials("chained", S),
                ?assertMatch(
                    {error, {assume_role_failed, _, "Access Denied"}}, Result
                )
            end}
        ]
    }.

%% Session name validation unit tests
session_name_validation_test_() ->
    [
        {"valid simple name", fun() ->
            ?assert(aws_lib_config:validate_session_name("my-session"))
        end},
        {"valid with all allowed chars", fun() ->
            ?assert(aws_lib_config:validate_session_name("a+=,.@-b"))
        end},
        {"too short (1 char)", fun() ->
            ?assertNot(aws_lib_config:validate_session_name("a"))
        end},
        {"too long (65 chars)", fun() ->
            LongName = lists:duplicate(65, $a),
            ?assertNot(aws_lib_config:validate_session_name(LongName))
        end},
        {"exactly 64 chars is valid", fun() ->
            Name64 = lists:duplicate(64, $a),
            ?assert(aws_lib_config:validate_session_name(Name64))
        end},
        {"exactly 2 chars is valid", fun() ->
            ?assert(aws_lib_config:validate_session_name("ab"))
        end},
        {"spaces are invalid", fun() ->
            ?assertNot(aws_lib_config:validate_session_name("has space"))
        end},
        {"exclamation marks are invalid", fun() ->
            ?assertNot(aws_lib_config:validate_session_name("invalid!!!"))
        end}
    ].

reset_environment() ->
    os:unsetenv("AWS_ACCESS_KEY_ID"),
    os:unsetenv("AWS_DEFAULT_REGION"),
    os:unsetenv("AWS_SECRET_ACCESS_KEY"),
    os:unsetenv("AWS_SESSION_TOKEN"),
    setup_test_file_with_env_var("AWS_CONFIG_FILE", "bad_config.ini"),
    setup_test_file_with_env_var(
        "AWS_SHARED_CREDENTIALS_FILE",
        "bad_credentials.ini"
    ),
    meck:expect(gun, open, fun(_, _, _) -> {error, timeout} end).

setup_test_config_env_var() ->
    setup_test_file_with_env_var("AWS_CONFIG_FILE", "test_aws_config.ini").

setup_test_file_with_env_var(EnvVar, Filename) ->
    os:putenv(
        EnvVar,
        filename:join([
            filename:absname("."),
            "test",
            Filename
        ])
    ).

setup_test_credentials_env_var() ->
    setup_test_file_with_env_var(
        "AWS_SHARED_CREDENTIALS_FILE",
        "test_aws_credentials.ini"
    ).
