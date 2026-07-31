-module(aws_lib_credential_process_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("aws_lib.hrl").

%%====================================================================
%% Command parsing tests
%%====================================================================

parse_command_test_() ->
    [
        {"simple command with no args", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", []}},
                aws_lib_credential_process:parse_command("/usr/bin/helper")
            )
        end},
        {"command with arguments", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", ["--profile", "default"]}},
                aws_lib_credential_process:parse_command("/usr/bin/helper --profile default")
            )
        end},
        {"command with single-quoted argument", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", ["hello world"]}},
                aws_lib_credential_process:parse_command("/usr/bin/helper 'hello world'")
            )
        end},
        {"command with double-quoted argument", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", ["hello world"]}},
                aws_lib_credential_process:parse_command("/usr/bin/helper \"hello world\"")
            )
        end},
        {"command with backslash escape", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", ["hello world"]}},
                aws_lib_credential_process:parse_command("/usr/bin/helper hello\\ world")
            )
        end},
        {"command with equals in argument", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/aws", ["sso", "get-credentials", "--account=12345"]}},
                aws_lib_credential_process:parse_command(
                    "/usr/bin/aws sso get-credentials --account=12345"
                )
            )
        end},
        {"empty command returns error", fun() ->
            ?assertEqual(
                {error, command_parse_error},
                aws_lib_credential_process:parse_command("")
            )
        end},
        {"whitespace-only command returns error", fun() ->
            ?assertEqual(
                {error, command_parse_error},
                aws_lib_credential_process:parse_command("   ")
            )
        end},
        {"unterminated single quote returns error", fun() ->
            ?assertEqual(
                {error, command_parse_error},
                aws_lib_credential_process:parse_command("/usr/bin/helper 'unterminated")
            )
        end},
        {"unterminated double quote returns error", fun() ->
            ?assertEqual(
                {error, command_parse_error},
                aws_lib_credential_process:parse_command("/usr/bin/helper \"unterminated")
            )
        end},
        {"command with multiple spaces between args", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", ["--arg1", "--arg2"]}},
                aws_lib_credential_process:parse_command("/usr/bin/helper   --arg1   --arg2")
            )
        end},
        {"command with leading/trailing whitespace", fun() ->
            ?assertEqual(
                {ok, {"/usr/bin/helper", ["--arg"]}},
                aws_lib_credential_process:parse_command("  /usr/bin/helper --arg  ")
            )
        end}
    ].

%%====================================================================
%% parse_output tests
%%====================================================================

parse_output_test_() ->
    [
        {"valid full output with all fields", fun() ->
            Json =
                <<"{\"Version\": 1, \"AccessKeyId\": \"AKID\", \"SecretAccessKey\": \"SECRET\", \"SessionToken\": \"TOKEN\", \"Expiration\": \"2026-01-15T12:30:00Z\"}">>,
            {ok, Creds} = aws_lib_credential_process:parse_output(Json),
            ?assertEqual("AKID", Creds#aws_credentials.access_key),
            ?assertEqual("SECRET", Creds#aws_credentials.secret_key),
            ?assertEqual("TOKEN", Creds#aws_credentials.security_token),
            ?assertEqual({{2026, 1, 15}, {12, 30, 0}}, Creds#aws_credentials.expiration)
        end},
        {"valid output without optional fields", fun() ->
            Json =
                <<"{\"Version\": 1, \"AccessKeyId\": \"AKID\", \"SecretAccessKey\": \"SECRET\"}">>,
            {ok, Creds} = aws_lib_credential_process:parse_output(Json),
            ?assertEqual("AKID", Creds#aws_credentials.access_key),
            ?assertEqual("SECRET", Creds#aws_credentials.secret_key),
            ?assertEqual(undefined, Creds#aws_credentials.security_token),
            ?assertEqual(undefined, Creds#aws_credentials.expiration)
        end},
        {"version as string '1' is accepted", fun() ->
            Json =
                <<"{\"Version\": \"1\", \"AccessKeyId\": \"AKID\", \"SecretAccessKey\": \"SECRET\"}">>,
            {ok, Creds} = aws_lib_credential_process:parse_output(Json),
            ?assertEqual("AKID", Creds#aws_credentials.access_key)
        end},
        {"invalid version returns error", fun() ->
            Json =
                <<"{\"Version\": 2, \"AccessKeyId\": \"AKID\", \"SecretAccessKey\": \"SECRET\"}">>,
            ?assertEqual(
                {error, {credential_process, invalid_version}},
                aws_lib_credential_process:parse_output(Json)
            )
        end},
        {"missing version returns error", fun() ->
            Json = <<"{\"AccessKeyId\": \"AKID\", \"SecretAccessKey\": \"SECRET\"}">>,
            ?assertEqual(
                {error, {credential_process, invalid_version}},
                aws_lib_credential_process:parse_output(Json)
            )
        end},
        {"missing AccessKeyId returns error", fun() ->
            Json = <<"{\"Version\": 1, \"SecretAccessKey\": \"SECRET\"}">>,
            ?assertEqual(
                {error, {credential_process, missing_fields}},
                aws_lib_credential_process:parse_output(Json)
            )
        end},
        {"missing SecretAccessKey returns error", fun() ->
            Json = <<"{\"Version\": 1, \"AccessKeyId\": \"AKID\"}">>,
            ?assertEqual(
                {error, {credential_process, missing_fields}},
                aws_lib_credential_process:parse_output(Json)
            )
        end},
        {"invalid JSON returns error", fun() ->
            ?assertEqual(
                {error, {credential_process, invalid_json}},
                aws_lib_credential_process:parse_output(<<"not json at all">>)
            )
        end},
        {"empty input returns error", fun() ->
            %% Empty binary decodes to empty proplist, which fails on
            %% missing Version field.
            ?assertEqual(
                {error, {credential_process, invalid_version}},
                aws_lib_credential_process:parse_output(<<>>)
            )
        end}
    ].

%%====================================================================
%% Execute integration tests (using real helper scripts)
%%====================================================================

execute_test_() ->
    {foreach, fun setup_helper_scripts/0, fun cleanup_helper_scripts/1, [
        {"successful execution with full credentials", fun() ->
            {ok, Creds} = aws_lib_credential_process:execute(
                helper_script_path("success_full")
            ),
            ?assertEqual("AKIAIOSFODNN7EXAMPLE", Creds#aws_credentials.access_key),
            ?assertEqual(
                "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY", Creds#aws_credentials.secret_key
            ),
            ?assertEqual("FwoGZXIvYXdzEBYaDA==", Creds#aws_credentials.security_token),
            ?assertEqual({{2026, 12, 31}, {23, 59, 59}}, Creds#aws_credentials.expiration)
        end},
        {"successful execution without optional fields", fun() ->
            {ok, Creds} = aws_lib_credential_process:execute(
                helper_script_path("success_minimal")
            ),
            ?assertEqual("AKIAIOSFODNN7EXAMPLE", Creds#aws_credentials.access_key),
            ?assertEqual(
                "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY", Creds#aws_credentials.secret_key
            ),
            ?assertEqual(undefined, Creds#aws_credentials.security_token),
            ?assertEqual(undefined, Creds#aws_credentials.expiration)
        end},
        {"non-zero exit code returns execution_failed", fun() ->
            ?assertEqual(
                {error, {credential_process, execution_failed}},
                aws_lib_credential_process:execute(helper_script_path("exit_nonzero"))
            )
        end},
        {"invalid JSON output returns invalid_json", fun() ->
            ?assertEqual(
                {error, {credential_process, invalid_json}},
                aws_lib_credential_process:execute(helper_script_path("invalid_json"))
            )
        end},
        {"missing required fields returns missing_fields", fun() ->
            ?assertEqual(
                {error, {credential_process, missing_fields}},
                aws_lib_credential_process:execute(helper_script_path("missing_fields"))
            )
        end},
        {"invalid version returns invalid_version", fun() ->
            ?assertEqual(
                {error, {credential_process, invalid_version}},
                aws_lib_credential_process:execute(helper_script_path("invalid_version"))
            )
        end},
        {"command not found returns command_not_found", fun() ->
            ?assertEqual(
                {error, {credential_process, command_not_found}},
                aws_lib_credential_process:execute("/nonexistent/path/to/helper")
            )
        end},
        {"output too large returns output_too_large", fun() ->
            ?assertEqual(
                {error, {credential_process, output_too_large}},
                aws_lib_credential_process:execute(helper_script_path("output_too_large"))
            )
        end},
        {"shell metacharacters are not interpreted", fun() ->
            %% This command contains shell metacharacters that would be
            %% dangerous if passed through a shell. With spawn_executable,
            %% they are treated as literal arguments.
            ?assertEqual(
                {error, {credential_process, command_not_found}},
                aws_lib_credential_process:execute(
                    "/nonexistent/binary; rm -rf / #"
                )
            )
        end},
        {"timeout triggers error", fun() ->
            %% The sleep helper sleeps for 60s, which exceeds the 30s timeout.
            %% We call collect_output directly with a short deadline for fast
            %% test execution. The deadline is an absolute monotonic timestamp.
            Script = helper_script_path("sleep_forever"),
            Port = erlang:open_port(
                {spawn_executable, Script},
                [binary, exit_status, use_stdio, stderr_to_stdout, hide]
            ),
            Deadline = erlang:monotonic_time(millisecond) + 500,
            Result = aws_lib_credential_process:collect_output(Port, <<>>, Deadline),
            ?assertEqual({error, {credential_process, timeout}}, Result)
        end}
    ]}.

%%====================================================================
%% Test helpers
%%====================================================================

helper_dir() ->
    "/tmp/aws_credential_process_test_helpers".

helper_script_path(Name) ->
    filename:join(helper_dir(), Name).

setup_helper_scripts() ->
    Dir = helper_dir(),
    filelib:ensure_dir(filename:join(Dir, "dummy")),

    %% Full credentials output
    write_script(
        Dir,
        "success_full",
        "#!/bin/sh\n"
        "echo '{\"Version\": 1, \"AccessKeyId\": \"AKIAIOSFODNN7EXAMPLE\", "
        "\"SecretAccessKey\": \"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\", "
        "\"SessionToken\": \"FwoGZXIvYXdzEBYaDA==\", "
        "\"Expiration\": \"2026-12-31T23:59:59Z\"}'\n"
    ),

    %% Minimal credentials (no SessionToken, no Expiration)
    write_script(
        Dir,
        "success_minimal",
        "#!/bin/sh\n"
        "echo '{\"Version\": 1, \"AccessKeyId\": \"AKIAIOSFODNN7EXAMPLE\", "
        "\"SecretAccessKey\": \"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\"}'\n"
    ),

    %% Non-zero exit
    write_script(
        Dir,
        "exit_nonzero",
        "#!/bin/sh\nexit 1\n"
    ),

    %% Invalid JSON
    write_script(
        Dir,
        "invalid_json",
        "#!/bin/sh\necho 'this is not json'\n"
    ),

    %% Missing required fields
    write_script(
        Dir,
        "missing_fields",
        "#!/bin/sh\n"
        "echo '{\"Version\": 1, \"AccessKeyId\": \"AKID\"}'\n"
    ),

    %% Invalid version
    write_script(
        Dir,
        "invalid_version",
        "#!/bin/sh\n"
        "echo '{\"Version\": 99, \"AccessKeyId\": \"AKID\", \"SecretAccessKey\": \"SECRET\"}'\n"
    ),

    %% Output too large (> 64KB)
    write_script(
        Dir,
        "output_too_large",
        "#!/bin/sh\n"
        "dd if=/dev/zero bs=1024 count=128 2>/dev/null | tr '\\0' 'A'\n"
    ),

    %% Sleep forever (for timeout testing)
    write_script(
        Dir,
        "sleep_forever",
        "#!/bin/sh\nsleep 60\n"
    ),

    Dir.

cleanup_helper_scripts(Dir) ->
    %% Remove all helper scripts
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(F) -> file:delete(filename:join(Dir, F)) end, Files),
            file:del_dir(Dir);
        _ ->
            ok
    end.

write_script(Dir, Name, Content) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Content),
    %% Make executable
    {ok, Info} = file:read_file_info(Path),
    ok = file:write_file_info(Path, Info#file_info{mode = 8#755}).
