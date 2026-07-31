%% ====================================================================
%% @doc Execute an external credential_process helper and parse its JSON
%%      output into an aws_credentials record.
%%
%%      This module implements the AWS credential_process contract: it
%%      executes the command specified by the `credential_process' key in
%%      ~/.aws/config, collects stdout with bounded size and timeout, then
%%      parses the JSON output.
%%
%%      Security invariants:
%%      - No shell invocation (open_port with spawn_executable).
%%      - Output size bounded to 64KB; port killed on overflow.
%%      - Hard 30-second timeout; port killed on expiry.
%%      - Credential values never appear in logs or error tuples.
%% @end
%% ====================================================================
-module(aws_lib_credential_process).

-export([execute/1]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("aws_lib.hrl").
-include_lib("kernel/include/logger.hrl").

%% Maximum output size from the credential helper (64KB).
-define(MAX_OUTPUT_BYTES, 65536).

%% Hard timeout for the credential helper process (30 seconds).
-define(PROCESS_TIMEOUT_MS, 30000).

-spec execute(Command :: string()) ->
    {ok, aws_credentials()} | {error, {credential_process, atom()}}.
%% @doc Execute the credential_process command, parse its JSON output, and
%%      return an aws_credentials record on success. The command string is
%%      parsed into an executable path and argument list without invoking a
%%      shell. Returns a categorized error on any failure.
%% @end
execute(Command) ->
    case parse_command(Command) of
        {ok, {Executable, Args}} ->
            ?LOG_DEBUG("credential_process: invoking helper ~ts", [Executable]),
            execute_helper(Executable, Args);
        {error, Reason} ->
            {error, {credential_process, Reason}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec execute_helper(Executable :: string(), Args :: [string()]) ->
    {ok, aws_credentials()} | {error, {credential_process, atom()}}.
%% @doc Open a port to the helper executable, collect output with bounds,
%%      and parse the result.
%% @end
execute_helper(Executable, Args) ->
    PortOpts = [
        {args, Args},
        binary,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        hide
    ],
    try
        Port = erlang:open_port({spawn_executable, Executable}, PortOpts),
        Deadline = erlang:monotonic_time(millisecond) + ?PROCESS_TIMEOUT_MS,
        collect_output(Port, <<>>, Deadline)
    catch
        error:enoent ->
            ?LOG_ERROR("credential_process: command not found"),
            {error, {credential_process, command_not_found}};
        error:eacces ->
            ?LOG_ERROR("credential_process: permission denied"),
            {error, {credential_process, command_not_found}};
        error:Reason ->
            ?LOG_ERROR("credential_process: port open failed: ~tp", [Reason]),
            {error, {credential_process, execution_failed}}
    end.

-spec collect_output(Port :: port(), Acc :: binary(), Deadline :: integer()) ->
    {ok, aws_credentials()} | {error, {credential_process, atom()}}.
%% @doc Receive loop for port data. Enforces a wall-clock deadline and output
%%      size cap, killing the port on either violation. Deadline is an absolute
%%      monotonic timestamp (milliseconds) computed at invocation start.
%% @end
collect_output(Port, Acc, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {data, Data}} ->
            NewAcc = <<Acc/binary, Data/binary>>,
            case byte_size(NewAcc) > ?MAX_OUTPUT_BYTES of
                true ->
                    kill_port(Port),
                    ?LOG_ERROR("credential_process: output exceeded size limit"),
                    {error, {credential_process, output_too_large}};
                false ->
                    collect_output(Port, NewAcc, Deadline)
            end;
        {Port, {exit_status, 0}} ->
            parse_output(Acc);
        {Port, {exit_status, _NonZero}} ->
            ?LOG_ERROR("credential_process: helper exited with non-zero status"),
            {error, {credential_process, execution_failed}}
    after Remaining ->
        kill_port(Port),
        ?LOG_ERROR("credential_process: helper timed out"),
        {error, {credential_process, timeout}}
    end.

-spec kill_port(Port :: port()) -> ok.
%% @doc Close the port, which sends SIGKILL to the OS process on most
%%      platforms. Ignores errors if the port is already closed.
%% @end
kill_port(Port) ->
    try
        erlang:port_close(Port)
    catch
        error:badarg -> ok
    end,
    %% Flush any remaining messages from the port
    flush_port(Port).

-spec flush_port(Port :: port()) -> ok.
%% @doc Drain any remaining messages from a closed port.
%% @end
flush_port(Port) ->
    receive
        {Port, _} -> flush_port(Port)
    after 0 ->
        ok
    end.

-spec parse_output(Output :: binary()) ->
    {ok, aws_credentials()} | {error, {credential_process, atom()}}.
%% @doc Decode the JSON output from the credential helper, validate the
%%      Version field, and extract credential fields.
%% @end
parse_output(Output) ->
    try
        Parsed = aws_lib_json:decode(Output),
        validate_and_extract(Parsed)
    catch
        _:_ ->
            ?LOG_ERROR("credential_process: failed to parse JSON output"),
            {error, {credential_process, invalid_json}}
    end.

-spec validate_and_extract(Parsed :: list()) ->
    {ok, aws_credentials()} | {error, {credential_process, atom()}}.
%% @doc Validate Version == 1 and extract credential fields from the
%%      parsed proplist.
%% @end
validate_and_extract(Parsed) ->
    case validate_version(Parsed) of
        ok ->
            extract_credentials(Parsed);
        {error, _} = Error ->
            Error
    end.

-spec validate_version(Parsed :: list()) ->
    ok | {error, {credential_process, invalid_version}}.
%% @doc Validate the Version field is 1 (integer or string "1").
%% @end
validate_version(Parsed) ->
    case proplists:get_value("Version", Parsed) of
        1 ->
            ok;
        "1" ->
            ok;
        _ ->
            ?LOG_ERROR("credential_process: invalid or missing Version field"),
            {error, {credential_process, invalid_version}}
    end.

-spec extract_credentials(Parsed :: list()) ->
    {ok, aws_credentials()} | {error, {credential_process, missing_fields}}.
%% @doc Extract AccessKeyId, SecretAccessKey, and optional SessionToken
%%      and Expiration from the parsed output.
%% @end
extract_credentials(Parsed) ->
    AccessKeyId = proplists:get_value("AccessKeyId", Parsed),
    SecretAccessKey = proplists:get_value("SecretAccessKey", Parsed),
    case {AccessKeyId, SecretAccessKey} of
        {undefined, _} ->
            ?LOG_ERROR("credential_process: missing required fields in output"),
            {error, {credential_process, missing_fields}};
        {_, undefined} ->
            ?LOG_ERROR("credential_process: missing required fields in output"),
            {error, {credential_process, missing_fields}};
        {_, _} ->
            SessionToken = proplists:get_value("SessionToken", Parsed),
            Expiration = parse_expiration(proplists:get_value("Expiration", Parsed)),
            Creds = #aws_credentials{
                access_key = AccessKeyId,
                secret_key = SecretAccessKey,
                security_token = SessionToken,
                expiration = Expiration
            },
            {ok, Creds}
    end.

-spec parse_expiration(Value :: string() | undefined) -> expiration().
%% @doc Parse the optional Expiration field from ISO8601 format. Returns
%%      undefined when not present (long-lived credentials).
%% @end
parse_expiration(undefined) ->
    undefined;
parse_expiration(Timestamp) ->
    try
        aws_lib_config:parse_iso8601_timestamp(Timestamp)
    catch
        _:_ ->
            ?LOG_WARNING("credential_process: could not parse Expiration timestamp"),
            undefined
    end.

%%====================================================================
%% Command parsing
%%====================================================================

-spec parse_command(Command :: string()) ->
    {ok, {Executable :: string(), Args :: [string()]}} | {error, command_parse_error}.
%% @doc Parse a command string into an executable path and argument list.
%%      Handles single and double quoting, and backslash escaping outside
%%      of quotes. Does NOT invoke a shell -- the result is suitable for
%%      open_port({spawn_executable, ...}, [{args, ...}]).
%% @end
parse_command(Command) ->
    Stripped = string:strip(Command),
    case Stripped of
        "" ->
            {error, command_parse_error};
        _ ->
            case tokenize(Stripped) of
                {ok, []} ->
                    {error, command_parse_error};
                {ok, [Exe | Args]} ->
                    {ok, {Exe, Args}};
                {error, _} ->
                    {error, command_parse_error}
            end
    end.

-spec tokenize(Input :: string()) ->
    {ok, [string()]} | {error, unterminated_quote}.
%% @doc Split a command string into tokens respecting single quotes, double
%%      quotes, and backslash escaping.
%% @end
tokenize(Input) ->
    tokenize(Input, [], [], none).

%% State: none -- not inside quotes
%% State: single -- inside single quotes
%% State: double -- inside double quotes
-spec tokenize(
    Input :: string(),
    CurrentToken :: string(),
    Tokens :: [string()],
    QuoteState :: none | single | double
) -> {ok, [string()]} | {error, unterminated_quote}.
%% End of input
tokenize([], [], Tokens, none) ->
    {ok, lists:reverse(Tokens)};
tokenize([], CurrentToken, Tokens, none) ->
    {ok, lists:reverse([lists:reverse(CurrentToken) | Tokens])};
tokenize([], _CurrentToken, _Tokens, _QuoteState) ->
    {error, unterminated_quote};
%% Backslash escaping outside quotes -- escape the next character
tokenize([$\\, Next | Rest], CurrentToken, Tokens, none) ->
    tokenize(Rest, [Next | CurrentToken], Tokens, none);
%% Trailing backslash at end of input
tokenize([$\\], CurrentToken, Tokens, none) ->
    {ok, lists:reverse([lists:reverse(CurrentToken) | Tokens])};
%% Start single quote
tokenize([$' | Rest], CurrentToken, Tokens, none) ->
    tokenize(Rest, CurrentToken, Tokens, single);
%% End single quote
tokenize([$' | Rest], CurrentToken, Tokens, single) ->
    tokenize(Rest, CurrentToken, Tokens, none);
%% Inside single quotes -- everything is literal
tokenize([C | Rest], CurrentToken, Tokens, single) ->
    tokenize(Rest, [C | CurrentToken], Tokens, single);
%% Start double quote
tokenize([$" | Rest], CurrentToken, Tokens, none) ->
    tokenize(Rest, CurrentToken, Tokens, double);
%% End double quote
tokenize([$" | Rest], CurrentToken, Tokens, double) ->
    tokenize(Rest, CurrentToken, Tokens, none);
%% Backslash inside double quotes -- only escapes specific characters
tokenize([$\\, C | Rest], CurrentToken, Tokens, double) when
    C =:= $\\; C =:= $"; C =:= $$; C =:= $`; C =:= $\n
->
    tokenize(Rest, [C | CurrentToken], Tokens, double);
%% Backslash inside double quotes -- literal backslash for other chars
tokenize([$\\, C | Rest], CurrentToken, Tokens, double) ->
    tokenize(Rest, [C, $\\ | CurrentToken], Tokens, double);
%% Inside double quotes -- everything else is literal
tokenize([C | Rest], CurrentToken, Tokens, double) ->
    tokenize(Rest, [C | CurrentToken], Tokens, double);
%% Whitespace outside quotes -- token boundary
tokenize([C | Rest], [], Tokens, none) when C =:= $\s; C =:= $\t ->
    tokenize(Rest, [], Tokens, none);
tokenize([C | Rest], CurrentToken, Tokens, none) when C =:= $\s; C =:= $\t ->
    tokenize(Rest, [], [lists:reverse(CurrentToken) | Tokens], none);
%% Regular character outside quotes
tokenize([C | Rest], CurrentToken, Tokens, none) ->
    tokenize(Rest, [C | CurrentToken], Tokens, none).
