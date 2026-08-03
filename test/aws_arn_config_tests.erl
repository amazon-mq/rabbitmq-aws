%% Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%% SPDX-License-Identifier: Apache-2.0
%% vim:ft=erlang:
%% -*- mode: erlang; -*-

-module(aws_arn_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aws_lib.hrl").

%% A handler module name used by the boot-path tests. The functions are
%% installed with meck (non_strict) per test, so the module itself need not
%% exist on disk.
-define(HANDLER, aws_arn_config_test_handler).

parse_arn_s3_test() ->
    Arn = "arn:aws:s3:::private-ca-42/cacertfile.pem",
    {ok, Parsed} = aws_arn_util:parse_arn(Arn),
    ?assertEqual("aws", maps:get(partition, Parsed)),
    ?assertEqual("s3", maps:get(service, Parsed)),
    ?assertEqual("", maps:get(region, Parsed)),
    ?assertEqual("", maps:get(account, Parsed)),
    ?assertEqual("private-ca-42/cacertfile.pem", maps:get(resource, Parsed)).

parse_arn_s3_nested_path_test() ->
    Arn = "arn:aws:s3:::my-bucket/path/to/cert.pem",
    {ok, Parsed} = aws_arn_util:parse_arn(Arn),
    ?assertEqual("my-bucket/path/to/cert.pem", maps:get(resource, Parsed)).

parse_arn_invalid_test() ->
    ?assertMatch({error, {invalid_arn_format, _}}, aws_arn_util:parse_arn("invalid")).

parse_arn_empty_test() ->
    ?assertMatch({error, {invalid_arn_format, _}}, aws_arn_util:parse_arn("")).

parse_arn_incomplete_test() ->
    ?assertMatch({error, {invalid_arn_format, _}}, aws_arn_util:parse_arn("arn:aws:s3")).

%% A cacertfile path that is guaranteed NOT to exist. replace/5 reads the
%% configured cacertfile and prepends its decoded certs to the resolved cacerts
%% (aws_arn_env:maybe_add_cacertfile_to_cacerts/2), so this case only holds while
%% the path is absent. The literal "/tmp/ca.pem" this used to name is shared,
%% machine-global state: anything on the host creating that file made the test
%% fail with an extra leading cert, which looked like an unrelated flake. Derive
%% a unique path per run instead, and assert the precondition rather than
%% assuming it.
absent_cacertfile_path() ->
    filename:join(unique_tmp_dir(), "absent-ca.pem").

unique_tmp_dir() ->
    filename:join(
        "/tmp",
        "aws_arn_config_tests-" ++ integer_to_list(erlang:unique_integer([positive]))
    ).

replace_in_env_test() ->
    ExpectedOtherKeyValue = "value",
    Expected = [<<"cacertdata">>],
    AbsentPath = absent_cacertfile_path(),
    ?assertNot(filelib:is_file(AbsentPath)),
    ok = application:set_env(
        rabbitmq_auth_backend_oauth2,
        key_config,
        [{other_key, ExpectedOtherKeyValue}, {cacertfile, AbsentPath}]
    ),

    ok = aws_arn_env:replace(
        rabbitmq_auth_backend_oauth2, key_config, cacertfile, cacerts, {ok, Expected}
    ),

    {ok, KeyConfig} = application:get_env(rabbitmq_auth_backend_oauth2, key_config),
    ?assertMatch(Expected, proplists:get_value(cacerts, KeyConfig)),
    ?assertMatch(ExpectedOtherKeyValue, proplists:get_value(other_key, KeyConfig)).

%% The companion case the suite was missing. When the configured cacertfile DOES
%% exist its certs are prepended to the resolved ARN material -- exactly the
%% behaviour that silently broke replace_in_env_test/0 whenever a stray
%% /tmp/ca.pem existed. Pin it deliberately, using a file this test owns.
replace_in_env_prepends_existing_cacertfile_test() ->
    Dir = unique_tmp_dir(),
    ok = filelib:ensure_path(Dir),
    CacertFile = filename:join(Dir, "ca.pem"),
    try
        case write_self_signed_cert(CacertFile) of
            {ok, [FileDer]} ->
                Resolved = [<<"cacertdata">>],
                ok = application:set_env(
                    rabbitmq_auth_backend_oauth2,
                    key_config,
                    [{cacertfile, CacertFile}]
                ),

                ok = aws_arn_env:replace(
                    rabbitmq_auth_backend_oauth2,
                    key_config,
                    cacertfile,
                    cacerts,
                    {ok, Resolved}
                ),

                {ok, KeyConfig} = application:get_env(
                    rabbitmq_auth_backend_oauth2, key_config
                ),
                %% The file's cert comes first, then the resolved ARN material.
                ?assertEqual([FileDer | Resolved], proplists:get_value(cacerts, KeyConfig)),
                %% cacertfile is consumed, not left alongside cacerts.
                ?assertEqual(undefined, proplists:get_value(cacertfile, KeyConfig));
            {error, Reason} ->
                %% openssl is unavailable in this environment. Skip rather than
                %% fail: the absent-file case above still covers the regression
                %% that motivated this suite change.
                ?debugFmt("skipping prepend case, no cert material: ~p", [Reason]),
                ok
        end
    after
        _ = file:delete(CacertFile),
        _ = file:del_dir(Dir)
    end.

%% Write a throwaway self-signed cert to Path, returning the DER list that
%% aws_pem_util:decode_data/1 yields for it, so the expected value is derived
%% from the same decoder production uses rather than hardcoded.
write_self_signed_cert(Path) ->
    KeyPath = Path ++ ".key",
    Cmd = lists:flatten(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s "
            "-days 1 -nodes -subj /CN=aws-arn-config-tests 2>/dev/null",
            [KeyPath, Path]
        )
    ),
    _ = os:cmd(Cmd),
    _ = file:delete(KeyPath),
    case file:read_file(Path) of
        {ok, Pem} -> aws_pem_util:decode_data(Pem);
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Boot-path orchestration: assume-role + ARN-handler state threading
%%
%% process_arns/0 seeds an empty aws_lib:aws_state() and threads it through
%% maybe_assume_role/1, then through run_arn_handlers/2 (one resolve_arn/2
%% per ARN). These cases mock aws_iam:assume_role/2 and
%% aws_arn_util:resolve_arn/2 -- the only external calls -- so the pure
%% orchestration runs without a broker or AWS. The opaque state is observed
%% via the credentials it carries (aws_lib:get_credentials/1).
%%--------------------------------------------------------------------

boot_path_test_() ->
    {foreach, fun boot_setup/0, fun boot_teardown/1, [
        fun assume_role_not_configured_threads_state/0,
        fun assume_role_success_threads_assumed_credentials/0,
        fun assume_role_failure_short_circuits/0,
        fun resolved_state_propagates_across_arns/0,
        fun resolve_arn_error_short_circuits/0,
        fun self_resolving_handler_receives_state/0,
        fun self_resolving_handler_state_propagates/0,
        fun self_resolving_handler_error_has_context/0
    ]}.

boot_setup() ->
    ok = meck:new(aws_iam, [no_link]),
    ok = meck:new(aws_arn_util, [passthrough, no_link]),
    ok = meck:new(?HANDLER, [non_strict, no_link]),
    ok.

boot_teardown(_) ->
    catch meck:unload(?HANDLER),
    catch meck:unload(aws_arn_util),
    catch meck:unload(aws_iam),
    ok.

%% A state carrying these credentials is distinguishable, via
%% aws_lib:get_credentials/1, from the credential-free aws_lib:new().
-define(ASSUMED_KEY, "assumed-access-key").
-define(RESOLVE_KEY, "resolved-access-key").

assumed_state() ->
    {ok, S} = aws_lib:set_credentials(?ASSUMED_KEY, "secret", "token", aws_lib:new()),
    S.

access_key(State) ->
    case aws_lib:get_credentials(State) of
        {ok, #aws_credentials{access_key = Key}} -> Key;
        {error, undefined} -> undefined
    end.

%% No assume_role_arn configured: maybe_assume_role returns
%% {ok, not_assumed, State}, the seeded empty state is threaded onward, and
%% handlers run with no credentials. assume_role is never called.
assume_role_not_configured_threads_state() ->
    Self = self(),
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
        Self ! {resolved_state_key, access_key(State)},
        {ok, <<"data">>, State}
    end),
    ok = meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ArnConfig = [{arns, [{?HANDLER, "arn:aws:s3:::b/k", some_key, [some_key, sub_key]}]}],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertEqual({ok, {iam_role_result, not_assumed}}, Result),
    ?assertEqual(0, meck:num_calls(aws_iam, assume_role, '_')),
    ?assertEqual(undefined, flush_resolved_state_key()).

%% assume_role succeeds and returns a state carrying assumed credentials;
%% that state -- not the seeded empty one -- must reach the ARN handlers.
assume_role_success_threads_assumed_credentials() ->
    Self = self(),
    ok = meck:expect(aws_iam, assume_role, fun(_RoleArn, _State) ->
        {ok, assumed_state()}
    end),
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
        Self ! {resolved_state_key, access_key(State)},
        {ok, <<"data">>, State}
    end),
    ok = meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/r"},
        {arns, [{?HANDLER, "arn:aws:s3:::b/k", some_key, [some_key, sub_key]}]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertEqual({ok, {iam_role_result, assumed}}, Result),
    ?assertEqual(?ASSUMED_KEY, flush_resolved_state_key()).

%% assume_role fails: process_arns never reaches the handlers and reports
%% the failure with not_assumed.
assume_role_failure_short_circuits() ->
    ok = meck:expect(aws_iam, assume_role, fun(_RoleArn, _State) ->
        {error, boom}
    end),
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {ok, <<"data">>, State} end),
    ok = meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/r"},
        {arns, [{?HANDLER, "arn:aws:s3:::b/k", some_key, [some_key, sub_key]}]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertMatch(
        {error, {handle_assume_role, {error, {assume_role_failed, boom}}},
            {iam_role_result, not_assumed}},
        Result
    ),
    ?assertEqual(0, meck:num_calls(aws_arn_util, resolve_arn, '_')).

%% The updated state returned by resolve_arn/2 for ARN N must be the state
%% passed to resolve_arn/2 for ARN N+1 (run_arn_handlers threads State1, not
%% the original State). The first resolution stamps credentials onto the
%% threaded state; the second must observe them.
resolved_state_propagates_across_arns() ->
    Self = self(),
    ok = meck:expect(aws_arn_util, resolve_arn, fun(Arn, State) ->
        Self ! {seen, Arn, access_key(State)},
        case Arn of
            "arn:aws:s3:::b/first" ->
                {ok, S} = aws_lib:set_credentials(?RESOLVE_KEY, "secret", "token", State),
                {ok, <<"data1">>, S};
            _ ->
                {ok, <<"data2">>, State}
        end
    end),
    ok = meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ArnConfig = [
        {arns, [
            {?HANDLER, "arn:aws:s3:::b/first", key1, [key1, sub1]},
            {?HANDLER, "arn:aws:s3:::b/second", key2, [key2, sub2]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertEqual({ok, {iam_role_result, not_assumed}}, Result),
    %% First ARN sees the seeded empty state; second sees the credentials the
    %% first resolution stamped on, proving State1 propagated.
    ?assertEqual({"arn:aws:s3:::b/first", undefined}, flush_seen()),
    ?assertEqual({"arn:aws:s3:::b/second", ?RESOLVE_KEY}, flush_seen()).

%% A resolve_arn/2 error stops the handler chain at that ARN: the remaining
%% ARN is never resolved and the error is wrapped for the failing key.
resolve_arn_error_short_circuits() ->
    ok = meck:expect(aws_arn_util, resolve_arn, fun(Arn, State) ->
        case Arn of
            "arn:aws:s3:::b/first" -> {error, not_found, State};
            _ -> erlang:error(second_arn_should_not_resolve)
        end
    end),
    ok = meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ArnConfig = [
        {arns, [
            {?HANDLER, "arn:aws:s3:::b/first", first_key, [first_key, sub1]},
            {?HANDLER, "arn:aws:s3:::b/second", second_key, [second_key, sub2]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertMatch(
        {error, {_ErrMsg, {error, not_found}}, {iam_role_result, not_assumed}},
        Result
    ),
    ?assertEqual(1, meck:num_calls(aws_arn_util, resolve_arn, '_')),
    ?assertEqual(0, meck:num_calls(?HANDLER, run, '_')).

%% The self-resolving handler (Arn = undefined) is invoked as
%% Mod:run(Args ++ [State]); it resolves its own ARNs, so the threaded state
%% must arrive as the trailing argument and it returns {ok, State1}.
self_resolving_handler_receives_state() ->
    Self = self(),
    %% run/4: Args is [Key, ConfigKey, Map] (3 elements), State appended.
    ok = meck:expect(?HANDLER, run, fun(_Key, _ConfigKey, _Map, State) ->
        Self ! {self_resolving_state, access_key(State)},
        {ok, State}
    end),
    ArnConfig = [
        {arns, [{?HANDLER, undefined, the_key, [the_key, providers, #{<<"0">> => <<"arn">>}]}]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertEqual({ok, {iam_role_result, not_assumed}}, Result),
    %% No assume role configured, so the trailing state is the seeded empty one.
    receive
        {self_resolving_state, Key} -> ?assertEqual(undefined, Key)
    after 1000 -> erlang:error(handler_not_called)
    end.

%% The {ok, State1} a self-resolving handler returns must be threaded to the
%% next handler, so credentials it refreshed are visible downstream. The
%% self-resolving handler stamps credentials onto its returned state; the
%% following regular ARN handler must observe them.
self_resolving_handler_state_propagates() ->
    Self = self(),
    ok = meck:expect(?HANDLER, run, fun(_Key, _ConfigKey, _Map, State) ->
        {ok, S} = aws_lib:set_credentials(?RESOLVE_KEY, "secret", "token", State),
        {ok, S}
    end),
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) ->
        Self ! {next_handler_key, access_key(State)},
        {ok, <<"data">>, State}
    end),
    ok = meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ArnConfig = [
        {arns, [
            {?HANDLER, undefined, the_key, [the_key, providers, #{<<"0">> => <<"arn">>}]},
            {?HANDLER, "arn:aws:s3:::b/k", some_key, [some_key, sub_key]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertEqual({ok, {iam_role_result, not_assumed}}, Result),
    receive
        {next_handler_key, Key} -> ?assertEqual(?RESOLVE_KEY, Key)
    after 1000 -> erlang:error(next_handler_not_called)
    end.

%% A resolve failure inside the real oauth2 handler is wrapped with provider
%% context (the {error, {BinaryMsg, OrigError}} shape the regular handler path
%% produces) rather than leaking a bare, context-free error. Exercises the real
%% aws_arn_config_oauth2:run/4, mocking only the ARN resolution it calls.
self_resolving_handler_error_has_context() ->
    ok = meck:expect(aws_arn_util, resolve_arn, fun(_Arn, State) -> {error, not_found, State} end),
    Result = aws_arn_config_oauth2:run(
        the_key, providers_https_cacertfile, #{<<"0">> => <<"arn:x">>}, aws_lib:new()
    ),
    ?assertMatch({error, {Msg, {error, not_found}}} when is_binary(Msg), Result),
    {error, {Msg, _}} = Result,
    %% The message names the failing provider key and ARN.
    ?assert(binary:match(Msg, <<"oauth2 provider">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"arn:x">>) =/= nomatch).

flush_resolved_state_key() ->
    receive
        {resolved_state_key, Key} -> Key
    after 1000 -> erlang:error(resolve_arn_not_called)
    end.

flush_seen() ->
    receive
        {seen, Arn, Key} -> {Arn, Key}
    after 1000 -> erlang:error(resolve_arn_not_called)
    end.

%%--------------------------------------------------------------------
%% Connection reuse across the boot ARN-resolution pass (issue #107).
%%
%% These exercise the full path from process_arn_config through
%% api_get_request down to gun, counting gun:open calls to prove that
%% same-host ARNs reuse one connection. gun is mocked; no real network.
%%--------------------------------------------------------------------

connection_reuse_boot_test_() ->
    {foreach, fun conn_reuse_setup/0, fun conn_reuse_teardown/1, [
        fun same_host_arns_reuse_one_connection/0,
        fun different_host_arns_reopen/0,
        fun connection_closed_at_end_of_pass/0
    ]}.

conn_reuse_setup() ->
    ok = meck:new(gun, []),
    ok = meck:new(aws_iam, [no_link]),
    ok = meck:new(aws_lib_config, [passthrough]),
    ok = meck:new(?HANDLER, [non_strict, no_link]),
    %% Provide valid credentials so ensure_credentials_valid does not hit IMDS.
    meck:expect(aws_lib_config, credentials, fun(Config) ->
        Creds = #aws_credentials{
            access_key = "AKID",
            secret_key = "SECRET",
            security_token = undefined,
            expiration = {{3016, 1, 1}, {0, 0, 0}}
        },
        {ok, Creds, Config}
    end),
    %% Resolve the region deterministically. Without this, do_refresh_credentials
    %% resolves an undefined region via aws_lib_config:region/1, which falls
    %% through to the EC2 metadata service when no region is configured in the
    %% environment. The mocked gun then returns the response body as the
    %% availability zone, corrupting the endpoint host. This is host-dependent:
    %% it passes where ~/.aws/config supplies a region but fails in CI where none
    %% is set.
    meck:expect(aws_lib_config, region, fun(Config) ->
        {ok, "us-east-1", Config}
    end),
    %% Assume-role succeeds trivially.
    meck:expect(aws_iam, assume_role, fun(_RoleArn, State) -> {ok, State} end),
    %% Handler always succeeds.
    meck:expect(?HANDLER, run, fun(_Data, _Key, _Sub) -> ok end),
    ok.

conn_reuse_teardown(_) ->
    catch meck:unload(?HANDLER),
    catch meck:unload(gun),
    catch meck:unload(aws_iam),
    catch meck:unload(aws_lib_config),
    ok.

%% Two S3 ARNs in the same region hit the same host (s3.us-east-1.amazonaws.com).
%% The boot pass should open one connection and reuse it for both resolves.
same_host_arns_reuse_one_connection() ->
    Conn = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
    meck:expect(gun, close, fun(_) -> ok end),
    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
    meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
    meck:expect(gun, await, fun(_, _, _) ->
        {response, nofin, 200, [{<<"content-type">>, <<"application/xml">>}]}
    end),
    meck:expect(gun, await_body, fun(_, _, _) ->
        {ok, <<"<data>cert-pem</data>">>}
    end),
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/r"},
        {arns, [
            {?HANDLER, "arn:aws:s3:::bucket/cert1.pem", ssl_cacertfile, [ssl_options, cacertfile]},
            {?HANDLER, "arn:aws:s3:::bucket/cert2.pem", ssl_certfile, [ssl_options, certfile]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertMatch({ok, {iam_role_result, assumed}}, Result),
    ?assertEqual(1, meck:num_calls(gun, open, '_')),
    Conn ! stop.

%% An S3 ARN and a Secrets Manager ARN hit different hosts (s3.us-east-1 vs
%% secretsmanager.us-east-1). The pass should close the first connection and
%% open a second for the different host.
different_host_arns_reopen() ->
    Conn = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
    meck:expect(gun, close, fun(_) -> ok end),
    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
    meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
    meck:expect(gun, post, fun(_, _, _, _, _) -> stream_ref end),
    %% Secrets Manager replies with JSON; maybe_decode_body decodes it into the
    %% string-keyed proplist aws_sms:find_secret/1 matches (issue #131 fixed the
    %% earlier double-decode, so aws_sms no longer decodes it a second time).
    meck:expect(gun, await, fun(_, _, _) ->
        {response, nofin, 200, [{<<"content-type">>, <<"application/x-amz-json-1.1">>}]}
    end),
    meck:expect(gun, await_body, fun(_, _, _) ->
        {ok, <<"{\"SecretString\": \"secret-value\"}">>}
    end),
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/r"},
        {arns, [
            {?HANDLER, "arn:aws:s3:::bucket/cert.pem", ssl_cacertfile, [ssl_options, cacertfile]},
            {?HANDLER, "arn:aws:secretsmanager:us-east-1:123456789012:secret:key", ssl_keyfile, [
                ssl_options, keyfile
            ]}
        ]}
    ],
    Result = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertMatch({ok, {iam_role_result, assumed}}, Result),
    %% Two different hosts -> two opens.
    ?assertEqual(2, meck:num_calls(gun, open, '_')),
    %% Two closes: ensure_open closes the S3 connection when it switches to the
    %% Secrets Manager host, and the end-of-pass close tears the second one down.
    ?assertEqual(2, meck:num_calls(gun, close, '_')),
    Conn ! stop.

%% The connection is closed at the end of the pass (not leaked).
connection_closed_at_end_of_pass() ->
    Conn = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
    meck:expect(gun, close, fun(_) -> ok end),
    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
    meck:expect(gun, get, fun(_, _, _) -> stream_ref end),
    meck:expect(gun, await, fun(_, _, _) ->
        {response, nofin, 200, [{<<"content-type">>, <<"application/xml">>}]}
    end),
    meck:expect(gun, await_body, fun(_, _, _) ->
        {ok, <<"<data>cert-pem</data>">>}
    end),
    ArnConfig = [
        {assume_role_arn, "arn:aws:iam::123456789012:role/r"},
        {arns, [
            {?HANDLER, "arn:aws:s3:::bucket/cert.pem", ssl_cacertfile, [ssl_options, cacertfile]}
        ]}
    ],
    _ = aws_arn_config:process_arn_config({handle_env_arn_config, {ok, ArnConfig}}),
    ?assertEqual(1, meck:num_calls(gun, close, '_')),
    Conn ! stop.
