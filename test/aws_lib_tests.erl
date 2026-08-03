-module(aws_lib_tests).

-include_lib("eunit/include/eunit.hrl").

-include("aws_lib.hrl").

%% Test helper functions
setup() ->
    application:ensure_all_started(aws_lib),
    ok.

teardown(_) ->
    application:stop(aws_lib),
    ok.

% Helper to create test state with credentials
set_test_credentials(AccessKey, SecretKey) ->
    set_test_credentials(AccessKey, SecretKey, undefined, undefined).

set_test_credentials(AccessKey, SecretKey, SecurityToken, Expiration) ->
    Creds = #aws_credentials{
        access_key = AccessKey,
        secret_key = SecretKey,
        security_token = SecurityToken,
        expiration = Expiration
    },
    State = aws_lib:new(),
    State#aws_state{credentials = Creds}.

set_test_region(Region) ->
    aws_lib:new(Region).

init_test_() ->
    {foreach,
        fun() ->
            os:putenv("AWS_DEFAULT_REGION", "us-west-3"),
            meck:new(aws_lib_config, [passthrough]),
            setup()
        end,
        fun(_) ->
            teardown(ok),
            os:unsetenv("AWS_DEFAULT_REGION"),
            meck:unload(aws_lib_config)
        end,
        [
            {"ok", fun() ->
                State0 = set_test_region("us-west-3"),
                os:unsetenv("AWS_SESSION_TOKEN"),
                os:putenv("AWS_ACCESS_KEY_ID", "Sésame"),
                os:putenv("AWS_SECRET_ACCESS_KEY", "ouvre-toi"),
                {ok, State1} = aws_lib:refresh_credentials(State0),
                ?assertEqual(true, aws_lib:has_credentials(State1)),
                {ok, Creds} = aws_lib:get_credentials(State1),
                ?assertEqual("Sésame", Creds#aws_credentials.access_key),
                ?assertEqual("ouvre-toi", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                {ok, Region} = aws_lib:get_region(State1),
                ?assertEqual("us-west-3", Region),
                os:unsetenv("AWS_ACCESS_KEY_ID"),
                os:unsetenv("AWS_SECRET_ACCESS_KEY")
            end},
            {"error", fun() ->
                State = set_test_region("us-west-3"),
                meck:expect(aws_lib_config, credentials, fun(_) -> {error, test_result} end),
                ?assertEqual({error, test_result}, aws_lib:refresh_credentials(State)),
                meck:validate(aws_lib_config)
            end}
        ]}.

endpoint_test_() ->
    [
        {"specified", fun() ->
            Region = "us-east-3",
            Service = "dynamodb",
            Path = "/",
            Host = "localhost:32767",
            Expectation = "https://localhost:32767/",
            ?assertEqual(
                Expectation, aws_lib:endpoint(Region, Host, Service, Path)
            )
        end},
        {"unspecified", fun() ->
            os:unsetenv("AWS_ENDPOINT_URL"),
            Region = "us-east-3",
            Service = "dynamodb",
            Path = "/",
            Host = undefined,
            Expectation = "https://dynamodb.us-east-3.amazonaws.com/",
            ?assertEqual(
                Expectation, aws_lib:endpoint(Region, Host, Service, Path)
            )
        end}
    ].

%% With no explicit host, endpoint/4 honours the AWS_ENDPOINT_URL override so
%% requests can target a local development endpoint. The override's scheme, host,
%% and port are used, with the AWS request path appended, and a trailing slash on
%% the override is not doubled up with the path.
endpoint_override_test_() ->
    Region = "us-east-1",
    Service = "s3",
    Path = "/bucket/key",
    {foreach,
        fun() ->
            os:unsetenv("AWS_ENDPOINT_URL"),
            os:unsetenv("AWS_ENDPOINT_URL_S3"),
            ok
        end,
        fun(_) ->
            os:unsetenv("AWS_ENDPOINT_URL"),
            os:unsetenv("AWS_ENDPOINT_URL_S3"),
            ok
        end,
        [
            {"global override sets scheme, host, and port", fun() ->
                os:putenv("AWS_ENDPOINT_URL", "http://localhost:4566"),
                ?assertEqual(
                    "http://localhost:4566/bucket/key",
                    aws_lib:endpoint(Region, undefined, Service, Path)
                )
            end},
            {"a trailing slash on the override is not doubled", fun() ->
                os:putenv("AWS_ENDPOINT_URL", "http://localhost:4566/"),
                ?assertEqual(
                    "http://localhost:4566/bucket/key",
                    aws_lib:endpoint(Region, undefined, Service, Path)
                )
            end},
            {"an explicit host still wins over the override", fun() ->
                os:putenv("AWS_ENDPOINT_URL", "http://localhost:4566"),
                ?assertEqual(
                    "https://myhost:9/bucket/key",
                    aws_lib:endpoint(Region, "myhost:9", Service, Path)
                )
            end}
        ]}.

%% gun_open_opts/2 takes the transport verbatim (scheme-driven upstream), so the
%% resulting Gun options carry TLS or plain TCP as given.
gun_open_opts_transport_test_() ->
    [
        {"tls transport", fun() ->
            ?assertMatch(#{transport := tls}, aws_lib:gun_open_opts(tls, []))
        end},
        {"tcp transport", fun() ->
            ?assertMatch(#{transport := tcp}, aws_lib:gun_open_opts(tcp, []))
        end}
    ].

endpoint_host_test_() ->
    [
        {"dynamodb service", fun() ->
            Expectation = "dynamodb.us-west-2.amazonaws.com",
            ?assertEqual(Expectation, aws_lib:endpoint_host("us-west-2", "dynamodb"))
        end}
    ].

cn_endpoint_host_test_() ->
    [
        {"s3", fun() ->
            Expectation = "s3.cn-north-1.amazonaws.com.cn",
            ?assertEqual(Expectation, aws_lib:endpoint_host("cn-north-1", "s3"))
        end},
        {"s3", fun() ->
            Expectation = "s3.cn-northwest-1.amazonaws.com.cn",
            ?assertEqual(Expectation, aws_lib:endpoint_host("cn-northwest-1", "s3"))
        end}
    ].

%% End-to-end guard for the DescribeVolumes response path: a real DescribeVolumes
%% XML document decoded by aws_lib_xml:parse/1 must still parse into the expected
%% volumes list. This pins the shape parse_volumes_response/1 depends on, so a
%% change to the XML parser cannot silently break it.
parse_volumes_response_test_() ->
    [
        {"a two-volume response parses into a volumes list", fun() ->
            Xml =
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                "<DescribeVolumesResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">"
                "<volumeSet>"
                "<item>"
                "<volumeId>vol-1111</volumeId><size>8</size>"
                "<volumeType>gp3</volumeType><status>in-use</status>"
                "<attachmentSet><item>"
                "<device>/dev/sda1</device><status>attached</status>"
                "</item></attachmentSet>"
                "</item>"
                "<item>"
                "<volumeId>vol-2222</volumeId><size>16</size>"
                "<volumeType>gp2</volumeType><status>available</status>"
                "<attachmentSet/>"
                "</item>"
                "</volumeSet>"
                "</DescribeVolumesResponse>",
            Parsed = aws_lib_xml:parse(Xml),
            {ok, Volumes} = aws_lib:parse_volumes_response(Parsed),
            ?assertEqual(2, length(Volumes)),
            [V1, V2] = Volumes,
            ?assertEqual("vol-1111", proplists:get_value(volume_id, V1)),
            ?assertEqual("gp3", proplists:get_value(volume_type, V1)),
            ?assertEqual(
                [{device, "/dev/sda1"}, {state, "attached"}],
                proplists:get_value(attachment, V1)
            ),
            ?assertEqual("vol-2222", proplists:get_value(volume_id, V2)),
            ?assertEqual([], proplists:get_value(attachment, V2))
        end}
    ].

timeout_test_() ->
    [
        {"defaults to the API timeout when unset", fun() ->
            ?assertEqual(30000, aws_lib:get_timeout(aws_lib:new()))
        end},
        {"set_timeout/2 overrides the default", fun() ->
            {ok, State} = aws_lib:set_timeout(60000, aws_lib:new()),
            ?assertEqual(60000, aws_lib:get_timeout(State))
        end}
    ].

expired_credentials_test_() ->
    {
        foreach,
        fun() ->
            meck:new(calendar, [passthrough, unstick]),
            [calendar]
        end,
        fun meck:unload/1,
        [
            {"true", fun() ->
                Value = {{2016, 4, 1}, {12, 0, 0}},
                Expectation = true,
                meck:expect(calendar, universal_time, fun() ->
                    {{2016, 4, 1}, {12, 0, 0}}
                end),
                ?assertEqual(Expectation, aws_lib:expired_credentials(Value)),
                meck:validate(calendar)
            end},
            {"false", fun() ->
                Value = {{2016, 5, 1}, {16, 30, 0}},
                Expectation = false,
                meck:expect(calendar, universal_time, fun() ->
                    {{2016, 4, 1}, {12, 0, 0}}
                end),
                ?assertEqual(Expectation, aws_lib:expired_credentials(Value)),
                meck:validate(calendar)
            end},
            %% Within the refresh buffer window (#93): credentials that expire in
            %% 4 minutes are still valid on the clock but are treated as expired
            %% so they refresh before a request can start with them.
            {"within refresh buffer is treated as expired", fun() ->
                Now = {{2016, 4, 1}, {12, 0, 0}},
                %% Expires 240s from now, inside the 300s buffer.
                Expiration = {{2016, 4, 1}, {12, 4, 0}},
                meck:expect(calendar, universal_time, fun() -> Now end),
                ?assertEqual(true, aws_lib:expired_credentials(Expiration)),
                meck:validate(calendar)
            end},
            %% Just outside the buffer window: expires in 6 minutes, still valid.
            {"outside refresh buffer is still valid", fun() ->
                Now = {{2016, 4, 1}, {12, 0, 0}},
                Expiration = {{2016, 4, 1}, {12, 6, 0}},
                meck:expect(calendar, universal_time, fun() -> Now end),
                ?assertEqual(false, aws_lib:expired_credentials(Expiration)),
                meck:validate(calendar)
            end},
            {"undefined", fun() ->
                ?assertEqual(false, aws_lib:expired_credentials(undefined))
            end}
        ]
    }.

has_credentials_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"true", fun() ->
                State = set_test_credentials("TESTVALUE1", "SECRET"),
                ?assertEqual(true, aws_lib:has_credentials(State))
            end},
            {"false", fun() ->
                State = aws_lib:new(),
                ?assertEqual(false, aws_lib:has_credentials(State))
            end}
        ]
    }.

local_time_test_() ->
    {
        foreach,
        fun() ->
            meck:new(calendar, [passthrough, unstick]),
            [calendar]
        end,
        fun meck:unload/1,
        [
            {"value", fun() ->
                Value = {{2016, 5, 1}, {12, 0, 0}},
                meck:expect(calendar, universal_time, fun() -> Value end),
                ?assertEqual(Value, aws_lib:local_time()),
                meck:validate(calendar)
            end}
        ]
    }.

perform_request_test_() ->
    {
        foreach,
        fun() ->
            setup(),
            meck:new(gun, []),
            [gun]
        end,
        fun(Mods) ->
            teardown(ok),
            meck:unload(Mods)
        end,
        [
            {
                "Successful run",
                fun() ->
                    State0 = set_test_credentials(
                        "AKIDEXAMPLE", "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                    ),
                    {ok, State} = aws_lib:set_region("us-east-1", State0),
                    Service = "ec2",
                    Method = get,
                    Headers = [],
                    Path = "/?Action=DescribeTags&Version=2015-10-01",
                    Body = "",
                    Options = [],

                    meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                    meck:expect(gun, close, fun(_) -> ok end),
                    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                    meck:expect(
                        gun,
                        get,
                        fun(_Pid, "/?Action=DescribeTags&Version=2015-10-01", _Headers) -> nofin end
                    ),
                    meck:expect(
                        gun,
                        await,
                        fun(_Pid, _, _) ->
                            {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                        end
                    ),
                    meck:expect(
                        gun,
                        await_body,
                        fun(_Pid, _, _) -> {ok, <<"{\"pass\": true}">>} end
                    ),

                    {ok, {Headers1, Body1}, State1} = aws_lib:request(
                        Service, Method, Path, Body, Headers, Options, State
                    ),
                    ?assertEqual([{<<"content-type">>, <<"application/json">>}], Headers1),
                    ?assertEqual([{"pass", true}], Body1),
                    ?assert(is_record(State1, aws_state)),
                    meck:validate(gun)
                end
            },
            {
                "the state timeout reaches gun:await when no option is given",
                fun() ->
                    State0 = set_test_credentials(
                        "AKIDEXAMPLE", "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                    ),
                    {ok, State1} = aws_lib:set_region("us-east-1", State0),
                    {ok, State} = aws_lib:set_timeout(45000, State1),
                    Self = self(),
                    meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                    meck:expect(gun, close, fun(_) -> ok end),
                    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                    meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                    %% Capture the timeout gun:await/3 is called with.
                    meck:expect(gun, await, fun(_Pid, _, Timeout) ->
                        Self ! {await_timeout, Timeout},
                        {response, fin, 200, []}
                    end),
                    {ok, _, _} = aws_lib:request("ec2", get, "/", "", [], [], State),
                    receive
                        {await_timeout, T} -> ?assertEqual(45000, T)
                    after 1000 -> ?assert(false)
                    end,
                    meck:validate(gun)
                end
            },
            {
                "an explicit timeout option overrides the state timeout",
                fun() ->
                    State0 = set_test_credentials(
                        "AKIDEXAMPLE", "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                    ),
                    {ok, State1} = aws_lib:set_region("us-east-1", State0),
                    {ok, State} = aws_lib:set_timeout(45000, State1),
                    Self = self(),
                    meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                    meck:expect(gun, close, fun(_) -> ok end),
                    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                    meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                    meck:expect(gun, await, fun(_Pid, _, Timeout) ->
                        Self ! {await_timeout, Timeout},
                        {response, fin, 200, []}
                    end),
                    %% Options carries an explicit timeout that must win.
                    {ok, _, _} = aws_lib:request("ec2", get, "/", "", [], [{timeout, 1234}], State),
                    receive
                        {await_timeout, T} -> ?assertEqual(1234, T)
                    after 1000 -> ?assert(false)
                    end,
                    meck:validate(gun)
                end
            },
            {
                "await_body timeout is returned as a clean {error, timeout}",
                fun() ->
                    State0 = set_test_credentials(
                        "AKIDEXAMPLE", "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                    ),
                    {ok, State} = aws_lib:set_region("us-east-1", State0),

                    meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                    meck:expect(gun, close, fun(_) -> ok end),
                    meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                    meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                    meck:expect(gun, await, fun(_Pid, _, _) ->
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    end),
                    %% The headers arrive but the body read times out. This must
                    %% surface as {error, timeout}, not {error, {badmatch, _}}.
                    meck:expect(gun, await_body, fun(_Pid, _, _) -> {error, timeout} end),

                    Result = aws_lib:request("ec2", get, "/", "", [], [], State),
                    ?assertEqual({error, timeout, undefined}, Result),
                    meck:validate(gun)
                end
            }
        ]
    }.

signing_failure_retries_test_() ->
    {
        foreach,
        fun() ->
            setup(),
            meck:new(aws_lib_sign, [passthrough]),
            [aws_lib_sign]
        end,
        fun(Mods) ->
            teardown(ok),
            meck:unload(Mods)
        end,
        [
            {
                "a signing failure exhausts retries instead of crashing",
                fun() ->
                    State0 = set_test_credentials(
                        "AKIDEXAMPLE", "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                    ),
                    {ok, State} = aws_lib:set_region("us-east-1", State0),
                    %% A signing failure makes prepare_signed_request/8 return a
                    %% 2-tuple {error, Reason}; the retry loop must shape it into
                    %% the 3-tuple result it matches on rather than raising a
                    %% case_clause (issue #80 review).
                    meck:expect(aws_lib_sign, headers, fun(_Request, _BodyHash) ->
                        {error, {malformed_uri, "bad"}}
                    end),
                    Result = aws_lib:api_request_with_retries(
                        "ec2", get, "/", "", [], 1, 0, State
                    ),
                    ?assertMatch({error, {service_error, retries_exhausted}, _State}, Result),
                    meck:validate(aws_lib_sign)
                end
            }
        ]
    }.

sign_headers_test_() ->
    {
        foreach,
        fun() ->
            meck:new(calendar, [passthrough, unstick]),
            [calendar]
        end,
        fun meck:unload/1,
        [
            {"with security token", fun() ->
                Value = {{2016, 5, 1}, {12, 0, 0}},
                meck:expect(calendar, universal_time, fun() -> Value end),
                AccessKey = "AKIDEXAMPLE",
                SecretKey = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
                SecurityToken =
                    "AQoEXAMPLEH4aoAH0gNCAPyJxz4BlCFFxWNE1OPTgk5TthT+FvwqnKwRcOIfrRh3c/L",
                Region = "us-east-1",
                Service = "ec2",
                Method = get,
                Headers = [],
                Body = "",
                URI = "http://ec2.us-east-1.amazonaws.com/?Action=DescribeTags&Version=2015-10-01",
                Expectation = [
                    {"authorization",
                        "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20160501/us-east-1/ec2/aws4_request, SignedHeaders=content-length;date;host;x-amz-content-sha256;x-amz-security-token, Signature=62d10b4897f7d05e4454b75895b5e372f6c2eb6997943cd913680822e94c6999"},
                    {"content-length", "0"},
                    {"date", "20160501T120000Z"},
                    {"host", "ec2.us-east-1.amazonaws.com"},
                    {"x-amz-content-sha256",
                        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
                    {"x-amz-security-token",
                        "AQoEXAMPLEH4aoAH0gNCAPyJxz4BlCFFxWNE1OPTgk5TthT+FvwqnKwRcOIfrRh3c/L"}
                ],
                ?assertEqual(
                    {ok, Expectation},
                    aws_lib:sign_headers(
                        AccessKey,
                        SecretKey,
                        SecurityToken,
                        Region,
                        Service,
                        Method,
                        URI,
                        Headers,
                        Body,
                        undefined
                    )
                ),
                meck:validate(calendar)
            end}
        ]
    }.

api_get_request_test_() ->
    {
        foreach,
        fun() ->
            setup(),
            meck:new(gun, []),
            meck:new(aws_lib_config, []),
            %% aws_lib_config is a strict mock here; endpoint/4 consults
            %% endpoint_url/1, so it must be expected. No override: target the
            %% default AWS endpoint.
            meck:expect(aws_lib_config, endpoint_url, fun(_) -> undefined end),
            %% Stub the inter-attempt backoff sleep. These cases exercise retry
            %% BEHAVIOUR (error surfacing, retry re-entry), not delay timing --
            %% the delay arithmetic is covered by backoff_delay/3's own tests.
            %% With exponential backoff a full 5-retry exhaustion would sleep up
            %% to ~15s of real time, past eunit's 5s per-test default, so these
            %% cases would time out. Skipping the real sleep keeps them fast
            %% without changing what they assert.
            meck:new(timer, [passthrough, unstick]),
            meck:expect(timer, sleep, fun(_) -> ok end),
            [gun, aws_lib_config, timer]
        end,
        fun(Mods) ->
            teardown(ok),
            meck:unload(Mods)
        end,
        [
            {"AWS service API request succeeded", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(
                    gun,
                    get,
                    fun(_Pid, _Path, _Headers) -> nofin end
                ),
                meck:expect(
                    gun,
                    await,
                    fun(_Pid, _, _) ->
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    end
                ),
                meck:expect(
                    gun,
                    await_body,
                    fun(_Pid, _, _) -> {ok, <<"{\"data\": \"value\"}">>} end
                ),

                {ok, Body, State1} = aws_lib:api_get_request("AWS", "API", State),
                ?assertEqual([{"data", "value"}], Body),
                ?assert(is_record(State1, aws_state)),
                meck:validate(gun)
            end},
            {"AWS service API request failed - API error with persistent failure", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, spawn(fun() -> ok end)} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(
                    gun,
                    get,
                    fun(_Pid, _Path, _Headers) -> nofin end
                ),
                meck:expect(
                    gun,
                    await,
                    fun(_Pid, _, _) -> {error, "network error"} end
                ),

                %% A transport failure carries no decoded body, so retry
                %% exhaustion reports the generic reason.
                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch({error, {service_error, retries_exhausted}, _State}, Result),
                meck:validate(gun)
            end},
            {"a persistent HTTP error surfaces the decoded AWS error body", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, spawn(fun() -> ok end)} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% Every attempt returns an HTTP 400 with a decoded JSON error
                %% body; after retries are exhausted the caller must receive that
                %% decoded body under service_error, not a generic string.
                meck:expect(gun, await, fun(_Pid, _, _) ->
                    {response, nofin, 400, [{<<"content-type">>, <<"application/json">>}]}
                end),
                meck:expect(gun, await_body, fun(_Pid, _, _) ->
                    {ok, <<
                        "{\"Error\": {\"Code\": \"ThrottlingException\", "
                        "\"Message\": \"Rate exceeded\"}}"
                    >>}
                end),

                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch(
                    {error,
                        {service_error, [
                            {"Error", [
                                {"Code", "ThrottlingException"},
                                {"Message", "Rate exceeded"}
                            ]}
                        ]},
                        _State},
                    Result
                ),
                meck:validate(gun)
            end},
            {"a non-retriable 4xx returns immediately without burning retries", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, spawn(fun() -> ok end)} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% A 400 with a non-throttling error code is not retriable
                %% (issue #80): the request must be made exactly once and the
                %% decoded error returned to the caller immediately.
                meck:expect(gun, await, fun(_Pid, _, _) ->
                    {response, nofin, 400, [{<<"content-type">>, <<"application/json">>}]}
                end),
                meck:expect(gun, await_body, fun(_Pid, _, _) ->
                    {ok, <<"{\"Error\": {\"Code\": \"InvalidParameterValue\"}}">>}
                end),

                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch(
                    {error,
                        {service_error, [
                            {"Error", [{"Code", "InvalidParameterValue"}]}
                        ]},
                        _State},
                    Result
                ),
                %% Exactly one attempt: not retried.
                ?assertEqual(1, meck:num_calls(gun, await, '_')),
                meck:validate(gun)
            end},
            {"a throttling 4xx is retried rather than returned immediately", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, spawn(fun() -> ok end)} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% A 400 whose body indicates throttling is retriable: the first
                %% attempt throttles, the second succeeds.
                meck:expect(
                    gun,
                    await,
                    3,
                    meck:seq([
                        {response, nofin, 400, [{<<"content-type">>, <<"application/json">>}]},
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    ])
                ),
                meck:expect(
                    gun,
                    await_body,
                    3,
                    meck:seq([
                        {ok, <<"{\"__type\": \"ThrottlingException\"}">>},
                        {ok, <<"{\"data\": \"value\"}">>}
                    ])
                ),

                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch({ok, [{"data", "value"}], _State}, Result),
                %% Two attempts: throttling was retried.
                ?assertEqual(2, meck:num_calls(gun, await, '_')),
                meck:validate(gun)
            end},
            {"a decoded error survives a later transport failure", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, spawn(fun() -> ok end)} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% The first attempt returns a retriable HTTP 503 with a decoded
                %% error body; every later attempt fails at the transport level
                %% (no body). The 503 keeps the loop retrying so the transport
                %% failures are consumed, exercising keep_informative_error/2:
                %% retry exhaustion must still surface the decoded body from the
                %% first attempt, not the bodiless transport error.
                meck:expect(
                    gun,
                    await,
                    3,
                    meck:seq([
                        {response, nofin, 503, [{<<"content-type">>, <<"application/json">>}]},
                        {error, "network error"},
                        {error, "network error"},
                        {error, "network error"},
                        {error, "network error"}
                    ])
                ),
                meck:expect(gun, await_body, fun(_Pid, _, _) ->
                    {ok, <<"{\"Error\": {\"Code\": \"InternalError\"}}">>}
                end),

                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch(
                    {error,
                        {service_error, [
                            {"Error", [{"Code", "InternalError"}]}
                        ]},
                        _State},
                    Result
                ),
                %% All attempts are consumed: the initial 503 plus the four
                %% transport failures across the retry budget.
                ?assertEqual(?MAX_RETRIES, meck:num_calls(gun, await, '_')),
                meck:validate(gun)
            end},
            {"AWS service API request succeeded after a transient error", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                meck:expect(gun, open, fun(_, _, _) -> {ok, spawn(fun() -> ok end)} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(
                    gun,
                    get,
                    fun(_Pid, _Path, _Headers) -> nofin end
                ),

                %% meck:expect(gun, get, 3, meck:seq(
                %%             fun(_Pid, _Path, _Headers) -> {error, "network errors"} end),
                meck:expect(
                    gun,
                    await,
                    3,
                    meck:seq([
                        {error, "network error"},
                        {response, nofin, 500, [{<<"content-type">>, <<"application/json">>}]},
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    ])
                ),

                meck:expect(
                    gun,
                    await_body,
                    3,
                    meck:seq([
                        {ok, <<"{\"error\": \"server error\"}">>},
                        {ok, <<"{\"data\": \"value\"}">>}
                    ])
                ),
                {ok, Body, State1} = aws_lib:api_get_request("AWS", "API", State),
                ?assertEqual([{"data", "value"}], Body),
                ?assert(is_record(State1, aws_state)),
                meck:validate(gun)
            end},
            {"gun:open failure re-enters the retry loop rather than raising", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                %% A connection that never opens must be retried and then
                %% reported as the exhausted-retries error, not escape as a
                %% {gun_open_failed, _} exception.
                meck:expect(gun, open, fun(_, _, _) -> {error, econnrefused} end),
                meck:expect(gun, close, fun(_) -> ok end),

                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch({error, {service_error, retries_exhausted}, _State}, Result),
                meck:validate(gun)
            end},
            {"gun:await_up failure re-enters the retry loop rather than raising", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                %% The socket opens but the protocol never comes up; this too
                %% must be retried, not raised as {gun_connection_failed, _}.
                meck:expect(gun, open, fun(_, _, _) -> {ok, pid} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {error, timeout} end),

                Result = aws_lib:api_get_request("AWS", "API", State),
                ?assertMatch({error, {service_error, retries_exhausted}, _State}, Result),
                meck:validate(gun)
            end}
        ]
    }.

%% Issue #91: the retry loop reuses one connection across attempts instead of
%% opening a fresh TCP+TLS connection every time. These tests pin that
%% behaviour by counting gun:open calls, which is the whole point of the change.
connection_reuse_across_retries_test_() ->
    {
        foreach,
        fun() ->
            setup(),
            meck:new(gun, []),
            meck:new(aws_lib_config, []),
            %% aws_lib_config is a strict mock here; endpoint/4 consults
            %% endpoint_url/1, so it must be expected. No override: target the
            %% default AWS endpoint.
            meck:expect(aws_lib_config, endpoint_url, fun(_) -> undefined end),
            %% Stub the inter-attempt backoff sleep so exhausting retries does
            %% not sleep ~15s of real time and trip eunit's 5s per-test default.
            %% These cases count gun:open calls across retries; the delay itself
            %% is irrelevant here and is covered by backoff_delay/3's own tests.
            meck:new(timer, [passthrough, unstick]),
            meck:expect(timer, sleep, fun(_) -> ok end),
            [gun, aws_lib_config, timer]
        end,
        fun(Mods) ->
            teardown(ok),
            meck:unload(Mods)
        end,
        [
            {"one connection is reused across HTTP-error retries", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                %% A long-lived pid so the reuse liveness check (is_process_alive)
                %% sees the connection as alive between attempts.
                Conn = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% Two 500s (HTTP-level errors, connection stays intact) then a 200.
                meck:expect(
                    gun,
                    await,
                    3,
                    meck:seq([
                        {response, nofin, 500, [{<<"content-type">>, <<"application/json">>}]},
                        {response, nofin, 500, [{<<"content-type">>, <<"application/json">>}]},
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    ])
                ),
                meck:expect(
                    gun,
                    await_body,
                    3,
                    meck:seq([
                        {ok, <<"{\"error\": \"e\"}">>},
                        {ok, <<"{\"error\": \"e\"}">>},
                        {ok, <<"{\"data\": \"value\"}">>}
                    ])
                ),

                {ok, Body, _State1} = aws_lib:api_get_request("AWS", "API", State),
                ?assertEqual([{"data", "value"}], Body),
                %% Three attempts, but the connection was opened exactly once and
                %% reused for the two retries.
                ?assertEqual(1, meck:num_calls(gun, open, '_')),
                %% Opened once and closed once (on the terminating success).
                ?assertEqual(1, meck:num_calls(gun, close, '_')),
                Conn ! stop,
                meck:validate(gun)
            end},
            {"a transport failure reopens the connection on the next attempt", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                Conn = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% A transport error (await returns {error, _}) makes the
                %% connection unusable, so it is closed and reopened; then a 200.
                meck:expect(
                    gun,
                    await,
                    3,
                    meck:seq([
                        {error, "network error"},
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    ])
                ),
                meck:expect(
                    gun,
                    await_body,
                    fun(_Pid, _, _) -> {ok, <<"{\"data\": \"value\"}">>} end
                ),

                {ok, Body, _State1} = aws_lib:api_get_request("AWS", "API", State),
                ?assertEqual([{"data", "value"}], Body),
                %% One transport failure -> a second open (reopen); two total.
                ?assertEqual(2, meck:num_calls(gun, open, '_')),
                Conn ! stop,
                meck:validate(gun)
            end},
            {"a connection that died between attempts is detected and reopened", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                Conn = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% Attempt 1: HTTP 500 (connection stays intact, carried forward).
                %% Attempt 2: the carried connection has since died, which gun
                %% surfaces as {error, {down, _}} from await's process monitor --
                %% a transport failure that closes and reopens. Attempt 3: 200 on
                %% the reopened connection.
                meck:expect(
                    gun,
                    await,
                    3,
                    meck:seq([
                        {response, nofin, 500, [{<<"content-type">>, <<"application/json">>}]},
                        {error, {down, noproc}},
                        {response, nofin, 200, [{<<"content-type">>, <<"application/json">>}]}
                    ])
                ),
                meck:expect(
                    gun,
                    await_body,
                    3,
                    meck:seq([
                        {ok, <<"{\"error\": \"e\"}">>},
                        {ok, <<"{\"data\": \"value\"}">>}
                    ])
                ),

                {ok, Body, _State1} = aws_lib:api_get_request("AWS", "API", State),
                ?assertEqual([{"data", "value"}], Body),
                %% Opened once up front; the {down, _} on attempt 2 forced exactly
                %% one reopen -- two opens total. The HTTP 500 on attempt 1 did
                %% NOT reopen (that connection was still usable).
                ?assertEqual(2, meck:num_calls(gun, open, '_')),
                Conn ! stop,
                meck:validate(gun)
            end},
            {"retry => 0 is passed to the reused connection's open options", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State} = aws_lib:set_region("us-east-1", State0),

                Self = self(),
                Conn = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                meck:expect(gun, open, fun(_, _, Opts) ->
                    Self ! {open_opts, Opts},
                    {ok, Conn}
                end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                meck:expect(gun, await, fun(_Pid, _, _) -> {response, fin, 200, []} end),

                {ok, _, _} = aws_lib:api_get_request("AWS", "API", State),
                receive
                    {open_opts, Opts} -> ?assertEqual(0, maps:get(retry, Opts))
                after 1000 -> ?assert(false)
                end,
                Conn ! stop,
                meck:validate(gun)
            end},
            {"a not_retriable 4xx in reuse mode hands the connection back", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State1} = aws_lib:set_region("us-east-1", State0),
                %% Reuse mode: the connection after a cleanly completed 4xx is as
                %% usable as after a 2xx, so it must be handed back in the state's
                %% reuse_conn rather than closed (issue #80 review).
                State = aws_lib:enable_connection_reuse(State1),

                Conn = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                meck:expect(gun, open, fun(_, _, _) -> {ok, Conn} end),
                meck:expect(gun, close, fun(_) -> ok end),
                meck:expect(gun, await_up, fun(_, _) -> {ok, protocol} end),
                meck:expect(gun, get, fun(_Pid, _Path, _Headers) -> nofin end),
                %% A 400 with a non-throttling error code is not retriable.
                meck:expect(gun, await, fun(_Pid, _, _) ->
                    {response, nofin, 400, [{<<"content-type">>, <<"application/json">>}]}
                end),
                meck:expect(gun, await_body, fun(_Pid, _, _) ->
                    {ok, <<"{\"Error\": {\"Code\": \"InvalidParameterValue\"}}">>}
                end),

                {error, {service_error, _}, StateOut} = aws_lib:api_get_request(
                    "AWS", "API", State
                ),
                %% Exactly one attempt: not retried.
                ?assertEqual(1, meck:num_calls(gun, await, '_')),
                %% The connection was handed back, not closed.
                ?assertEqual(0, meck:num_calls(gun, close, '_')),
                ?assertMatch({Conn, _Host, _Port}, StateOut#aws_state.reuse_conn),
                Conn ! stop,
                meck:validate(gun)
            end}
        ]
    }.

ensure_credentials_valid_test_() ->
    {
        foreach,
        fun() ->
            setup(),
            meck:new(aws_lib_config, []),
            [aws_lib_config]
        end,
        fun(Mods) ->
            teardown(ok),
            meck:unload(Mods)
        end,
        [
            {"expired credentials are refreshed", fun() ->
                State0 = set_test_credentials("ExpiredKey", "ExpiredAccessKey", undefined, {
                    {2016, 4, 1}, {12, 0, 0}
                }),
                {ok, State1} = aws_lib:set_region("us-east-1", State0),

                % Mock config to return new credentials when refresh is called
                NewCreds = #aws_credentials{
                    access_key = "NewKey",
                    secret_key = "NewAccessKey",
                    security_token = undefined,
                    expiration = {{3016, 4, 1}, {12, 0, 0}}
                },
                meck:expect(
                    aws_lib_config,
                    credentials,
                    fun(Config) ->
                        {ok, NewCreds, Config}
                    end
                ),

                {ok, State2} = aws_lib:ensure_credentials_valid(State1),

                {ok, Creds} = aws_lib:get_credentials(State2),
                ?assertEqual("NewKey", Creds#aws_credentials.access_key),
                ?assertEqual("NewAccessKey", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                {ok, Region} = aws_lib:get_region(State2),
                ?assertEqual("us-east-1", Region),
                meck:validate(aws_lib_config)
            end},
            {"valid credentials are returned", fun() ->
                State0 = set_test_credentials("GoodKey", "GoodAccessKey", undefined, {
                    {3016, 4, 1}, {12, 0, 0}
                }),
                {ok, State1} = aws_lib:set_region("us-east-1", State0),

                {ok, State2} = aws_lib:ensure_credentials_valid(State1),

                {ok, Creds} = aws_lib:get_credentials(State2),
                ?assertEqual("GoodKey", Creds#aws_credentials.access_key),
                ?assertEqual("GoodAccessKey", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                {ok, Region} = aws_lib:get_region(State2),
                ?assertEqual("us-east-1", Region),
                meck:validate(aws_lib_config)
            end},
            {"load credentials if missing", fun() ->
                State0 = aws_lib:new("us-east-1"),

                NewCreds = #aws_credentials{
                    access_key = "GoodKey",
                    secret_key = "GoodAccessKey",
                    security_token = undefined,
                    expiration = {{3016, 4, 1}, {12, 0, 0}}
                },
                meck:expect(
                    aws_lib_config,
                    credentials,
                    fun(Config) ->
                        {ok, NewCreds, Config}
                    end
                ),

                {ok, State1} = aws_lib:ensure_credentials_valid(State0),

                {ok, Creds} = aws_lib:get_credentials(State1),
                ?assertEqual("GoodKey", Creds#aws_credentials.access_key),
                ?assertEqual("GoodAccessKey", Creds#aws_credentials.secret_key),
                ?assertEqual(undefined, Creds#aws_credentials.security_token),
                {ok, Region} = aws_lib:get_region(State1),
                ?assertEqual("us-east-1", Region),
                meck:validate(aws_lib_config)
            end}
        ]
    }.

expired_imdsv2_token_test_() ->
    [
        {"imdsv2 token is valid", fun() ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Imdsv2Token = #imdsv2token{token = "value", expiration = Now + 100},
            ?assertEqual(false, aws_lib:expired_imdsv2_token(Imdsv2Token))
        end},
        {"imdsv2 token is expired", fun() ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Imdsv2Token = #imdsv2token{token = "value", expiration = Now - 100},
            ?assertEqual(true, aws_lib:expired_imdsv2_token(Imdsv2Token))
        end},
        {"imdsv2 token is not yet initialized", fun() ->
            ?assertEqual(true, aws_lib:expired_imdsv2_token(undefined))
        end},
        {"imdsv2 token is undefined", fun() ->
            Imdsv2Token = #imdsv2token{token = undefined, expiration = undefined},
            ?assertEqual(true, aws_lib:expired_imdsv2_token(Imdsv2Token))
        end}
    ].

%% ----------------------------------------------------------------------------
%% backoff_delay/3 -- exponential backoff with equal jitter (issue #81)
%% ----------------------------------------------------------------------------
backoff_delay_test_() ->
    [
        {"attempt 0 with base 500 is in range [250, 500]", fun() ->
            Results = [aws_lib:backoff_delay(0, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 250 andalso V =< 500)
                end,
                Results
            )
        end},
        {"attempt 1 with base 500 is in range [500, 1000]", fun() ->
            Results = [aws_lib:backoff_delay(1, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 500 andalso V =< 1000)
                end,
                Results
            )
        end},
        {"attempt 2 with base 500 is in range [1000, 2000]", fun() ->
            Results = [aws_lib:backoff_delay(2, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 1000 andalso V =< 2000)
                end,
                Results
            )
        end},
        {"attempt 3 with base 500 is in range [2000, 4000]", fun() ->
            Results = [aws_lib:backoff_delay(3, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 2000 andalso V =< 4000)
                end,
                Results
            )
        end},
        {"attempt 4 with base 500 is in range [4000, 8000]", fun() ->
            Results = [aws_lib:backoff_delay(4, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 4000 andalso V =< 8000)
                end,
                Results
            )
        end},
        {"attempt 5 with base 500 is capped in range [5000, 10000]", fun() ->
            Results = [aws_lib:backoff_delay(5, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 5000 andalso V =< 10000)
                end,
                Results
            )
        end},
        {"attempt 10 is capped in range [5000, 10000]", fun() ->
            Results = [aws_lib:backoff_delay(10, 500, 10000) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 5000 andalso V =< 10000)
                end,
                Results
            )
        end},
        {"boundary: base=1 cap=1 does not crash", fun() ->
            V = aws_lib:backoff_delay(0, 1, 1),
            ?assert(V >= 1)
        end},
        {"boundary: base=0 cap=0 does not crash (returns >= 1 due to guard)", fun() ->
            V = aws_lib:backoff_delay(0, 0, 0),
            ?assert(V >= 1)
        end},
        {"statistical spread: jitter produces varying results", fun() ->
            Results = [aws_lib:backoff_delay(2, 500, 10000) || _ <- lists:seq(1, 100)],
            Unique = lists:usort(Results),
            %% With 100 samples from a range of 1000 values, we expect more than
            %% a single distinct value. If all are identical, jitter is broken.
            ?assert(length(Unique) > 1)
        end},
        {"monotonic growth: average delay increases with attempt (up to cap)", fun() ->
            Avg = fun(Attempt) ->
                Vals = [aws_lib:backoff_delay(Attempt, 500, 10000) || _ <- lists:seq(1, 200)],
                lists:sum(Vals) / length(Vals)
            end,
            Avg0 = Avg(0),
            Avg1 = Avg(1),
            Avg2 = Avg(2),
            Avg3 = Avg(3),
            ?assert(Avg1 >= Avg0),
            ?assert(Avg2 >= Avg1),
            ?assert(Avg3 >= Avg2)
        end},
        {"cap less than base clips delay to cap range", fun() ->
            Results = [aws_lib:backoff_delay(0, 5000, 100) || _ <- lists:seq(1, 50)],
            lists:foreach(
                fun(V) ->
                    ?assert(V >= 51 andalso V =< 100)
                end,
                Results
            )
        end}
    ].
