%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% @doc aws_lib client library
%% @end
%% ====================================================================
-module(aws_lib).

%% API exports
-export([
    new/0, new/1,
    get_region/1,
    set_region/2,
    get_timeout/1,
    set_timeout/2,
    get_credentials/1,
    get/3, get/4, get/5,
    put/5, put/6,
    post/5, post/6,
    refresh_credentials/1,
    request/6, request/7, request/8,
    set_credentials/3,
    set_credentials/4,
    has_credentials/1,
    ensure_credentials_valid/1,
    ensure_imdsv2_token_valid/1,
    expired_imdsv2_token/1,
    local_time/0,
    api_get_request/3,
    api_post_request/5,
    endpoint/4,
    sign_headers/10,
    instance_volumes/1,
    enable_connection_reuse/1,
    close_reuse_connection/1
]).

-export_type([aws_state/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("aws_lib.hrl").
-include_lib("kernel/include/logger.hrl").

-opaque aws_state() :: #aws_state{}.

%%====================================================================
%% State construction and accessors
%%====================================================================

-spec new() -> aws_state().
%% @doc Create a new AWS state with undefined credentials and default config.
%% @end
new() ->
    #aws_state{credentials = undefined, config = #aws_config{}}.

-spec new(Region :: region()) -> aws_state().
%% @doc Create a new AWS state with the specified region.
%% @end
new(Region) ->
    #aws_state{credentials = undefined, config = #aws_config{region = Region}}.

-spec get_region(State :: aws_state()) -> {ok, region()} | {error, undefined}.
%% @doc Get the region from the state.
%% @end
get_region(#aws_state{config = #aws_config{region = undefined}}) ->
    {error, undefined};
get_region(#aws_state{config = #aws_config{region = Region}}) ->
    {ok, Region}.

-spec set_region(Region :: region(), State :: aws_state()) -> {ok, aws_state()}.
%% @doc Set the region in the state.
%% @end
set_region(Region, State = #aws_state{config = Config}) ->
    {ok, State#aws_state{config = Config#aws_config{region = Region}}}.

-spec get_timeout(State :: aws_state()) -> timeout().
%% @doc Get the AWS API request timeout (ms) from the state, falling back to
%% ?DEFAULT_API_TIMEOUT when the state does not set one.
%% @end
get_timeout(#aws_state{config = #aws_config{timeout = undefined}}) ->
    ?DEFAULT_API_TIMEOUT;
get_timeout(#aws_state{config = #aws_config{timeout = Timeout}}) ->
    Timeout.

-spec set_timeout(Timeout :: timeout(), State :: aws_state()) -> {ok, aws_state()}.
%% @doc Set the AWS API request timeout (ms) in the state. Applied to requests
%% that do not pass an explicit `timeout' option.
%% @end
set_timeout(Timeout, State = #aws_state{config = Config}) ->
    {ok, State#aws_state{config = Config#aws_config{timeout = Timeout}}}.

-spec get_credentials(State :: aws_state()) -> {ok, aws_credentials()} | {error, undefined}.
%% @doc Get the credentials from the state.
%% @end
get_credentials(#aws_state{credentials = undefined}) ->
    {error, undefined};
get_credentials(#aws_state{credentials = Creds}) ->
    {ok, Creds}.

-spec enable_connection_reuse(State :: aws_state()) -> aws_state().
%% @doc Arm the state for connection reuse: subsequent api_get_request /
%% api_post_request calls that target the same Host:Port reuse a single
%% connection across calls instead of opening a fresh one each time. The caller
%% MUST call close_reuse_connection/1 when the unit of work is complete.
%% @end
enable_connection_reuse(State) ->
    State#aws_state{reuse_conn = none}.

-spec close_reuse_connection(State :: aws_state()) -> aws_state().
%% @doc Close any connection held in the reuse slot and clear it, returning the
%% state to one-shot mode. Safe to call when no connection is held.
%% @end
close_reuse_connection(#aws_state{reuse_conn = undefined} = State) ->
    State;
close_reuse_connection(#aws_state{reuse_conn = none} = State) ->
    State#aws_state{reuse_conn = undefined};
close_reuse_connection(#aws_state{reuse_conn = {Conn, _Host, _Port}} = State) ->
    aws_lib_httpc:close(Conn),
    State#aws_state{reuse_conn = undefined}.

%%====================================================================
%% Legacy functions - to be updated
%%====================================================================

-spec has_credentials(State :: aws_state()) -> boolean().
%% @doc Check if the state contains valid, non-expired credentials.
%% @end
has_credentials(#aws_state{credentials = undefined}) ->
    false;
has_credentials(#aws_state{credentials = #aws_credentials{expiration = Expiration}}) ->
    not expired_credentials(Expiration).

%%====================================================================
%% exported wrapper functions
%%====================================================================

get(Service, Path, State) ->
    get(Service, Path, [], State).

get(Service, Path, Headers, State) ->
    get(Service, Path, Headers, [], State).

get(Service, Path, Headers, Options, State) ->
    request(Service, get, Path, <<>>, Headers, Options, State).

-spec post(
    Service :: string(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    State :: aws_state()
) -> {ok, {headers(), term()}, aws_state()} | result_error().
%% @doc Perform a HTTP Post request to the AWS API for the specified service. The
%%      response will automatically be decoded if it is either in JSON or XML
%%      format.
%% @end
post(Service, Path, Body, Headers, State) ->
    post(Service, Path, Body, Headers, [], State).

post(Service, Path, Body, Headers, Options, State) ->
    request(Service, post, Path, Body, Headers, Options, State).

-spec put(
    Service :: string(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    State :: aws_state()
) -> {ok, {headers(), term()}, aws_state()} | result_error().
%% @doc Perform a HTTP Put request to the AWS API for the specified service. The
%%      response will automatically be decoded if it is either in JSON or XML
%%      format.
%% @end
put(Service, Path, Body, Headers, State) ->
    put(Service, Path, Body, Headers, [], State).

put(Service, Path, Body, Headers, Options, State) ->
    request(Service, put, Path, Body, Headers, Options, State).

-spec refresh_credentials(State :: aws_state()) -> {ok, aws_state()} | {error, term()}.
%% @doc Manually refresh the credentials from the environment, filesystem or EC2 Instance Metadata Service.
%% @end
refresh_credentials(State) ->
    do_refresh_credentials(State).

-spec sign_headers(
    AccessKey :: access_key(),
    SecretKey :: secret_access_key(),
    SecurityToken :: security_token(),
    Region :: region(),
    Service :: string(),
    Method :: method(),
    URI :: string(),
    Headers :: headers(),
    Body :: body(),
    BodyHash :: iodata() | undefined
) -> {ok, headers()} | {error, {malformed_uri, string()}}.
sign_headers(
    AccessKey, SecretKey, SecurityToken, Region, Service, Method, URI, Headers, Body, BodyHash
) ->
    aws_lib_sign:headers(
        #request{
            access_key = AccessKey,
            secret_access_key = SecretKey,
            security_token = SecurityToken,
            region = Region,
            service = Service,
            method = Method,
            uri = URI,
            headers = Headers,
            body = Body
        },
        BodyHash
    ).

-spec request(
    Service :: string(),
    Method :: method(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    State :: aws_state()
) -> {ok, {headers(), term()}, aws_state()} | result_error().
%% @doc Perform a HTTP request to the AWS API for the specified service. The
%%      response will automatically be decoded if it is either in JSON or XML
%%      format.
%% @end
request(Service, Method, Path, Body, Headers, State) ->
    request(Service, Method, Path, Body, Headers, [], State).

-spec request(
    Service :: string(),
    Method :: method(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    HTTPOptions :: http_options(),
    State :: aws_state()
) -> {ok, {headers(), term()}, aws_state()} | result_error().
%% @doc Perform a HTTP request to the AWS API for the specified service. The
%%      response will automatically be decoded if it is either in JSON or XML
%%      format.
%% @end
request(Service, Method, Path, Body, Headers, HTTPOptions, State) ->
    request(Service, Method, Path, Body, Headers, HTTPOptions, undefined, State).

-spec request(
    Service :: string(),
    Method :: method(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    HTTPOptions :: http_options(),
    Endpoint :: host() | undefined,
    State :: aws_state()
) -> {ok, {headers(), term()}, aws_state()} | result_error().
%% @doc Perform a HTTP request to the AWS API for the specified service, overriding
%%      the endpoint URL to use when invoking the API. This is useful for local testing
%%      of services such as DynamoDB. The response will automatically be decoded
%%      if it is either in JSON or XML format.
%% @end
request(Service, Method, Path, Body, Headers, HTTPOptions, Endpoint, State) ->
    perform_request_direct(Service, Method, Headers, Path, Body, HTTPOptions, Endpoint, State).

-spec set_credentials(access_key(), secret_access_key(), aws_state()) -> {ok, aws_state()}.
%% @doc Manually set the access credentials for requests. This should
%%      be used in cases where the client application wants to control
%%      the credentials instead of automatically discovering them from
%%      configuration or the AWS Instance Metadata service.
%% @end
set_credentials(AccessKey, SecretAccessKey, State) ->
    Creds = #aws_credentials{
        access_key = AccessKey,
        secret_key = SecretAccessKey,
        security_token = undefined,
        expiration = undefined
    },
    {ok, State#aws_state{credentials = Creds}}.

-spec set_credentials(access_key(), secret_access_key(), security_token(), aws_state()) ->
    {ok, aws_state()}.
%% @doc Manually set the access credentials for requests. This should
%%      be used in cases where the client application wants to control
%%      the credentials instead of automatically discovering them from
%%      configuration or the AWS Instance Metadata service.
%% @end
set_credentials(AccessKey, SecretAccessKey, SecurityToken, State) ->
    Creds = #aws_credentials{
        access_key = AccessKey,
        secret_key = SecretAccessKey,
        security_token = SecurityToken,
        expiration = undefined
    },
    {ok, State#aws_state{credentials = Creds}}.

-spec ensure_credentials_valid(State :: aws_state()) -> {ok, aws_state()} | {error, term()}.
%% @doc Invoked before each AWS service API request to check if the current credentials are available and that they have not expired.
%%      If the credentials are available and are still current, then move on and perform the request.
%%      If the credentials are not available or have expired, then refresh them before performing the request.
%% @end
ensure_credentials_valid(State) ->
    ?LOG_DEBUG("Making sure AWS credentials are available and still valid"),
    case has_credentials(State) of
        true ->
            {ok, State};
        false ->
            refresh_credentials(State)
    end.

-spec perform_request_direct(
    Service :: string(),
    Method :: method(),
    Headers :: headers(),
    Path :: path(),
    Body :: body(),
    Options :: http_options(),
    Host :: string() | undefined,
    State :: aws_state()
) -> {ok, {headers(), term()}, aws_state()} | result_error().
perform_request_direct(Service, Method, Headers, Path, Body, Options, Host, State0) ->
    case prepare_signed_request(Service, Method, Headers, Path, Body, Options, Host, State0) of
        {ok, URI, SignedHeaders, Options1, State1} ->
            case gun_request(Method, URI, SignedHeaders, Body, Options1) of
                {ok, Response} ->
                    {ok, Response, State1};
                Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec prepare_signed_request(
    Service :: string(),
    Method :: method(),
    Headers :: headers(),
    Path :: path(),
    Body :: body(),
    Options :: http_options(),
    Host :: string() | undefined,
    State :: aws_state()
) ->
    {ok, URI :: string(), headers(), http_options(), aws_state()}
    | {error, term()}.
%% Ensure credentials, resolve the region, build the endpoint URI, and sign the
%% request headers. Shared by the one-shot path (perform_request_direct/8) and
%% the connection-reusing retry path (perform_request_reuse/8); it performs no
%% network I/O of its own beyond a possible credential refresh.
prepare_signed_request(Service, Method, Headers, Path, Body, Options, Host, State0) ->
    % Ensure we have credentials
    State1 =
        case has_credentials(State0) of
            false ->
                case refresh_credentials(State0) of
                    {ok, S} -> S;
                    {error, _} -> State0
                end;
            true ->
                State0
        end,

    % Get credentials and region from state
    case State1#aws_state.credentials of
        #aws_credentials{
            access_key = AccessKey,
            secret_key = SecretKey,
            security_token = SecurityToken
        } ->
            % Get region
            Region =
                case State1#aws_state.config of
                    #aws_config{region = R} when R =/= undefined -> R;
                    _ -> ?DEFAULT_REGION
                end,

            URI = endpoint(Region, Host, Service, Path),
            case
                sign_headers(
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
            of
                {ok, SignedHeaders} ->
                    Options1 = ensure_timeout_option(Options, State1),
                    {ok, URI, SignedHeaders, Options1, State1};
                {error, _} = Error ->
                    Error
            end;
        undefined ->
            {error, no_credentials}
    end.

-spec endpoint(
    Region :: region(),
    Host :: string() | undefined,
    Service :: string(),
    Path :: string()
) -> string().
%% @doc Construct the request URI for the service, region, and path.
%%
%% An explicit `Host' (the request/8 Endpoint parameter) always wins and is used
%% verbatim over HTTPS, preserving the pre-existing behaviour. Otherwise, when an
%% ``AWS_ENDPOINT_URL``/``AWS_ENDPOINT_URL_<SERVICE>`` override is configured for
%% local development, the request targets that base URL (its scheme, host, and
%% port), with the AWS request path appended. With no explicit host and no
%% override, the request targets the standard regional HTTPS endpoint.
%% @end
endpoint(Region, undefined, Service, Path) ->
    case aws_lib_config:endpoint_url(Service) of
        undefined ->
            lists:flatten(["https://", endpoint_host(Region, Service), Path]);
        Override ->
            lists:flatten([strip_trailing_slash(Override), Path])
    end;
endpoint(_, Host, _, Path) ->
    lists:flatten(["https://", Host, Path]).

%% Drop a single trailing slash from an endpoint-URL override so joining it with
%% a request path (which always starts with "/") does not produce a double slash.
strip_trailing_slash(Url) ->
    case lists:reverse(Url) of
        [$/ | Rest] -> lists:reverse(Rest);
        _ -> Url
    end.

%% @doc Construct the endpoint hostname for the request based upon the service
%%      and region.
%% @end
endpoint_host(Region, Service) ->
    lists:flatten(string:join([Service, Region, endpoint_tld(Region)], ".")).

-spec endpoint_tld(Region :: region()) -> host().
%% @doc Construct the endpoint hostname TLD for the request based upon the region.
%%      See https://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region for details.
%% @end
endpoint_tld("cn-north-1") ->
    "amazonaws.com.cn";
endpoint_tld("cn-northwest-1") ->
    "amazonaws.com.cn";
endpoint_tld("us-iso-east-1") ->
    "c2s.ic.gov";
endpoint_tld("us-iso-west-1") ->
    "c2s.ic.gov";
endpoint_tld("us-isob-east-1") ->
    "sc2s.sgov.gov";
endpoint_tld("us-isof-east-1") ->
    "csp.hci.ic.gov";
endpoint_tld("us-isof-south-1") ->
    "csp.hci.ic.gov";
endpoint_tld("eusc-de-east-1") ->
    "amazonaws.eu";
endpoint_tld(_Other) ->
    "amazonaws.com".

-spec do_refresh_credentials(State :: aws_state()) -> {ok, aws_state()} | {error, term()}.
%% @doc Refresh credentials from environment, filesystem, or EC2 Instance Metadata Service.
%% @end
do_refresh_credentials(State0) ->
    % Get or detect region
    {Region, State1} =
        case State0#aws_state.config of
            #aws_config{region = undefined} ->
                {ok, R, _} = aws_lib_config:region(#aws_config{}),
                {R, State0#aws_state{config = #aws_config{region = R}}};
            #aws_config{region = R} ->
                {R, State0}
        end,

    % Load credentials
    Config =
        case State1#aws_state.config of
            undefined -> #aws_config{region = Region};
            C -> C
        end,

    case aws_lib_config:credentials(Config) of
        {ok, Creds, Config2} ->
            {ok, State1#aws_state{credentials = Creds, config = Config2}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec expired_credentials(Expiration :: calendar:datetime()) -> boolean().
%% @doc Indicates whether the given expiration is at or within the refresh
%% buffer window. Credentials are treated as expired a few minutes before they
%% actually lapse (?CREDENTIAL_REFRESH_BUFFER_SECONDS) so they are refreshed
%% proactively rather than after a request has already started with credentials
%% that expire mid-flight.
%% end
expired_credentials(undefined) ->
    false;
expired_credentials(Expiration) ->
    Now = calendar:datetime_to_gregorian_seconds(local_time()),
    Expires = calendar:datetime_to_gregorian_seconds(Expiration),
    Now >= (Expires - ?CREDENTIAL_REFRESH_BUFFER_SECONDS).

-spec local_time() -> calendar:datetime().
%% @doc Return the current UTC time. Callers compare this against UTC expiration
%% timestamps, so it must be UTC. We read calendar:universal_time/0 directly
%% rather than converting local time via local_time_to_universal_time_dst/1:
%% that conversion returns two datetimes during a DST fall-back transition (the
%% local hour is ambiguous) and none during a spring-forward gap, so matching a
%% single [Value] crashed with badmatch in the transition window.
%% @end
local_time() ->
    calendar:universal_time().

-spec expired_imdsv2_token('undefined' | imdsv2token()) -> boolean().
%% @doc Determine whether or not an Imdsv2Token has expired.
%% @end
expired_imdsv2_token(undefined) ->
    ?LOG_DEBUG("EC2 IMDSv2 token has not yet been obtained"),
    true;
expired_imdsv2_token(#imdsv2token{expiration = undefined}) ->
    ?LOG_DEBUG("EC2 IMDSv2 token is not available"),
    true;
expired_imdsv2_token(#imdsv2token{expiration = Expiration}) ->
    Now = calendar:datetime_to_gregorian_seconds(local_time()),
    HasExpired = Now >= Expiration,
    ?LOG_DEBUG("EC2 IMDSv2 token has expired: ~tp", [HasExpired]),
    HasExpired.

-spec get_imdsv2_token(State :: aws_state()) -> {ok, imdsv2token() | undefined}.
%% @doc return the current Imdsv2Token used to perform instance metadata service requests.
%% @end
get_imdsv2_token(#aws_state{config = #aws_config{imdsv2_token = Token}}) ->
    {ok, Token};
get_imdsv2_token(#aws_state{config = undefined}) ->
    {ok, undefined}.

-spec set_imdsv2_token(
    Token :: imdsv2token(),
    State :: aws_state()
) -> {ok, aws_state()}.
%% @doc Manually set the Imdsv2Token used to perform instance metadata service requests.
%% @end
set_imdsv2_token(Token, State = #aws_state{config = Config}) when Config =/= undefined ->
    {ok, State#aws_state{config = Config#aws_config{imdsv2_token = Token}}};
set_imdsv2_token(Token, State) ->
    {ok, State#aws_state{config = #aws_config{imdsv2_token = Token}}}.

-spec ensure_imdsv2_token_valid(State :: aws_state()) -> {ok, security_token(), aws_state()}.
ensure_imdsv2_token_valid(State0) ->
    {ok, Imdsv2Token} = get_imdsv2_token(State0),
    case expired_imdsv2_token(Imdsv2Token) of
        true ->
            Value = aws_lib_config:load_imdsv2_token(),
            Expiration =
                calendar:datetime_to_gregorian_seconds(local_time()) + ?METADATA_TOKEN_TTL_SECONDS,
            NewToken = #imdsv2token{token = Value, expiration = Expiration},
            {ok, State1} = set_imdsv2_token(NewToken, State0),
            {ok, Value, State1};
        _ ->
            {ok, Imdsv2Token#imdsv2token.token, State0}
    end.

-spec api_get_request(
    Service :: string(),
    Path :: path(),
    State :: aws_state()
) -> {ok, list(), aws_state()} | {error, term(), aws_state()}.
%% @doc Invoke an API call to an AWS service.
%% @end
api_get_request(Service, Path, State) ->
    ?LOG_DEBUG("invoking AWS get request {Service: ~tp; Path: ~tp}...", [Service, Path]),
    api_request_with_retries(
        Service,
        get,
        Path,
        "",
        [],
        ?MAX_RETRIES,
        ?LINEAR_BACK_OFF_MILLIS,
        State
    ).

-spec api_post_request(
    Service :: string(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    State :: aws_state()
) -> {ok, list(), aws_state()} | {error, term(), aws_state()}.
%% @doc Perform a HTTP Post request to the AWS API for the specified service. The
%%      response will automatically be decoded if it is either in JSON or XML
%%      format.
%% @end
api_post_request(Service, Path, Body, Headers, State) ->
    ?LOG_DEBUG("invoking AWS post request {Service: ~tp; Path: ~tp}...", [Service, Path]),
    api_request_with_retries(
        Service,
        post,
        Path,
        Body,
        Headers,
        ?MAX_RETRIES,
        ?LINEAR_BACK_OFF_MILLIS,
        State
    ).

-spec instance_volumes(State :: aws_state()) -> {ok, volumes_list(), aws_state()} | {error, term()}.
%% @doc Return the EBS volumes attached to the current instance from the EC2 API.
%% @end
instance_volumes(State0 = #aws_state{config = Config0}) ->
    case aws_lib_config:instance_id(Config0) of
        {ok, InstanceId, Config1} ->
            Path =
                "/?Action=DescribeVolumes&Filter.1.Name=attachment.instance-id&Filter.1.Value.1=" ++
                    InstanceId ++ "&Version=2016-11-15",
            case aws_lib:api_get_request("ec2", Path, State0) of
                {ok, Response, _State1} ->
                    case parse_volumes_response(Response) of
                        {ok, Volumes} ->
                            {ok, Volumes, State0#aws_state{config = Config1}};
                        Error ->
                            Error
                    end;
                {error, Reason, _State1} ->
                    {error, Reason}
            end;
        Error ->
            Error
    end.

%% -----------------------------------------------------------------------------
%% Private / Internal Methods
%% -----------------------------------------------------------------------------

-spec api_request_with_retries(
    Service :: string(),
    Method :: method(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    Retries :: integer(),
    WaitTime :: integer(),
    State :: aws_state()
) ->
    {ok, list(), aws_state()} | {error, term(), aws_state()}.
%% @doc Invoke an API call to an AWS service with retries.
%% @end
api_request_with_retries(Service, Method, Path, Body, Headers, Retries, WaitTime, State) ->
    %% Open a single connection and reuse it across the whole retry sequence
    %% instead of opening a fresh TCP+TLS connection per attempt (issue #91).
    %% When the state carries a reuse_conn (cross-request reuse, issue #107),
    %% seed from it so the first attempt avoids a fresh handshake; otherwise
    %% start with `undefined' (nothing open yet, opened lazily on first attempt).
    Conn0 = seed_conn_from_state(State),
    api_request_with_retries(
        Service, Method, Path, Body, Headers, Retries, WaitTime, State, Conn0, undefined
    ).

-spec api_request_with_retries(
    Service :: string(),
    Method :: method(),
    Path :: path(),
    Body :: body(),
    Headers :: headers(),
    Retries :: integer(),
    WaitTime :: integer(),
    State :: aws_state(),
    Conn :: aws_lib_httpc:conn() | undefined,
    LastError :: result_error() | undefined
) ->
    {ok, list(), aws_state()} | {error, term(), aws_state()}.
api_request_with_retries(
    _Service, _Method, _Path, _Body, _Headers, Retries, _WaitTime, State, Conn, LastError
) when
    Retries =< 0
->
    ?LOG_ERROR("Request to AWS service has failed after ~b retries", [?MAX_RETRIES]),
    %% Reconcile the carried connection into the returned state: in reuse mode a
    %% still-live connection is handed back, a transport-dropped one (undefined)
    %% clears the slot; in one-shot mode it is closed.
    State1 = finish_conn(Conn, State),
    %% Surface the last attempt's decoded AWS error so the caller can act on the
    %% service error code (throttling vs invalid parameter vs access denied)
    %% rather than a generic string (issue #82).
    {error, exhausted_error(LastError), State1};
api_request_with_retries(
    Service, Method, Path, Body, Headers, Retries, WaitTime, State0, Conn0, LastError
) ->
    case ensure_credentials_valid(State0) of
        {ok, State1} ->
            {Result, Conn1, Verdict} =
                perform_request_reuse(Service, Method, Headers, Path, Body, [], State1, Conn0),
            case Result of
                {ok, {_Headers, Payload}, State2} ->
                    ?LOG_DEBUG("AWS request: ~ts~nResponse: ~tp", [Path, Payload]),
                    State3 = finish_conn(Conn1, State2),
                    {ok, Payload, State3};
                {error, Message, Response} = Error ->
                    %% Message may be a status string (from format_response/1
                    %% on an HTTP error) or a tuple such as
                    %% {gun_open_failed, Reason} on a connection failure, so
                    %% use ~tp rather than ~ts.
                    ?LOG_WARNING("Error occurred: ~tp", [Message]),
                    case Response of
                        {_, Payload} ->
                            ?LOG_WARNING("Failed AWS request: ~ts~nResponse: ~tp", [
                                Path, Payload
                            ]);
                        _ ->
                            ok
                    end,
                    case Verdict of
                        not_retriable ->
                            %% A 4xx client error that will not succeed on retry
                            %% (issue #80): return the decoded error to the caller
                            %% immediately rather than burning the remaining
                            %% retries. The exchange completed cleanly, so in
                            %% reuse mode the connection is handed back for the
                            %% next request; in one-shot mode finish_conn/2 closes
                            %% it.
                            %% Log at ERROR level so log-level alerting still sees
                            %% a permanent failure: before issue #80 short-circuited
                            %% the retry loop, this failure would have exhausted the
                            %% retries and reached the ?LOG_ERROR clause above.
                            ?LOG_ERROR(
                                "Request to AWS service has failed with a permanent error: ~tp",
                                [Message]
                            ),
                            State3 = finish_conn(Conn1, State1),
                            {error, exhausted_error(Error), State3};
                        retriable ->
                            ?LOG_WARNING(
                                "Will retry AWS request, remaining retries: ~b", [Retries]
                            ),
                            timer:sleep(WaitTime),
                            %% perform_request_reuse/8 hands back a live connection
                            %% after an HTTP-level error (reused on the next
                            %% attempt) or `undefined' after a transport failure
                            %% (reopened next attempt). Carry the most informative
                            %% error forward so retry exhaustion can return it: a
                            %% decoded service-error body from an earlier attempt
                            %% is kept even if a later attempt fails at the
                            %% transport level with no body.
                            api_request_with_retries(
                                Service,
                                Method,
                                Path,
                                Body,
                                Headers,
                                Retries - 1,
                                WaitTime,
                                State1,
                                Conn1,
                                keep_informative_error(LastError, Error)
                            )
                    end
            end;
        {error, Reason} ->
            State1 = finish_conn(Conn0, State0),
            {error, {credentials, Reason}, State1}
    end.

%% Keep the more informative of two retry errors: an error carrying a decoded
%% AWS error body (an HTTP 4xx/5xx) wins over one without (a transport failure,
%% which format_response/1 shapes as {error, _, undefined}). This preserves a
%% service error seen on an earlier attempt when a later attempt fails at the
%% transport level, so retry exhaustion still surfaces the useful detail.
keep_informative_error(_LastError, {error, _Message, {_Headers, _Body}} = NewError) ->
    %% The new error has a decoded body: it is at least as informative.
    NewError;
keep_informative_error({error, _Message, {_Headers, _Body}} = LastError, _NewError) ->
    %% The new error has no body but a prior one did: keep the prior error.
    LastError;
keep_informative_error(_LastError, NewError) ->
    NewError.

%% Build the caller-facing error at retry exhaustion. When an attempt produced a
%% decoded AWS error body (an HTTP 4xx/5xx), surface that body under
%% `service_error' so the caller can inspect the AWS error code. A transport
%% failure carries no body (or there was no attempt), so fall back to a generic
%% reason. The decoded body is a proplist for JSON/XML responses but may be a
%% raw binary for other content types, hence the list()|binary() element.
-spec exhausted_error(result_error() | undefined) ->
    {service_error, list() | binary()} | {service_error, retries_exhausted}.
exhausted_error({error, _Message, {_Headers, DecodedBody}}) ->
    {service_error, DecodedBody};
exhausted_error(_) ->
    {service_error, retries_exhausted}.

%% Perform one request attempt on a reusable connection. Opens a connection when
%% none is carried (or the carried one targets a different host) and reuses the
%% carried one otherwise. Returns the same result shape as request/6 together
%% with the connection slot to carry into the next attempt: a live
%% {Conn, Host, Port} after an HTTP-level error (so the retry reuses it), or
%% `undefined' after a transport failure (so the retry reopens). The connection
%% is opened with `retry => 0' so a dropped socket terminates the Gun process; a
%% request on a since-dead connection then fails fast through gun:await's process
%% monitor ({error, {down, _}}) and is reopened on the next attempt.
-spec perform_request_reuse(
    Service :: string(),
    Method :: method(),
    Headers :: headers(),
    Path :: path(),
    Body :: body(),
    Options :: http_options(),
    State :: aws_state(),
    ConnSlot :: {aws_lib_httpc:conn(), string(), inet:port_number()} | undefined
) ->
    {
        {ok, {headers(), term()}, aws_state()} | result_error(),
        {aws_lib_httpc:conn(), string(), inet:port_number()} | undefined,
        retriable | not_retriable
    }.
perform_request_reuse(Service, Method, Headers, Path, Body, Options, State0, ConnSlot0) ->
    case prepare_signed_request(Service, Method, Headers, Path, Body, Options, undefined, State0) of
        {ok, URI, SignedHeaders, Options1, State1} ->
            case aws_lib_uri:parse(URI) of
                {ok, Uri} ->
                    Host = aws_lib_uri:host(Uri),
                    Port = aws_lib_uri:port(Uri),
                    Target = aws_lib_uri:target(Uri),
                    OpenOpts = (gun_open_opts(aws_lib_uri:transport(Uri), Options1))#{retry => 0},
                    case ensure_open(ConnSlot0, Host, Port, OpenOpts) of
                        {ok, Conn1} ->
                            Timeout = proplists:get_value(
                                timeout, Options1, ?DEFAULT_API_TIMEOUT
                            ),
                            Response = aws_lib_httpc:request(
                                Conn1, Method, Target, SignedHeaders, Body, Timeout
                            ),
                            %% Classify from the raw response (it still carries
                            %% the numeric status) and the formatted result (it
                            %% carries the body decoded once by
                            %% format_response/1).
                            Formatted = aws_lib_response:format_response(Response),
                            classify_reuse_response(
                                Formatted,
                                aws_lib_response:classify_response(Response, Formatted),
                                State1,
                                {Conn1, Host, Port}
                            );
                        {error, Reason} ->
                            {
                                aws_lib_response:format_response({error, Reason}),
                                undefined,
                                retriable
                            }
                    end;
                {error, _Reason} = Error ->
                    %% A malformed URI never becomes valid on retry, but it is not
                    %% an AWS response either; preserve the pre-existing retry
                    %% behaviour by treating it as retriable.
                    {aws_lib_response:format_response(Error), ConnSlot0, retriable}
            end;
        {error, _} = Error ->
            %% Shape the 2-tuple error through format_response/1 into the
            %% 3-tuple result the retry loop matches on; returning the raw
            %% {error, Reason} here would raise a case_clause in
            %% api_request_with_retries/10.
            {aws_lib_response:format_response(Error), ConnSlot0, retriable}
    end.

%% Attach the connection slot and retry verdict to carry forward based on the
%% request outcome: success or an HTTP-level error leaves the connection intact;
%% a transport failure (format_response/1's {error, _, undefined}) makes it
%% unusable. The verdict from classify_response/1 rides through unchanged; a
%% success is always reported as retriable but the caller ignores the verdict on
%% the success branch.
classify_reuse_response({ok, Result}, Verdict, State, ConnSlot) ->
    {{ok, Result, State}, ConnSlot, Verdict};
classify_reuse_response({error, _Reason, undefined} = Err, Verdict, _State, {Conn, _, _}) ->
    aws_lib_httpc:close(Conn),
    {Err, undefined, Verdict};
classify_reuse_response({error, _Message, _Payload} = Err, Verdict, _State, ConnSlot) ->
    {Err, ConnSlot, Verdict}.

%% Return a usable connection to Host:Port. If the carried slot targets the same
%% host, reuse it; if it targets a different host (cross-service boot pass),
%% close the old one and open fresh; if none is carried, open fresh. A carried
%% connection is reused as-is: if it has since died, gun:await's process monitor
%% surfaces a transport error and the next attempt reopens.
ensure_open(undefined, Host, Port, OpenOpts) ->
    aws_lib_httpc:open(Host, Port, OpenOpts);
ensure_open({Conn, Host, Port}, Host, Port, _OpenOpts) ->
    {ok, Conn};
ensure_open({OldConn, _OldHost, _OldPort}, Host, Port, OpenOpts) ->
    aws_lib_httpc:close(OldConn),
    aws_lib_httpc:open(Host, Port, OpenOpts).

close_if_open(undefined) -> ok;
close_if_open({Conn, _Host, _Port}) -> aws_lib_httpc:close(Conn).

%% Seed the retry loop's connection slot from the state's reuse_conn. In
%% one-shot mode (reuse_conn = undefined, the default) this returns `undefined'
%% and the loop opens fresh on the first attempt. In reuse mode (none or a
%% cached {Conn, Host, Port}) the state's slot is handed in so the first attempt
%% avoids a handshake when possible.
seed_conn_from_state(#aws_state{reuse_conn = undefined}) ->
    undefined;
seed_conn_from_state(#aws_state{reuse_conn = none}) ->
    undefined;
seed_conn_from_state(#aws_state{reuse_conn = ConnSlot}) ->
    ConnSlot.

%% Reconcile the retry loop's connection slot into the returned state at every
%% terminal path (success, a not_retriable error, retry exhaustion, a
%% credentials failure). In one-shot mode (reuse_conn = undefined), close the
%% connection. In reuse mode, write a still-live slot back into the state so the
%% next api_*_request call in the same unit of work reuses it; a transport
%% failure left `undefined', so record `none' (reuse mode, no connection) rather
%% than `undefined' (which would silently drop the state out of reuse mode).
%% Handing back a connection after a cleanly completed 4xx keeps the reuse slot
%% in sync with reality: the connection is as usable as after a 2xx, and the
%% caller's state no longer references a connection this call already closed
%% (issue #80).
finish_conn(ConnSlot, #aws_state{reuse_conn = undefined} = State) ->
    close_if_open(ConnSlot),
    State;
finish_conn(undefined, State) ->
    State#aws_state{reuse_conn = none};
finish_conn(ConnSlot, State) ->
    State#aws_state{reuse_conn = ConnSlot}.

%% Add the state's AWS API timeout to the request options unless the caller
%% already passed an explicit `timeout'. A per-request `timeout' option therefore
%% takes precedence over the per-state value (aws_lib:set_timeout/2), which in
%% turn falls back to ?DEFAULT_API_TIMEOUT via get_timeout/1.
ensure_timeout_option(Options, State) ->
    case proplists:is_defined(timeout, Options) of
        true -> Options;
        false -> [{timeout, get_timeout(State)} | Options]
    end.

%% Gun HTTP client functions. The Gun lifecycle itself lives in aws_lib_httpc;
%% these functions build the AWS-API-specific request (URI parsing, connection
%% options from Options) and shape the result via format_response/1.
gun_request(Method, URI, Headers, Body, Options) ->
    %% A parse or connection failure is returned as {error, Reason} (not raised)
    %% so it flows through format_response/1 into the {error, _, _} shape the
    %% retry loop in api_request_with_retries/8 matches, rather than escaping it.
    case aws_lib_uri:parse(URI) of
        {ok, Uri} ->
            Host = aws_lib_uri:host(Uri),
            Port = aws_lib_uri:port(Uri),
            %% target/1 carries the query: Path is the Gun request line.
            Path = aws_lib_uri:target(Uri),
            OpenOpts = gun_open_opts(aws_lib_uri:transport(Uri), Options),
            Response = aws_lib_httpc:request(
                Host, Port, Method, Path, Headers, Body, OpenOpts
            ),
            aws_lib_response:format_response(Response);
        {error, _Reason} = Error ->
            aws_lib_response:format_response(Error)
    end.

%% Build aws_lib_httpc open options from the request Options and Transport:
%% derive Gun protocols from the requested HTTP version, use the given transport
%% (TLS or plain TCP), and thread the connect and request timeouts through. The
%% transport is scheme-driven (see aws_lib_uri:transport/1) rather than derived
%% from the port, so an https endpoint on a non-443 port still uses TLS and an
%% http override stays plaintext.
gun_open_opts(Transport, Options) when Transport =:= tls; Transport =:= tcp ->
    HttpVersion = proplists:get_value(version, Options, "HTTP/1.1"),
    Protocols =
        case HttpVersion of
            "HTTP/2" -> [http2, http];
            "HTTP/2.0" -> [http2, http];
            "HTTP/1.1" -> [http];
            "HTTP/1.0" -> [http];
            % Default: try HTTP/2, fallback to HTTP/1.1
            _ -> [http2, http]
        end,
    #{
        transport => Transport,
        protocols => Protocols,
        connect_timeout => proplists:get_value(connect_timeout, Options, infinity),
        timeout => proplists:get_value(timeout, Options, ?DEFAULT_API_TIMEOUT)
    }.

-spec parse_volumes_response(term()) -> {'ok', volumes_list()} | {'error', 'parse_error'}.
%% @doc Parse the DescribeVolumes XML response into a list of volume information.
%% @end
parse_volumes_response([{"DescribeVolumesResponse", VolumeData}]) ->
    case proplists:get_value("volumeSet", VolumeData, []) of
        [] ->
            {ok, []};
        VolumeSet when is_list(VolumeSet) ->
            Volumes = lists:map(fun parse_volume/1, VolumeSet),
            {ok, Volumes};
        _ ->
            {error, parse_error}
    end;
parse_volumes_response(_) ->
    {error, parse_error}.

-spec parse_volume(term()) -> volume_info().
%% @doc Parse individual volume data from XML response.
%% @end
parse_volume({"item", VolumeProps}) ->
    VolumeId = proplists:get_value("volumeId", VolumeProps, ""),
    Size = proplists:get_value("size", VolumeProps, ""),
    VolumeType = proplists:get_value("volumeType", VolumeProps, ""),
    State = proplists:get_value("status", VolumeProps, ""),

    AttachmentSet = proplists:get_value("attachmentSet", VolumeProps, []),
    Attachment =
        case AttachmentSet of
            [{"item", AttachProps}] ->
                [
                    {device, proplists:get_value("device", AttachProps, "")},
                    {state, proplists:get_value("status", AttachProps, "")}
                ];
            _ ->
                []
        end,

    [
        {volume_id, VolumeId},
        {size, Size},
        {volume_type, VolumeType},
        {state, State},
        {attachment, Attachment}
    ];
parse_volume(_) ->
    [].
