%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%% @headerfile
%% @private
%% @doc aws_lib client library constants and records
%% @end
%% ====================================================================

-define(MIME_AWS_JSON, "application/x-amz-json-1.0").
-define(SCHEME, https).

-define(DEFAULT_REGION, "us-east-1").
-define(DEFAULT_PROFILE, "default").

-define(INSTANCE_AZ, "placement/availability-zone").
-define(INSTANCE_HOST, "169.254.169.254").

% rabbitmq/rabbitmq-peer-discovery-aws#25

% Timeout for EC2 Instance Metadata service (IMDS) requests. INSTANCE_HOST is a
% link-local pseudo-IP that should have good performance and return data
% quickly, so a short timeout is appropriate here. This is NOT used for AWS API
% requests -- see ?DEFAULT_API_TIMEOUT.
-define(DEFAULT_IMDS_TIMEOUT, 2250).

% Default timeout for AWS API requests, applied when neither the request options
% nor the aws_config() specify one. AWS API operations (S3 uploads,
% CreateSnapshot, DynamoDB batch writes, and so on) can routinely exceed a few
% seconds, so this matches the AWS SDK default of 30s rather than the short IMDS
% timeout. Overridable per request via the `timeout' option or per state via
% aws_lib:set_timeout/2.
-define(DEFAULT_API_TIMEOUT, 30000).

-define(INSTANCE_CREDENTIALS, "iam/security-credentials").
-define(INSTANCE_METADATA_BASE, "latest/meta-data").
-define(INSTANCE_ID, "instance-id").

-define(TOKEN_URL, "latest/api/token").

-define(METADATA_TOKEN_TTL_HEADER, "X-aws-ec2-metadata-token-ttl-seconds").

% EC2 Instance Metadata service version 2 (IMDSv2) uses session-oriented authentication.
% Instance metadata service requests are only needed for loading/refreshing credentials.
% Long-lived EC2 IMDSv2 tokens are unnecessary. The token only needs to be valid long enough
% to successfully load/refresh the credentials. 60 seconds is more than enough time to accomplish this.
-define(METADATA_TOKEN_TTL_SECONDS, 60).

-define(METADATA_TOKEN, "X-aws-ec2-metadata-token").

% Refresh credentials this many seconds before they actually expire, so a
% request does not start with credentials that lapse mid-flight. Matches the
% 5-minute buffer erlcloud uses.
-define(CREDENTIAL_REFRESH_BUFFER_SECONDS, 300).

-define(BACKOFF_BASE_MILLIS, 500).
-define(BACKOFF_CAP_MILLIS, 10000).
-define(MAX_RETRIES, 5).

-define(AWS_CREDENTIALS_TABLE, aws_credentials).

%% TODO LRB
%% -define(AWS_CONFIG_TABLE, aws_config).

-type access_key() :: nonempty_string().
-type secret_access_key() :: nonempty_string().
-type expiration() :: calendar:datetime() | undefined.
-type security_token() :: nonempty_string() | undefined.
-type region() :: nonempty_string() | undefined.
-type path() :: string().

-type attachment_info() :: [{device, string()} | {state, string()}].
-type volume_info() :: [
    {volume_id, string()}
    | {size, string()}
    | {volume_type, string()}
    | {state, string()}
    | {attachment, attachment_info()}
].
-type volumes_list() :: [volume_info()].

-type error() :: {error, Reason :: atom()}.

-record(imdsv2token, {
    token :: security_token() | undefined,
    expiration :: non_neg_integer() | undefined
}).
-type imdsv2token() :: #imdsv2token{}.

-record(aws_credentials, {
    access_key :: access_key(),
    secret_key :: secret_access_key(),
    security_token :: security_token(),
    expiration :: expiration()
}).
-type aws_credentials() :: #aws_credentials{}.

-record(aws_config, {
    region = undefined :: region(),
    imdsv2_token = undefined :: imdsv2token() | undefined,
    %% Per-state override for the AWS API request timeout (ms). undefined means
    %% use ?DEFAULT_API_TIMEOUT. Set via aws_lib:set_timeout/2.
    timeout = undefined :: timeout() | undefined
}).
-type aws_config() :: #aws_config{}.

-record(aws_state, {
    credentials :: aws_credentials() | undefined,
    config :: aws_config() | undefined,
    %% A reusable connection carried across a bounded unit of work (e.g. the
    %% boot ARN-resolution pass). `undefined' = one-shot mode (the default for
    %% all callers that do not opt in). `none' = reuse mode armed but no
    %% connection cached yet. `{Conn, Host, Port}' = a cached connection to the
    %% given endpoint. api_request_with_retries seeds from and writes back to
    %% this field when it is not `undefined'.
    reuse_conn :: {aws_lib_httpc:conn(), string(), inet:port_number()} | none | undefined
}).
%% Type aws_state() and related result types are defined in aws_lib.erl

-type host() :: string().
-type query_args() :: [tuple() | string()].

-type method() :: head | get | put | post | trace | options | delete | patch.
-type http_version() :: string().
-type status_code() :: integer().
-type reason_phrase() :: string().
-type status_line() :: {http_version(), status_code(), reason_phrase()}.
-type field() :: string().
-type value() :: string().
-type header() :: {Field :: field(), Value :: value()}.
-type headers() :: [header()].
-type body() :: iodata().

-type ssl_options() :: [ssl:tls_client_option()].

-type http_option() ::
    {timeout, timeout()}
    | {connect_timeout, timeout()}
    | {ssl, ssl_options()}
    | {essl, ssl_options()}
    | {autoredirect, boolean()}
    | {proxy_auth, {User :: string(), Password :: string()}}
    | {version, http_version()}
    | {relaxed, boolean()}
    | {url_encode, boolean()}.
-type http_options() :: [http_option()].

-record(request, {
    access_key :: access_key(),
    secret_access_key :: secret_access_key(),
    security_token :: security_token(),
    service :: string(),
    region = "us-east-1" :: string(),
    method = get :: method(),
    headers = [] :: headers(),
    uri :: string(),
    body = "" :: body()
}).
-type request() :: #request{}.

%% The response payload is the decoded body: a proplist for JSON/XML content
%% types, or the raw binary when the body could not be decoded.
-type result_ok() :: {ok, {ResponseHeaders :: headers(), Response :: list() | binary()}}.
-type result_error() ::
    {'error', Message :: reason_phrase(),
        {ResponseHeaders :: headers(), Response :: list() | binary()} | undefined}
    | {'error', {credentials, Reason :: string()}}
    | {'error', string()}.
-type result() :: result_ok() | result_error().
