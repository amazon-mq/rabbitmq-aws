# aws_lib

A focused AWS API client core for Erlang: request signing, credential management, and HTTP request lifecycle. Pure library - no OTP application, no processes, no shared state. All state is explicitly passed through function calls.

Designed to be vendored inside the RabbitMQ broker with a minimal dependency footprint (`gun` and `thoas` only). It is not a general-purpose AWS SDK; for that, see [aws-erlang](https://github.com/aws-beam/aws-erlang) or [erlcloud](https://github.com/erlcloud/erlcloud).

Originally forked from [gmr/httpc-aws](https://github.com/gmr/httpc-aws).

## Supported Erlang Versions

OTP 26+

## Quick Start

```erlang
%% Create state with region
State0 = aws_lib:new("us-east-1"),

%% Load credentials from environment/config/IMDS
{ok, State1} = aws_lib:refresh_credentials(State0),

%% Make a request
{ok, {Headers, Response}, State2} =
    aws_lib:get("ec2", "/?Action=DescribeTags&Version=2015-10-01", [], State1).
```

Or manually set credentials:

```erlang
State0 = aws_lib:new("us-east-1"),
{ok, State1} = aws_lib:set_credentials(
    "AKIDEXAMPLE",
    "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
    State0
),

RequestHeaders = [{"Content-Type", "application/x-amz-json-1.0"},
                  {"X-Amz-Target", "DynamoDB_20120810.ListTables"}],

{ok, {RespHeaders, RespBody}, State2} =
    aws_lib:post("dynamodb", "/", "{\"Limit\": 20}", RequestHeaders, State1).
```

## API

### State Management

| Function | Description |
|----------|-------------|
| `aws_lib:new/0` | Create state with default region |
| `aws_lib:new/1` | Create state with specified region |
| `aws_lib:get_region/1` | Get region from state |
| `aws_lib:set_region/2` | Set the region |
| `aws_lib:get_credentials/1` | Get credentials from state |
| `aws_lib:set_credentials/3` | Set access key and secret key |
| `aws_lib:set_credentials/4` | Set access key, secret key, and session token |
| `aws_lib:has_credentials/1` | Check if valid credentials are present |
| `aws_lib:refresh_credentials/1` | Load credentials from environment/config/IMDS |
| `aws_lib:ensure_credentials_valid/1` | Refresh credentials if missing or expired |

### HTTP Requests

| Function | Description |
|----------|-------------|
| `aws_lib:get/3-5` | GET request to an AWS service |
| `aws_lib:post/5-6` | POST request to an AWS service |
| `aws_lib:put/5-6` | PUT request to an AWS service |
| `aws_lib:request/6-8` | Generic request with full control |
| `aws_lib:api_get_request/3` | GET with automatic retries |
| `aws_lib:api_post_request/5` | POST with automatic retries |

### EC2 Operations

| Function | Description |
|----------|-------------|
| `aws_lib:instance_volumes/1` | Get EBS volumes attached to current instance |
| `aws_lib_ops:create_volume_snapshot/2-3` | Create an EBS snapshot |
| `aws_lib_ops:delete_volume_snapshot/2-3` | Delete an EBS snapshot |

## Configuration

### Credential Precedence

1. Explicitly set via `set_credentials/3-4`
2. Environment variables (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`)
3. Config file (`~/.aws/config` or `$AWS_CONFIG_FILE`)
4. Credentials file (`~/.aws/credentials` or `$AWS_SHARED_CREDENTIALS_FILE`)
5. EC2 Instance Metadata Service

### Environment Variables

| Variable | Description |
|----------|-------------|
| `AWS_ACCESS_KEY_ID` | AWS access key |
| `AWS_SECRET_ACCESS_KEY` | AWS secret key |
| `AWS_SESSION_TOKEN` | Session token for temporary credentials |
| `AWS_DEFAULT_REGION` | Default region |
| `AWS_DEFAULT_PROFILE` | Profile name in config files |
| `AWS_CONFIG_FILE` | Path to config file (default: `~/.aws/config`) |
| `AWS_SHARED_CREDENTIALS_FILE` | Path to credentials file (default: `~/.aws/credentials`) |

### EC2 Instance Metadata Service

By default, IMDSv2 (session-authenticated) is preferred. Falls back to IMDSv1 on failure. Control this with the `aws_prefer_imdsv2` application environment variable:

```erlang
%% sys.config
[{aws_lib, [{aws_prefer_imdsv2, true}]}].  %% default
```

Also available via Cuttlefish schema (`priv/schema/aws.schema`) as `aws.arns.prefer_imdsv2` for RabbitMQ plugin integration. The bare `aws.prefer_imdsv2` key is owned by the `rabbitmq_aws` library, so this plugin scopes its own setting under `aws.arns` to avoid a mapping collision.

## Build

```bash
make              # Compile
make eunit        # Run tests
make dialyze      # Static analysis
make check        # All of the above
```

## License

BSD 3-Clause License
