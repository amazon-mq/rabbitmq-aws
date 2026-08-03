# aws_lib

A focused AWS API client core for Erlang: SigV4 signing, credential discovery, and request lifecycle. Pure library (no OTP application, no processes, no ETS) - all state is explicitly passed through function calls.

Originally forked from [gmr/httpc-aws](https://github.com/gmr/httpc-aws).

## Scope and Non-Goals

aws_lib exists to be vendored **inside the RabbitMQ broker** to back Amazon MQ plugins that touch a handful of services (S3, STS, ACM-PCA, Secrets Manager, EC2). It is deliberately **not** a general-purpose AWS SDK.

**Hard constraint:** the dependency footprint must stay minimal. The only third-party deps are `gun` and `thoas`, both already in RabbitMQ's dependency set, so aws_lib adds zero new vendored dependencies to the broker. This is the dominant design constraint - it is the reason aws_lib exists rather than using [aws-erlang](https://github.com/aws-beam/aws-erlang) or [erlcloud](https://github.com/erlcloud/erlcloud), both of which are excellent but carry dependency footprints that cannot go inside the broker.

**Evaluating feature requests:** the test is "does the thing aws_lib exists to do need it?" - not "does erlcloud / aws-erlang have it?" Those libraries are useful references for finding bugs and validating approaches, but feature parity with them is explicitly a non-goal. Reaching for parity turns a focused core into a second-rate SDK and inflates the dependency surface that aws_lib was created to avoid.

**Genuine defects** (crashes, incorrect signing, credential mishandling) matter regardless of scope and should always be fixed.

## Architecture

### State-Passing Model

Every API function takes an `aws_state()` and returns an updated one:

```erlang
State0 = aws_lib:new("us-east-1"),
{ok, State1} = aws_lib:refresh_credentials(State0),
{ok, {Headers, Body}, State2} = aws_lib:get("ec2", "/path", [], State1).
```

`aws_state()` is opaque - access only through exported functions. It contains:
- `aws_credentials()` - access key, secret key, optional session token + expiration
- `aws_config()` - region, IMDSv2 token cache

### Credential Discovery Chain

Checked in order by `aws_lib_config:credentials/1`:
1. Environment variables (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`)
2. Config file (`~/.aws/config` or `$AWS_CONFIG_FILE`)
3. Credentials file (`~/.aws/credentials` or `$AWS_SHARED_CREDENTIALS_FILE`)
4. EC2 Instance Metadata Service (IMDSv2 preferred, falls back to IMDSv1)

### HTTP Client

Uses Gun (HTTP/1.1 and HTTP/2). One-shot request lifecycle:
- `request/6-8` opens connection, makes request, closes connection

### Retry Logic

`api_get_request/3` and `api_post_request/5` retry up to 5 times, sleeping between attempts for an equal-jitter exponential backoff interval: `Temp = min(10000, 500 * 2^Attempt)` ms, then a random delay in `[Temp/2, Temp]`. The delay is skipped after the final attempt, so a fully exhausted request waits at most 7.5s in total. Credentials are validated before the first attempt and re-validated before each retry. If credentials cannot be loaded, returns `{error, {credentials, Reason}}` immediately without retrying.

## Modules

| Module | Purpose |
|--------|---------|
| `aws_lib` | Main API: state management, request lifecycle, retries, EBS volume discovery |
| `aws_lib_config` | Credential/region loading from env, files, and IMDS; INI parsing; IMDSv2 token management |
| `aws_lib_sign` | AWS Signature Version 4: canonical request, signing key derivation, authorization header |
| `aws_lib_ops` | Higher-level EC2 operations: create/delete EBS snapshots |
| `aws_lib_json` | JSON decoding via thoas, converts to proplists with string keys |
| `aws_lib_xml` | XML parsing via xmerl, converts to nested proplists |
| `aws_lib_uri` | URI build/parse using `uri_string`, `#uri{}` record |

### Module Boundaries

- `aws_lib_config` works with `aws_config()` records directly - it does not know about `aws_state()`
- `aws_lib` wraps `aws_lib_config` results into `aws_state()`
- `aws_lib_sign` takes a `#request{}` record and returns signed headers
- `aws_lib_ops` calls `aws_lib:post/5` - it's a consumer of the core API

## Conventions

- `new/0` and `new/1` return `aws_state()` directly (not wrapped in `{ok, ...}`)
- `set_region/2`, `set_credentials/3-4` return `{ok, NewState}`
- `request/6-8`, `post/5-6`, `put/5-6` return `{ok, {Headers, Body}, NewState}` or `result_error()` (see `aws_lib.hrl` for `result_error()` type - includes 2-tuple and 3-tuple error forms)
- `api_get_request/3`, `api_post_request/5` return `{ok, Payload, NewState}` or `{error, term()}`
- All types are string-based (Erlang lists), not binaries - except HTTP bodies which are `iodata()`
- JSON responses are decoded to proplists with string keys and string values
- XML responses are decoded to nested proplists with string keys

## Build and Test

```bash
make              # Compile
make eunit        # Run all tests
make dialyze      # Dialyzer static analysis
make check        # Compile + test + dialyze
make clean        # Clean build artifacts
make distclean    # Clean everything including deps
```

Run a specific test module:
```bash
make t=aws_lib_tests eunit
```

## Dependencies

- **gun** (git, ninenines/gun) - HTTP client
- **thoas** (hex, v1.2.1) - JSON decoding
- **meck** (hex, v1.1.0) - Test mocking (TEST_DEPS only)
- **LOCAL_DEPS:** crypto, inets, ssl, xmerl, public_key

## Configuration

Application env `aws_prefer_imdsv2` (default: `true`) controls whether IMDSv2 is attempted before IMDSv1. Also exposed via Cuttlefish schema at `priv/schema/aws_lib.schema` as `aws.prefer_imdsv2` for RabbitMQ integration.

## Known Issues

1. **`aws_lib_all_tests.erl`** references `aws_lib_app_tests` and `aws_lib_sup_tests` which no longer exist (removed when OTP application structure was removed). The runner will fail if invoked directly; use `make eunit` instead.

2. **`include/aws_lib.hrl` lines 49-52** contain unused ETS table defines left over from the old architecture:
   ```erlang
   -define(AWS_CREDENTIALS_TABLE, aws_credentials).
   %% TODO LRB
   %% -define(AWS_CONFIG_TABLE, aws_config).
   ```
