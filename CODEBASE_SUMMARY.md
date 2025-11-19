# RabbitMQ AWS Plugin - Codebase Summary

## Overview

The `rabbitmq-aws` plugin enables RabbitMQ to fetch sensitive configuration data (certificates, secrets, passwords) from AWS services at startup instead of storing them on disk. It resolves AWS ARNs specified in RabbitMQ configuration and injects the retrieved values directly into the application environment.

**Version:** 0.1.0  
**License:** Apache-2.0  
**Compatibility:** RabbitMQ 4.2.0+

## Core Architecture

### Application Structure

```
aws_app (OTP Application)
  └── aws_sup (Supervisor - currently empty)
  └── aws_arn_config (Boot Step Handler)
```

The plugin uses RabbitMQ's boot step mechanism to execute before networking starts, ensuring all ARN-based configuration is resolved before RabbitMQ services initialize.

### Key Design Principles

1. **No Disk Storage**: Retrieved certificates and secrets are never written to disk
2. **Boot-Time Resolution**: All ARNs are resolved during RabbitMQ startup
3. **Credential Management**: Supports IAM role assumption for cross-account access
4. **In-Memory Transformation**: Converts PEM data to DER format in memory for Erlang/OTP SSL

## Module Breakdown

### Core Configuration Modules

#### `aws_arn_config.erl`
**Purpose:** Orchestrates the entire ARN resolution process

**Key Functions:**
- `process_arns/0` - Entry point called during boot step
- `maybe_assume_role/1` - Handles optional IAM role assumption
- `run_arn_handlers/1` - Iterates through configured ARNs and resolves them

**Flow:**
1. Check for `aws.arn_config` in application environment
2. Optionally assume IAM role if `assume_role_arn` is configured
3. Resolve each ARN using appropriate AWS service
4. Invoke handler modules to update application configuration
5. Reset credentials if role was assumed

#### `aws_arn_util.erl`
**Purpose:** ARN parsing and resolution routing

**Key Functions:**
- `parse_arn/1` - Parses ARN string into structured map
- `resolve_arn/1` - Routes to appropriate AWS service based on ARN

**Supported ARN Formats:**
- `arn:aws:s3:::bucket/key` - S3 objects
- `arn:aws:secretsmanager:region:account:secret:name` - Secrets Manager
- `arn:aws:acm-pca:region:account:certificate-authority/id` - ACM Private CA
- `arn:aws-debug:file:::path` - Local files (testing only)

#### `aws_arn_env.erl`
**Purpose:** Application environment manipulation

**Key Functions:**
- `replace/3` - Simple key-value replacement
- `replace/5` - Complex replacement with key transformation

**Transformations:**
- `cacertfile` → `cacerts` (list of DER-encoded certificates)
- `certfile` → `certs_keys` (map with `cert` key)
- `keyfile` → `certs_keys` (map with `key` key)

These transformations align with Erlang/OTP SSL option requirements.

### AWS Service Integration Modules

#### `aws_s3.erl`
Fetches objects from S3 buckets using the `rabbitmq_aws` library.

#### `aws_sms.erl` (Secrets Manager)
Retrieves secrets from AWS Secrets Manager, supporting both `SecretString` and `SecretBinary` formats.

#### `aws_acm_pca.erl`
Fetches CA certificates from ACM Private Certificate Authority.

#### `aws_iam.erl`
Handles IAM role assumption via STS `AssumeRole` API, parsing XML responses and setting temporary credentials.

#### `aws_sts.erl`
Adds custom headers to STS requests (useful for cross-account scenarios).

### Configuration Handler Modules

Each handler module implements the `run/3` or `run/4` function to process resolved ARN data for specific RabbitMQ components:

#### `aws_arn_config_rabbit.erl`
Handles core RabbitMQ `ssl_options` configuration.

#### `aws_arn_config_management.erl`
Handles `rabbitmq_management` plugin SSL and OAuth client secrets.

#### `aws_arn_config_ldap.erl`
Handles `rabbitmq_auth_backend_ldap` SSL options and bind passwords.

#### `aws_arn_config_http.erl`
Handles `rabbitmq_auth_backend_http` SSL options.

#### `aws_arn_config_oauth2.erl`
Handles `rabbitmq_auth_backend_oauth2` HTTPS CA certificates, including per-provider configuration.

### Utility Modules

#### `aws_pem_util.erl`
**Purpose:** PEM to DER conversion

**Key Functions:**
- `decode_data/1` - Decodes PEM certificates to DER format
- `decode_key_data/1` - Decodes PEM private keys to DER format

**Supported Key Types:**
- RSAPrivateKey
- DSAPrivateKey
- ECPrivateKey
- PrivateKeyInfo

**Limitation:** Encrypted PEM files are not supported.

#### `aws_app_env.erl`
Low-level application environment manipulation (update/delete operations).

#### `aws_util.erl`
Credential management utilities, primarily for resetting AWS credentials after role assumption.

### Management API Module

#### `aws_arn_mgmt.erl`
**Purpose:** HTTP API for ARN validation

**Endpoint:** `PUT /api/aws/arn/validate`

**Request Format:**
```json
{
  "assume_role_arn": "arn:aws:iam::account:role/name",
  "arns": [
    "arn:aws:secretsmanager:region:account:secret:name"
  ]
}
```

**Response Format:**
```json
[
  {
    "arn": "arn:aws:secretsmanager:...",
    "value": "secret-value"
  }
]
```

**Features:**
- Validates ARN resolution without restarting RabbitMQ
- Supports role assumption for testing cross-account access
- Returns HTTP 422 for unprocessable ARNs
- Implements proper credential cleanup after validation

## Configuration Schema

The plugin uses Cuttlefish schema (`priv/schema/aws.schema`) to translate `rabbitmq.conf` settings into Erlang application environment.

### Supported Configuration Keys

**Core Settings:**
- `aws.prefer_imdsv2` - Prefer EC2 IMDSv2 for metadata (default: true)
- `aws.arns.assume_role_arn` - IAM role to assume before fetching resources

**RabbitMQ Core SSL:**
- `aws.arns.ssl_options.cacertfile`
- `aws.arns.ssl_options.certfile`
- `aws.arns.ssl_options.keyfile`

**Management Plugin:**
- `aws.arns.management.ssl.cacertfile`
- `aws.arns.management.ssl.certfile`
- `aws.arns.management.ssl.keyfile`
- `aws.arns.management.oauth_client_secret`

**Auth Backend HTTP:**
- `aws.arns.auth_http.ssl_options.{cacertfile,certfile,keyfile}`

**Auth Backend LDAP:**
- `aws.arns.auth_ldap.ssl_options.{cacertfile,certfile,keyfile}`
- `aws.arns.auth_ldap.dn_lookup_bind.password`
- `aws.arns.auth_ldap.other_bind.password`

**Auth Backend OAuth2:**
- `aws.arns.auth_oauth2.https.cacertfile`
- `aws.arns.auth_oauth2.oauth_providers.$name.https.cacertfile`

**STS Custom Headers:**
- `aws.sts.custom_headers.$header` - Custom headers for STS calls

### Schema Translation Logic

The schema performs complex translation to build a list of tuples:
```erlang
[{Module, Arn, SchemaKey, Args}, ...]
```

Each tuple represents:
- **Module**: Handler module to invoke
- **Arn**: ARN string to resolve (or `undefined` for special cases)
- **SchemaKey**: Configuration key for error reporting
- **Args**: Arguments to pass to handler's `run/N` function

## Data Flow

### Startup Sequence

```
1. RabbitMQ Boot
   ↓
2. aws_app:boot_step(aws_arn_config)
   ↓
3. aws_arn_config:process_arns()
   ↓
4. Load aws.arn_config from application environment
   ↓
5. [Optional] aws_iam:assume_role()
   ↓
6. For each ARN:
   a. aws_arn_util:resolve_arn()
   b. Route to AWS service module
   c. Fetch data from AWS
   d. Invoke handler module
   e. Update application environment
   ↓
7. [Optional] Reset AWS credentials
   ↓
8. RabbitMQ continues boot (networking enabled)
```

### ARN Resolution Flow

```
ARN String
   ↓
aws_arn_util:parse_arn()
   ↓
Service Detection (s3, secretsmanager, acm-pca)
   ↓
Service Module (aws_s3, aws_sms, aws_acm_pca)
   ↓
rabbitmq_aws:api_get_request() or api_post_request()
   ↓
Raw Data (PEM, JSON, etc.)
   ↓
Handler Module (aws_arn_config_*)
   ↓
aws_pem_util:decode_data() [if PEM]
   ↓
aws_arn_env:replace()
   ↓
application:set_env()
```

## Testing Strategy

### Unit Tests (EUnit)

**`aws_arn_config_tests.erl`:**
- ARN parsing validation
- S3 ARN format handling
- Nested path support
- Invalid ARN detection
- Environment replacement logic

**`aws_app_tests.erl`:**
- Application start/stop behavior
- Supervisor initialization (using meck)

### Integration Tests (Common Test)

**`aws_arn_mgmt_SUITE.erl`:**
- HTTP API endpoint validation
- Method restrictions (only PUT and OPTIONS allowed)
- Error handling (400, 405, 422 responses)
- Malformed ARN handling
- Empty ARN list validation

**`config_schema_SUITE.erl`:**
- Cuttlefish schema validation
- Configuration snippet testing

### Test Infrastructure

- Uses `meck` for mocking
- Uses `rabbitmq_ct_helpers` for RabbitMQ-specific test utilities
- Includes test data in `test/config_schema_SUITE_data/`

## Dependencies

### Runtime Dependencies
- `rabbit_common` - RabbitMQ common libraries
- `rabbitmq_aws` - AWS API client library
- `rabbit` - RabbitMQ core
- `rabbitmq_management` - Management plugin (for HTTP API)

### Build Dependencies
- `meck` - Mocking framework
- `rabbitmq_ct_helpers` - Common Test helpers
- `rabbitmq_ct_client_helpers` - Client test utilities

### Standard Library Dependencies
- `crypto` - Cryptographic functions
- `inets` - HTTP client
- `ssl` - SSL/TLS support
- `xmerl` - XML parsing (for STS responses)
- `public_key` - PEM/DER encoding

## Build System

**Build Tool:** erlang.mk (RabbitMQ plugin framework)

**Key Targets:**
- `make` - Build plugin
- `make tests` - Run all tests
- `make dialyzer` - Type checking

**Project Metadata:**
- Project name: `aws`
- Module: `aws_app`
- Registered process: `aws_sup`
- Version: 0.1.0

## CI/CD

**GitHub Actions Workflows:**

1. **build-test.yaml** - Main CI pipeline
   - Tests against RabbitMQ versions: v3.13.7, v4.2.x, main
   - Matrix builds with different OTP/Elixir versions
   - Runs daily at 16:00 UTC
   - Caches RabbitMQ server builds

2. **build-rabbitmq-server.yaml** - Builds RabbitMQ server for testing

3. **format-check.yaml** - Code formatting validation

## Security Considerations

1. **No Disk Persistence**: Secrets never touch the filesystem
2. **Memory-Only Processing**: PEM to DER conversion happens in memory
3. **Credential Cleanup**: Temporary credentials are reset after use
4. **No Encrypted PEM Support**: Encrypted private keys are not supported (design choice)
5. **IAM Role Assumption**: Supports cross-account access patterns
6. **IMDSv2 Preference**: Defaults to more secure EC2 metadata service version

## Error Handling

### Error Propagation
- Errors during ARN resolution prevent RabbitMQ startup
- Detailed error messages include ARN and configuration key
- Stacktraces logged for debugging

### Error Types
- `{invalid_arn_format, _}` - Malformed ARN string
- `{unsupported_service, Service}` - ARN service not implemented
- `{assume_role_failed, _}` - IAM role assumption failed
- `{invalid_pem_data, _}` - PEM decoding failed
- `{error_decoding_certs, _}` - Certificate parsing failed

### HTTP API Errors
- **400 Bad Request** - Invalid JSON or unexpected error
- **405 Method Not Allowed** - Wrong HTTP method
- **422 Unprocessable Entity** - Valid JSON but invalid ARN

## Logging

Uses Erlang's `logger` framework with custom macros defined in `aws.hrl`:

- `?AWS_LOG_DEBUG` - Debug-level logging
- `?AWS_LOG_INFO` - Informational messages
- `?AWS_LOG_WARNING` - Warning messages
- `?AWS_LOG_ERROR` - Error messages

All log messages include module name for traceability.

## Extension Points

### Adding New AWS Services

To support a new AWS service:

1. Create service module (e.g., `aws_newservice.erl`)
2. Implement `fetch_*/N` function
3. Add service detection in `aws_arn_util:resolve_arn/1`
4. Update documentation

### Adding New Configuration Keys

To support a new RabbitMQ configuration key:

1. Add mapping in `priv/schema/aws.schema`
2. Create or update handler module in `aws_arn_config_*.erl`
3. Add to translation function in schema
4. Add tests

## Known Limitations

1. **No Encrypted PEM Support**: Private keys must be unencrypted
2. **Startup-Only Resolution**: ARNs are resolved once at startup, not dynamically
3. **No Certificate Rotation**: Requires RabbitMQ restart to refresh certificates
4. **Single Region per ARN**: Each ARN specifies its own region
5. **No Caching**: Each startup fetches fresh data from AWS

## Future Considerations

Based on the codebase structure, potential enhancements could include:

1. **Dynamic Refresh**: Periodic re-resolution of ARNs without restart
2. **Certificate Rotation**: Automatic certificate renewal
3. **Caching Layer**: Local caching with TTL
4. **Additional Services**: Support for AWS Systems Manager Parameter Store
5. **Encrypted PEM**: Support for password-protected private keys
6. **Metrics**: Expose ARN resolution metrics via management API

## Development Workflow

### Local Development
1. Clone into RabbitMQ server deps directory
2. Run `make` to build
3. Run `make tests` to test
4. Use `make dialyzer` for type checking

### Testing with RabbitMQ
The plugin integrates with RabbitMQ's test infrastructure and requires a full RabbitMQ server build for integration testing.

### Code Style
- Erlang/OTP conventions
- Vim modelines for consistent formatting
- Copyright headers on all files
- Type specifications where applicable

## Documentation

- **README.md** - User-facing documentation
- **API.md** - HTTP API documentation
- **CONTRIBUTING.md** - Contribution guidelines
- **RELEASE.md** - Release process
- **CHANGELOG.md** - Version history

## Summary

This plugin is a well-architected solution for integrating RabbitMQ with AWS infrastructure services. It follows Erlang/OTP design principles, uses RabbitMQ's extension mechanisms properly, and maintains security by avoiding disk storage of sensitive data. The codebase is modular, testable, and designed for extension while maintaining a focused scope on AWS ARN resolution at startup time.
