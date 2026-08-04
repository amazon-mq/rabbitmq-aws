<!-- vim:tw=125
-->
# RabbitMQ AWS infrastructure Plugin

[![CI](https://github.com/amazon-mq/rabbitmq-aws/actions/workflows/build-test.yaml/badge.svg)](https://github.com/amazon-mq/rabbitmq-aws/actions/workflows/build-test.yaml)

This plugin is specifically for RabbitMQ features that integrate with AWS
infrastructure services. If a feature doesn't require AWS services, it belongs
in [rabbitmq-server](https://github.com/rabbitmq/rabbitmq-server) or other
appropriate repositories instead.

While this project lives in the `amazon-mq` GitHub organization, it's designed
for anyone running RabbitMQ on AWS, not just Amazon MQ users. We welcome
contributions that help the community run RabbitMQ on AWS. The best features
are ones that solve problems many users face when deploying RabbitMQ on AWS
infrastructure.

# Requirements

This plugin is compatible with RabbitMQ version 4.2.0 or later.

# Current Capabilities

## Configuration via AWS ARN

This plugin enables AWS ARNs to be specified directly in RabbitMQ configuration
instead of hardcoding sensitive values or values that require access to local
filesystem. It automatically resolves ARNs at startup and replaces
configuration values with actual content from AWS services. Resolved ARN
content, such as X509 certificates, **is not stored on disk** - it's passed
directly to RabbitMQ.

### Supported AWS Services & APIs

- **AWS Secrets Manager** (`GetSecretValue`) - Recommended for passwords and private keys
- **Amazon S3** (`GetObject`) - Recommended for public keys, certificate files and configuration files
- **ACM Private CA** (`GetCertificateAuthorityCertificate`) - Recommended for CA certificates
- **AWS STS** (`AssumeRole`) - Recommended for cross-account access

### ARN Resolution Methods

The plugin resolves AWS credentials using one of the following methods:

- **Assume Role** - If `aws.arns.assume_role_arn` is configured, assumes the
  specified IAM role before resolving ARNs

- **Environment Credentials** - If assume role is not configured, uses default
  AWS credential chain (EC2 IMDSv2, environment variables, credential files)

### New Configuration Keys

This plugin introduces new configuration keys that mirror existing RabbitMQ
configuration keys but with the `aws.arns.` prefix. These keys accept AWS ARNs
instead of literal values:

- `aws.arns.ssl_options.cacertfile`
- `aws.arns.ssl_options.certfile`
- `aws.arns.ssl_options.keyfile`
- `aws.arns.amqp_client.ssl_options.cacertfile`
- `aws.arns.amqp_client.ssl_options.certfile`
- `aws.arns.amqp_client.ssl_options.keyfile`
- `aws.arns.amqp10_client.ssl_options.cacertfile`
- `aws.arns.amqp10_client.ssl_options.certfile`
- `aws.arns.amqp10_client.ssl_options.keyfile`
- `aws.arns.management.ssl.cacertfile`
- `aws.arns.management.ssl.certfile`
- `aws.arns.management.ssl.keyfile`
- `aws.arns.management.oauth_client_secret`
- `aws.arns.auth_http.ssl_options.cacertfile`
- `aws.arns.auth_http.ssl_options.certfile`
- `aws.arns.auth_http.ssl_options.keyfile`
- `aws.arns.auth_ldap.ssl_options.cacertfile`
- `aws.arns.auth_ldap.ssl_options.certfile`
- `aws.arns.auth_ldap.ssl_options.keyfile`
- `aws.arns.auth_ldap.dn_lookup_bind.password`
- `aws.arns.auth_ldap.other_bind.password`
- `aws.arns.auth_oauth2.https.cacertfile`
- `aws.arns.auth_oauth2.oauth_providers.$name.https.cacertfile`

### Example

Here is an example `rabbitmq.conf` that configures RabbitMQ's `ssl_options` via AWS ARNs:

```
aws.arns.ssl_options.cacertfile = arn:aws:s3:::private-ca-42/cacertfile.pem
aws.arns.ssl_options.certfile = arn:aws:s3:::private-ca-42/server_certficate.pem
aws.arns.ssl_options.keyfile = arn:aws:s3:::private-ca-42/server_key.pem
```

The above configuration will fetch the data from S3 and configure RabbitMQ as
though the X509 certificates were present on the local filesystem, without
writing any data to disk. The `cacertfile` setting will be translated to the
equivalent
[`cacerts`](https://www.erlang.org/doc/apps/ssl/ssl.html#t:server_option_cert/0)
setting, and `certfile` / `keyfile` translated into the equivalent
[`certs_keys`](https://www.erlang.org/doc/apps/ssl/ssl.html#t:common_option_cert/0)
setting.

**NOTE:** encrypted X509 certificates are _not_ supported at this time.

## Auth Configuration Validation

Validating an authentication backend configuration on a running broker has
traditionally meant editing `rabbitmq.conf`, restarting the broker, observing
the result, and repeating -- an expensive guess-restart-guess loop. This plugin
adds a synchronous HTTP endpoint that answers whether a new auth config is viable
_before_ it is applied, returning an immediate categorized result instead.

```
PUT /api/aws/auth/validate/:method
```

The `:method` path segment selects the backend to validate. Supported methods:

- `ldap` - LDAP simple bind, optional StartTLS, optional post-bind DN lookup and
  authorization-query checks (mirrors `rabbitmq_auth_backend_ldap`).
- `http` - HTTP auth backend reachability, including mTLS (client
  certificate/key) to the auth service.
- `oauth` - OAuth2/OIDC: JWKS reachability and, when an access token is
  supplied, signature and `exp`/`nbf`/`aud` verification plus optional
  scope/authorization evaluation.
- `tls` - TLS handshake and certificate-chain validation against a target,
  including certificate-login (`EXTERNAL`) checks.

The request body is a JSON object carrying the backend configuration to test.
Each backend accepts only the fields relevant to it (see the per-method
`allowed_fields` in the corresponding `aws_auth_validate_*` module); any other
fields are ignored. Secret material (such as an LDAP bind password) is supplied
by _reference_ as a `password_arn` and resolved through AWS Secrets Manager
(see "Configuration via AWS ARN" above), never passed inline.

### Responses

A successful validation returns **204 No Content**. Failures return a fixed set
of categories -- deliberately coarse so the endpoint never leaks server
hostnames, DNs, or raw backend errors -- as a JSON body `{"error": ..., "message": ...}`:

| Status | Categories |
|---|---|
| 400 | `input_invalid`, `body_too_large`, `connection_failed`, `tls_failed`, `query_invalid` |
| 401 | `insufficient_user_tag` |
| 404 | `unknown_method`, `method_disabled` |
| 422 | `auth_failed`, `config_conflict`, `authz_unverified`, `token_expired`, `token_invalid` |
| 503 | `capacity_exhausted` |

### Access control

The endpoint is gated on the RabbitMQ management API and, by default, requires
the `administrator` user tag. It performs outbound connections to
operator-supplied targets, so treat access to it accordingly.

### Enabling the endpoint

The feature is **opt-in and disabled by default**. Two levels of toggle are
required: a master switch that starts the subsystem, and a per-method switch for
each backend you want to validate. When disabled, the endpoint returns 404.

```
# Start the validation subsystem (required)
aws.auth_validation.enabled = true

# Enable individual methods (each is opt-in; enable only what you need)
aws.auth_validation.enabled_methods.ldap = true
aws.auth_validation.enabled_methods.http = true
aws.auth_validation.enabled_methods.oauth = true
aws.auth_validation.enabled_methods.tls = true
```

Additional tuning keys (each has an effective default applied in code, so the
key only needs to be set to override it):

| Key | Effective default | Purpose |
|---|---|---|
| `aws.auth_validation.max_body_size` | `65536` | Maximum request body size, in bytes (1..1048576). |
| `aws.auth_validation.max_concurrent` | `5` | Maximum concurrent outbound validation connections (1..100). |
| `aws.auth_validation.connection_timeout_ms` | `5000` | Per-connection timeout for outbound calls, in ms (1..60000). |
| `aws.auth_validation.required_user_tag` | `administrator` | Management user tag required to call the endpoint. |

When enabled, the plugin also registers an admin-gated **Auth Validation** tab
in the RabbitMQ management console UI that drives the same endpoint.

## Installation

Visit the [GitHub Releases](https://github.com/amazon-mq/rabbitmq-aws/releases)
page for this project to download the `ez` file for this plugin. Then, copy the
`ez` file to the [correct location](https://www.rabbitmq.com/docs/plugins#plugin-directories) for your
RabbitMQ broker to find it. Finally, enable the plugin as described
[in the official documentation](https://www.rabbitmq.com/docs/plugins#ways-to-enable-plugins).

## Build

See [CONTRIBUTING](CONTRIBUTING.md#build) for more information.

## Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for more information.

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This project is licensed under the Apache-2.0 License.
