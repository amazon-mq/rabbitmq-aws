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
  supplied, signature and `exp`/`aud` verification plus optional
  scope/authorization evaluation. `nbf` is deliberately not checked, matching
  `rabbit_auth_backend_oauth2`, which validates `exp` alone; a post-dated token
  therefore passes.
- `tls` - offline validation of the broker's own inbound TLS/mTLS material:
  the CA bundle ARN resolves, holds well-formed and currently-valid
  certificates, and the `verify` / `fail_if_no_peer_cert` / `depth` / `versions`
  options are well-shaped, plus optional certificate-login (`EXTERNAL`) checks.
  Unlike the other methods this one performs no handshake and connects to
  nothing: `target` selects which local listener's material to check
  (`listener` or `management`), not a remote endpoint. A pass means the material
  is usable, not that a live handshake against a running broker succeeds.

The request body is a JSON object carrying the backend configuration to test.
Each backend accepts only the fields relevant to it (see the per-method
`allowed_fields` in the corresponding `aws_auth_validate_*` module); any other
_top-level_ field is silently dropped. This does not extend to nested
`ssl_options`: an unrecognized key inside that object is rejected with 400
`input_invalid` rather than ignored.

Secret material is supplied by _reference_ as an ARN and resolved server-side
(see "Configuration via AWS ARN" above), so it does not transit the request
body. An LDAP bind `password_arn` resolves through AWS Secrets Manager;
certificate material (`cacertfile_arn`, `certfile_arn`, `keyfile_arn`) resolves
through whichever service the ARN names, which may be S3, Secrets Manager, or
ACM Private CA. The one exception is the `oauth` method's optional
`access_token`, which is passed inline: it is a short-lived token the caller
minted out of band, so no long-lived secret is involved.

### Responses

A successful validation returns **204 No Content**. Failures return a fixed set
of categories -- deliberately coarse so the endpoint never leaks server
hostnames, DNs, or raw backend errors -- as a JSON body `{"error": ..., "message": ...}`:

| Status | Categories |
|---|---|
| 400 | `input_invalid`, `body_too_large`, `connection_failed`, `tls_failed`, `query_invalid` |
| 401 | `not_authorised` or `insufficient_user_tag` (different body shape, see below) |
| 404 | `unknown_method`, `method_disabled` |
| 422 | `auth_failed`, `config_conflict`, `authz_unverified`, `token_expired`, `token_invalid` |
| 500 | `internal_error` |
| 503 | `capacity_exhausted` |

`internal_error` signals a fault in the endpoint itself, not a problem with the
submitted configuration; a genuinely unreachable server is reported as a 400
`connection_failed`.

Authorization failures are the one case that does not use the shape above,
because they are produced by the management plugin before this endpoint's code
runs. Under the default `required_user_tag = administrator`, a caller without
that tag gets **401** with a `reason` field rather than a `message` one:

```json
{"error": "not_authorised", "reason": "Not administrator user"}
```

Only when an operator lowers `required_user_tag` to some other tag does a missing
tag produce this plugin's own **401**:

```json
{"error": "insufficient_user_tag", "message": "User does not have required tag"}
```

Clients should therefore treat any 401 as "not permitted" rather than matching on
a specific category.

### Access control

The endpoint is gated on the RabbitMQ management API and, by default, requires
the `administrator` user tag (see `required_user_tag` below to require a
different one). The `ldap`, `http` and `oauth` methods make outbound connections
to caller-supplied targets, and every method can cause the broker to assume the
configured role and fetch ARN-backed material, so treat access to it
accordingly.

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

**The master switch is read only at boot, so enabling it requires a broker
restart.** The subsystem's worker is added to the supervision tree when the node
starts; setting `aws.auth_validation.enabled = true` and reloading
configuration without restarting leaves no worker running, and every request
then returns 503 `capacity_exhausted` with "Validation service is not ready;
broker restart required". The per-method switches are read per request and do
not require a restart.

The `ldap` and `tls` methods additionally require an assume-role ARN, because
both always resolve at least one ARN (an LDAP `password_arn`, a TLS
`cacertfile_arn`) and the endpoint deliberately never falls back to the broker's
own instance role for that:

```
aws.arns.assume_role_arn = arn:aws:iam::111122223333:role/rabbitmq-auth-validation
```

Without it, requests to those two methods fail with 422 `config_conflict` even
when otherwise correct. The `http` and `oauth` methods need it only when the
request references ARN-backed TLS material.

Additional tuning keys (each has an effective default applied in code, so the
key only needs to be set to override it):

| Key | Effective default | Accepted range | Purpose |
|---|---|---|---|
| `aws.auth_validation.max_body_size` | `65536` | 1..1048576 | Maximum request body size, in bytes. |
| `aws.auth_validation.max_concurrent` | `5` | 1..100 | Maximum concurrent outbound validation connections. |
| `aws.auth_validation.connection_timeout_ms` | `5000` | 1..60000 | Per-connection timeout for outbound calls, in ms. |
| `aws.auth_validation.required_user_tag` | `administrator` | any tag | Management user tag required to call the endpoint. |

**A value outside the accepted range is silently replaced by the effective
default, not rejected.** These bounds are enforced in code rather than by the
Cuttlefish schema, so an out-of-range setting produces no configuration error
and no log entry. Setting `max_body_size = 2000000`, for example, leaves the
limit at `65536`, and a 200 KB body is then rejected as `body_too_large` with
nothing pointing at the ignored setting.

The plugin also registers an **Auth Validation** tab in the RabbitMQ management
console UI that drives the same endpoint. Note that the tab is registered
whenever the plugin itself is enabled, independently of the toggles above: with
the feature disabled the tab is still visible to administrators, and submissions
from it return 404 `method_disabled`. Its visibility is hard-coded to the
`administrator` tag, so lowering `required_user_tag` does not make the tab
appear for users holding the lowered tag, even though the endpoint accepts
their requests.

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
