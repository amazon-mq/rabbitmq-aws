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

## Validation of AWS ARNs

See the [API documentation](API.md) for how you can use an HTTP API to
validate that ARNs resolve as expected.

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
