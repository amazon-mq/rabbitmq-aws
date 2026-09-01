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
_before_ it is applied:

```
PUT /api/aws/auth/validate/:method
```

Supported `:method` values are `ldap`, `http`, `oauth`, and `tls`. A viable
config returns **204 No Content**; problems come back as a fixed set of
categorized error responses. The endpoint is admin-gated, opt-in, and disabled
by default. Secret material (bind passwords, certificates) is supplied by ARN
reference and resolved server-side rather than passed in the request body.

See **[AUTH_VALIDATION.md](AUTH_VALIDATION.md)** for the full reference: the
per-method behaviour, request fields, response categories, security model, and
the configuration keys needed to enable it.

## Node Health Detection

A partially-degraded cluster node -- one whose host or uplink drops a large
fraction of packets while the node still answers and participates -- is hard to
spot with reachability-based health checks. This plugin turns the broker's
existing node failure detector into per-node Prometheus metrics that attribute
a degraded node so it can be replaced early:

```
rabbitmq_aws_node_health_peer_down_probability{peer="..."}
rabbitmq_aws_node_health_peer_down_suspected{peer="..."}
rabbitmq_aws_node_health_peer_down_confidence{peer="..."}
rabbitmq_aws_node_health_cluster_congested
```

Each node samples its per-peer reachability, gossips it to the others so every
node holds the full picture, and decides whether one node is degraded, or
whether the condition is cluster-wide (and so must not be blamed on a single
node). A dumb alarm can threshold `rabbitmq_aws_node_health_peer_down_suspected` to catch a
single degraded node, or watch the unlabelled `rabbitmq_aws_node_health_cluster_congested`
(1 when the condition is symmetric across the cluster) for uniform congestion.
The feature is admin-neutral, opt-in, and disabled by default.

See **[NODE_HEALTH.md](NODE_HEALTH.md)** for the full reference: the metrics, the
detection logic, and the configuration keys needed to enable it.

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
