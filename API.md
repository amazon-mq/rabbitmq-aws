# `aws` plugin API

## ARN validation HTTP API

This plugin provides the following HTTP endpoint to validate that AWS ARNs can
be resolved:

```
/api/aws/arn/validate
```

To use the API, make a `content-type: application/json` `HTTP PUT` request with
a JSON body in this form:

```
{
    arns: [
        "arn:aws:secretsmanager:us-east-1:999999999999:secret:the-secret-AAAA",
        "arn:aws:secretsmanager:us-east-1:999999999999:secret:another-secret-BBBB"
    ]
}
```

Here is an example that uses `curl` that pretty-prints the result using `jq`:

```
curl -ksu 'user:password' -XPUT -H 'content-type: application/json' \
    https://b-1c458ac3-0781-465c-8687-52d79cb3c934-1.mq.us-east-1.amazonaws.com:443/api/aws/arn/validate \
    -d {"arns":["arn:aws:secretsmanager:us-west-2:888888888888:secret:rabbitmq-ldap-password-gCv56n"]}' | jq '.'
```

The response will contain an array of objects containing the original ARN as
well as the value:

```
[
    {
        arn: "arn:aws:secretsmanager:us-east-1:999999999999:secret:the-secret-AAAA",
        value: "foobar"
    },
    {
        arn: "arn:aws:secretsmanager:us-east-1:999999999999:secret:another-secret-BBBB",
        value: "bazbat"
    }
]
```

### Assume Role

This API allows an ARN to be used to assume a role prior to resolving the ARNs
in the JSON. Here is the structure of such a request:

```
{
    assume_role_arn: "arn:aws:iam::500000000000:role/AmazonMqRabbitMqArnRole",
    arns: [
        "arn:aws:secretsmanager:us-east-1:999999999999:secret:the-secret-AAAA",
        "arn:aws:secretsmanager:us-east-1:999999999999:secret:another-secret-BBBB"
    ]
}
```

## Auth backend validation HTTP API

This plugin can validate an auth backend configuration end-to-end without
restarting the broker. Make a `content-type: application/json` `HTTP PUT`
request to:

```
/api/aws/auth/validate/:method
```

where `:method` selects the backend. The only method currently supported is
`ldap`. The endpoint is disabled by default and must be enabled
with `aws.auth_validation.enabled = true`; access requires an authenticated
management user with the configured tag (`administrator` by default).

A successful validation returns **`204 No Content`**. Any failure returns a
JSON body of the form `{"error": "<category>", "message": "<fixed message>"}`.
The response never echoes credentials, DNs, server hostnames, or raw LDAP
errors — only a fixed category so the endpoint cannot be used to probe
infrastructure.

### LDAP (`ldap`)

The request validates three layers, each building on the previous one:

1. **Authentication** — opens an ephemeral connection (optionally over LDAPS
   or StartTLS) and performs a simple bind.
2. **DN lookup** *(optional)* — when `dn_lookup_base` is supplied, confirms
   the base DN exists and is readable by the bound user.
3. **Authorization queries** *(optional)* — parses each query in `queries`
   with the same grammar as `rabbitmq_auth_backend_ldap`, then confirms that
   any group/DN referenced with a *literal* DN (one containing no `${...}`
   placeholder) exists and is readable. Query terms that depend on a runtime
   principal (e.g. `${username}`) are validated for grammar only.

```
{
    "servers": ["ldap.example.com"],
    "port": 636,
    "user_dn": "cn=admin,dc=example,dc=com",
    "password_arn": "arn:aws:secretsmanager:us-east-1:999999999999:secret:ldap-bind-AAAA",
    "use_ssl": true,
    "ssl_options": {
        "verify": "verify_peer",
        "cacertfile_arn": "arn:aws:s3:::my-bucket/ca.pem"
    },
    "dn_lookup_base": "ou=users,dc=example,dc=com",
    "dn_lookup_attribute": "uid",
    "queries": {
        "tags": "[{administrator, {in_group, \"cn=admins,ou=groups,dc=example,dc=com\"}}]",
        "vhost_access": "{in_group, \"cn=rabbitmq,ou=groups,dc=example,dc=com\"}",
        "resource_access": "{constant, true}",
        "topic_access": "{constant, true}"
    }
}
```

Example with `curl`:

```
curl -ksu 'user:password' -XPUT -H 'content-type: application/json' \
    https://broker.example:443/api/aws/auth/validate/ldap \
    -d @request.json
```

#### Response categories

| HTTP status | `error` category | Meaning |
|---|---|---|
| 204 | _(none)_ | All configured layers validated successfully |
| 400 | `input_invalid` | A field was missing or had the wrong type |
| 400 | `connection_failed` | Could not open a connection to the server |
| 400 | `tls_failed` | TLS/StartTLS handshake failed |
| 400 | `query_invalid` | An authorization query string was not valid |
| 422 | `config_conflict` | `use_ssl` and `use_starttls` were both true |
| 422 | `auth_failed` | The simple bind was rejected |
| 422 | `authz_unverified` | A referenced base/group DN does not exist or is not readable |
