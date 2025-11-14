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
