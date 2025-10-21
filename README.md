## RabbitMQ AWS infrastructure Plugin

This plugin is specifically for RabbitMQ features that integrate with AWS infrastructure services. If a feature doesn't require AWS services,
it belongs in [rabbitmq-server](https://github.com/rabbitmq/rabbitmq-server) or other appropriate repositories instead.

While this project lives in the `amazon-mq` GitHub organization, it's designed for anyone running RabbitMQ on AWS, not just Amazon MQ users. We
welcome contributions that help the community run RabbitMQ on AWS. The best features are ones that solve problems many users face when
deploying RabbitMQ on AWS infrastructure.

## Code Formatting

This project uses [erlfmt](https://github.com/WhatsApp/erlfmt) for consistent Erlang code formatting. All code must pass `erlfmt -c` checks before merging. PRs that fail erlfmt checks will not be merged.

To format your code before submitting:
```bash
erlfmt -w .
```

## Contributing

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## Build

```
git clone https://github.com/rabbitmq/rabbitmq-server.git
cd rabbitmq-server
git clone https://github.com/amazon-mq/rabbitmq-aws.git deps/aws
make -C deps/aws
```

## Run

```
cd path/to/rabbitmq-server
make PLUGINS='rabbitmq_management aws' run-broker
```

## License

This project is licensed under the Apache-2.0 License.
