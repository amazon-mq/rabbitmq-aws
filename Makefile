PROJECT = aws
PROJECT_DESCRIPTION = RabbitMQ - AWS integration plugin
PROJECT_MOD = aws_app
PROJECT_REGISTERED = aws_sup
PROJECT_VERSION = 0.3.0

define PROJECT_ENV
[]
endef

DEPS = rabbit_common rabbitmq_aws rabbit
TEST_DEPS = meck rabbitmq_ct_helpers rabbitmq_ct_client_helpers rabbitmq_auth_backend_ldap
LOCAL_DEPS = crypto inets ssl xmerl public_key eldap

PLT_APPS = rabbit

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

include ../../rabbitmq-components.mk
include ../../erlang.mk
