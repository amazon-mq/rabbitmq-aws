PROJECT = aws
PROJECT_DESCRIPTION = RabbitMQ - AWS integration plugin
PROJECT_MOD = aws_app
PROJECT_REGISTERED = aws_sup
PROJECT_VERSION = 0.3.0

define PROJECT_ENV
[]
endef

DEPS = rabbit_common rabbit rabbitmq_management prometheus gun jose
TEST_DEPS = meck proper rabbitmq_ct_helpers rabbitmq_ct_client_helpers rabbitmq_auth_backend_ldap rabbitmq_auth_backend_http rabbitmq_auth_backend_oauth2
LOCAL_DEPS = crypto inets ssl xmerl public_key eldap

PLT_APPS = rabbit

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

include ../../rabbitmq-components.mk
include ../../erlang.mk

# Gate the optional OAuth authz layer on the arity-4 scope API it needs, not
# just oauth2.hrl existing: the header predates resource_access/4 and the
# scope_pattern_syntax field (both landed in the v4.2.0-beta series -- the
# supported-series floor), so a header-only guard would build against a missing
# function. scope_pattern_syntax in the resolved header is the sentinel for that
# API. When absent, the module still compiles (-else branch), available/0
# returns false, and authz_check reports config_conflict. Must follow erlang.mk
# so DEPS_DIR and ERLC_OPTS are set.
OAUTH2_HRL = $(DEPS_DIR)/rabbitmq_auth_backend_oauth2/include/oauth2.hrl
ifneq ($(wildcard $(OAUTH2_HRL)),)
ifneq ($(shell grep -l scope_pattern_syntax $(OAUTH2_HRL) 2>/dev/null),)
ERLC_OPTS += -DHAVE_OAUTH2_RESOURCE_SERVER=1
TEST_ERLC_OPTS += -DHAVE_OAUTH2_RESOURCE_SERVER=1
endif
endif
