PROJECT = aws
PROJECT_DESCRIPTION = RabbitMQ - AWS integration plugin
PROJECT_MOD = aws_app
PROJECT_REGISTERED = aws_sup
PROJECT_VERSION = 0.3.0

define PROJECT_ENV
[]
endef

# aws depends on aten directly (aws_node_health_worker calls aten_sink). ra
# also declares dep_aten, so keep this pin in sync with rabbitmq/ra's Makefile
# to avoid a version-set conflict between the two declarations.
dep_aten = hex 0.6.0 aten
DEPS = rabbit_common rabbit rabbitmq_management rabbitmq_prometheus gun jose aten
TEST_DEPS = meck proper rabbitmq_ct_helpers rabbitmq_ct_client_helpers rabbitmq_auth_backend_ldap rabbitmq_auth_backend_http rabbitmq_auth_backend_oauth2
LOCAL_DEPS = crypto inets ssl xmerl public_key eldap

PLT_APPS = rabbit

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

include ../../rabbitmq-components.mk

# Precompile the auth-validation UI's EJS templates into priv/www/js/aws-ejs.js
# at build time. rabbitmq-management-plugin.mk (which provides the
# compile-ejs-templates target and the `app::` build hook) only exists on
# RabbitMQ main/v4.3.x, where the management UI stopped compiling .ejs at
# runtime. On the older supported series (v4.2.x, v3.13.x) the fragment is
# absent and the UI still uses the runtime EJS loader, so we must NOT reference
# it: erlang.mk consumes DEP_PLUGINS via `-include`, which silently no-ops when
# the file is missing -- leaving web_ui/0 pointing at an aws-ejs.js that never
# gets generated. Gate on the fragment actually being present. DEPS_DIR is set
# by rabbitmq-components.mk (above) and DEP_PLUGINS is consumed by erlang.mk
# (below), so this must sit between the two includes.
MGMT_EJS_MK = $(DEPS_DIR)/rabbitmq_management/mk/rabbitmq-management-plugin.mk
ifneq ($(wildcard $(MGMT_EJS_MK)),)
DEP_PLUGINS += rabbitmq_management/mk/rabbitmq-management-plugin.mk
endif

include ../../erlang.mk

# Tell aws_auth_validate_mgmt:web_ui/0 whether the precompiled EJS bundle
# (aws-ejs.js) will be produced. It exists only when the management EJS
# precompile fragment is available (same gate as DEP_PLUGINS above); on the
# older series the runtime .ejs loader is used and the bundle must not be
# registered. Must follow erlang.mk so ERLC_OPTS is set.
ifneq ($(wildcard $(MGMT_EJS_MK)),)
ERLC_OPTS += -DHAVE_MGMT_EJS_PRECOMPILE=1
TEST_ERLC_OPTS += -DHAVE_MGMT_EJS_PRECOMPILE=1
endif

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
