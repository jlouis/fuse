PROJECT = fuse

ERLC_OPTS := +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard

.DEFAULT_GOAL := app

app_eqc: ERLC_OPTS += -DEQC_TESTING
app_eqc: app

# Options.
CT_SUITES = fuse
PLT_APPS = sasl

# Dependencies
DEPS = folsom bear
dep_folsom = https://github.com/boundary/folsom.git
dep_bear = https://github.com/boundary/bear.git

include erlang.mk
