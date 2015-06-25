PROJECT = fuse

rebar3-compile:
	rebar3 compile | sed -e 's|_build/default/lib/fuse/||'

ERLC_OPTS := +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard

.DEFAULT_GOAL := app

app_eqc: ERLC_OPTS += -DEQC_TESTING
app_eqc: app

# Options.
CT_SUITES = fuse
PLT_APPS = sasl
COMPILE_FIRST = fuse_stats_plugin

include erlang.mk
