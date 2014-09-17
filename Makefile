PROJECT = fuse

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
