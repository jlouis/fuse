PROJECT = fuse

app_eqc: ERLC_OPTS += -DEQC_TESTING
app_eqc: app

# Options.
CT_SUITES = fuse
CT_OPTS = -cover cover.spec
PLT_APPS = sasl


# Dependencies
DEPS = folsom
dep_folsom = https://github.com/boundary/folsom.git

include erlang.mk
