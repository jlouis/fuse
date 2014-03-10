PROJECT = fuse

# Options.
CT_SUITES = fuse
PLT_APPS = sasl

# Dependencies
DEPS = folsom
dep_folsom = https://github.com/boundary/folsom.git

include erlang.mk
