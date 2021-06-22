PROJECT = fuse

.PHONY: all
all:
	rebar3 compile

.PHONY: tests
tests:
	rebar3 ct

.PHONY: app_eqc
app_eqc: all
	erlc -DEQC_TESTING test/*.erl
