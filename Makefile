PROJECT = fuse

.PHONY: all
all:
	rebar3 compile

.PHONY: tests
tests:
	rebar3 ct
