-module(fuse_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([
	simple_test/1,
	reset_test/1
]).

%% ct.
all() ->
	[
		simple_test,
		reset_test
	].
	
groups() ->
	[].
	
init_per_suite(Config) ->
	application:start(fuse),
	Config.
	
end_per_suite(_Config) ->
	application:stop(fuse),
	ok.
	
init_per_group(_Group, Config) ->
	Config.
	
end_per_group(_Group, _Config) ->
	ok.
	
%% Tests.

-define(FUSE_SIMPLE, simple_fuse).
simple_test(_Config) ->
	ct:log("Set up a new fuse, melt it and then verify it resets correctly"),
	ok = fuse:install(?FUSE_SIMPLE, {{standard, 2, 60}, {reset, 500}}),
	ok = fuse:ask(?FUSE_SIMPLE),
	ok = fuse:melt(?FUSE_SIMPLE),
	ok = fuse:ask(?FUSE_SIMPLE),
	ok = fuse:melt(?FUSE_SIMPLE),
	ok = fuse:ask(?FUSE_SIMPLE),
	ok = fuse:melt(?FUSE_SIMPLE),
	blown = fuse:ask(?FUSE_SIMPLE),
	ct:sleep(600),
	ok = fuse:ask(?FUSE_SIMPLE),
	ok.

-define(FUSE_RESET, reset_fuse).
reset_test(_Config) ->
	ct:log("Installing a fuse, then resetting it should clear out timers"),
	ok = fuse:install(?FUSE_RESET, {{standard, 2, 60}, {reset, 500}}),
	ok = fuse:ask(?FUSE_RESET),
	ok = fuse:melt(?FUSE_RESET),
	ok = fuse:melt(?FUSE_RESET),
	ok = fuse:melt(?FUSE_RESET),
	blown = fuse:ask(?FUSE_RESET),
	ok = fuse:reset(?FUSE_RESET),
	ok = fuse:ask(?FUSE_RESET),
	ok.