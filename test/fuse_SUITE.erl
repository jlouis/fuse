-module(fuse_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([simple_test/1]).

%% ct.
all() ->
	[
		simple_test
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

simple_test(_Config) ->
	ct:log("Set up a new fuse, melt it and then verify it resets correctly"),
	ok = fuse:install(test_fuse, {{standard, 2, 60}, {reset, 500}}),
	ok = fuse:ask(test_fuse),
	ok = fuse:melt(test_fuse),
	ok = fuse:ask(test_fuse),
	ok = fuse:melt(test_fuse),
	ok = fuse:ask(test_fuse),
	ok = fuse:melt(test_fuse),
	blown = fuse:ask(test_fuse),
	ct:sleep(600),
	ok = fuse:ask(test_fuse),
	ok.
