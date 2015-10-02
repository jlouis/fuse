-module(fuse_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([
	simple_test/1,
	reset_test/1,
	remove_test/1
]).

-ifdef(EQC_TESTING).
-define(EQC_PRESENT, true).
-else.
-define(EQC_PRESENT, false).
-endif.

%% ct.
all() -> [
	simple_test,
	reset_test,
	remove_test
].

groups() -> [].

suite() ->
	[{timetrap, {minutes, 2}}].

init_per_suite(Config) ->
     case ?EQC_PRESENT of
       false ->
	application:load(sasl),
	application:set_env(sasl, sasl_error_logger, false),
	application:set_env(sasl, errlog_type, error),
	error_logger:tty(false),
	ok = application:start(sasl),
	{ok, _} = application:ensure_all_started(fuse),
	Config;
      true ->
        {skip, running_eqc}
    end.

end_per_suite(_Config) ->
	application:stop(fuse),
	application:stop(sasl),
	ok.

init_per_group(_Group, Config) ->
	Config.

end_per_group(_Group, _Config) ->
	ok.

%% Tests.

-define(FUSE_SIMPLE, simple_fuse).
simple_test(_Config) ->
	ct:log("Install a new alarm handler to check alarms"),
	ok = gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {my_ah, [self()]}),

	ct:log("install a new event handler"),
	ok = fuse_event:add_handler(my_eh, [self()]),

	ct:log("Set up a new fuse, melt it and then verify it resets correctly"),
	ok = fuse:install(?FUSE_SIMPLE, {{standard, 2, 60}, {reset, 60*1000}}),
	ok = fuse:ask(?FUSE_SIMPLE, sync),
	ok = fuse:melt(?FUSE_SIMPLE),
	ok = fuse:ask(?FUSE_SIMPLE, sync),
	{ok, b} = fuse:run(?FUSE_SIMPLE, fun() -> {melt, b} end, sync),
	{ok, a} = fuse:run(?FUSE_SIMPLE, fun() -> {ok, a} end, sync),
	ok = fuse:melt(?FUSE_SIMPLE),
	blown = fuse:ask(?FUSE_SIMPLE, sync),
	receive
		{?FUSE_SIMPLE, blown} -> ok
	after 1000 ->
	    ct:fail(timeout_eh)
	end,
	receive
		{set_alarm, {?FUSE_SIMPLE, fuse_blown}} -> ok
	after 61 * 1000 ->
	    ct:fail(timeout_ah)
	end,
	ct:sleep(600),
	ok = fuse:ask(?FUSE_SIMPLE, sync),
	receive
		{?FUSE_SIMPLE, ok} ->
			ok
	after 1000 ->
		ct:fail(timeout_eh_2)
	end,
	ct:log("Removing the handler again"),
	ok = fuse_event:delete_handler(my_eh, []),
	ok.

-define(FUSE_RESET, reset_fuse).
reset_test(_Config) ->
	ct:log("Installing a fuse, then resetting it should clear out timers"),
	ok = fuse:install(?FUSE_RESET, {{standard, 2, 60}, {reset, 5000}}),
	ok = fuse:ask(?FUSE_RESET, sync),
	ok = fuse:melt(?FUSE_RESET),
	ok = fuse:melt(?FUSE_RESET),
	ok = fuse:melt(?FUSE_RESET),
	blown = fuse:ask(?FUSE_RESET, sync),
	ok = fuse:reset(?FUSE_RESET),
	ok = fuse:ask(?FUSE_RESET, sync),
	Stats = fuse_stats_ets:counters(?FUSE_RESET),
	3 = proplists:get_value(melt, Stats),
	2 = proplists:get_value(ok, Stats),
	1 = proplists:get_value(blown, Stats),
	ok.

-define(FUSE_REMOVE, remove_fuse).
remove_test(_Config) ->
	ct:log("Install a fuse, melt it, blow it, then remove it, then recreate it"),
	ok = fuse:install(?FUSE_REMOVE, {{standard, 2, 60}, {reset, 5000}}),
	ok = fuse:ask(?FUSE_REMOVE, sync),
	ok = fuse:melt(?FUSE_REMOVE),
	ok = fuse:melt(?FUSE_REMOVE),
	ok = fuse:melt(?FUSE_REMOVE),
	blown = fuse:ask(?FUSE_REMOVE, sync),
	Stats = fuse_stats_ets:counters(?FUSE_REMOVE),
	3 = proplists:get_value(melt, Stats),
	1 = proplists:get_value(ok, Stats),
	1 = proplists:get_value(blown, Stats),
	ok = fuse:remove(?FUSE_REMOVE),
	{error, not_found} = fuse:ask(?FUSE_REMOVE, sync),
	{error, not_found} = fuse:remove(?FUSE_REMOVE),
	ok = fuse:install(?FUSE_REMOVE, {{standard, 2, 60}, {reset, 5000}}),
	ok = fuse:ask(?FUSE_REMOVE, sync),
	Stats2 = fuse_stats_ets:counters(?FUSE_REMOVE),
	0 = proplists:get_value(melt, Stats2),
	1 = proplists:get_value(ok, Stats2),
	0 = proplists:get_value(blown, Stats2),
	ok.
