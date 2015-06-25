-module(fuse_eqc_SUITE).
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
	fuse_time_seq/1,
	fuse_time_par/1,
	fuse_monitor_component/1,
	fuse_server_seq/1, fuse_server_par/1
]).

-define(TESTING_TIME, 15).

-ifdef(EQC_TESTING).
-define(PRESENT, true).
-else.
-define(PRESENT, false).
-endif.

qc(Prop) ->
        case eqc:counterexample(eqc:testing_time(?TESTING_TIME, Prop)) of
          true -> 
            true;
          false ->
            exit(gave_up);
          _quickcheck_CounterExample ->
            exit(_quickcheck_CounterExample)
        end.

%% ct.
all() -> [
	{group, time},
	{group, monitor},
	{group, server}
].
	
groups() -> [
	{monitor, [], [fuse_monitor_component]},
	{time, [], [fuse_time_seq, fuse_time_par]},
	{server, [], [fuse_server_seq, fuse_server_par]}
].
	
suite() ->
	[{timetrap, {minutes, 2}}].

init_per_suite(Config) ->
	case ?PRESENT of
	    false -> {skip, no_quickcheck};
	    true ->
	        error_logger:tty(false),
	        application:load(sasl),
	        application:set_env(sasl, sasl_error_logger, false),
	        application:set_env(sasl, errlog_type, error),
	        {ok, _Apps} = application:ensure_all_started(sasl),
	        Config
	end.
	
end_per_suite(_Config) ->	
	ok.
	
init_per_group(server, Config) ->
	Config;
init_per_group(_Group, Config) ->
	Config.
	
end_per_group(server, _Config) ->
	application:stop(fuse),
	ok;
end_per_group(_Group, _Config) ->
	ok.

%% Tests.
fuse_time_seq(_Config) ->
	qc(fuse_time_eqc:prop_seq()).

fuse_time_par(_Config) ->
	qc(fuse_time_eqc:prop_par()).

fuse_monitor_component(_Config) ->
	qc(mon_eqc:prop_component_correct()).

fuse_server_seq(_Config) ->
	qc(fuse_eqc:prop_model_seq()).

fuse_server_par(_Config) ->
	qc(fuse_eqc:prop_model_par()).
