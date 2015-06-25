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
	fuse_time_par/1
]).

-ifdef(EQC).
-define(PRESENT, true).
-else.
-define(PRESENT, false).
-endif.

qc(Prop) ->
        case eqc:counterexample(Prop) of
          true -> 
            true;
          false ->
            exit(gave_up);
          _quickcheck_CounterExample ->
            exit(_quickcheck_CounterExample)
        end.

%% ct.
all() -> [
    fuse_time_seq,
    fuse_time_par
].
	
groups() -> [].
	
suite() ->
	[{timetrap, {minutes, 2}}].

init_per_suite(Config) ->
	case ?PRESENT of
	    false -> {skip, no_quickcheck};
	    true -> Config
	end.
	
end_per_suite(_Config) ->	
	ok.
	
init_per_group(_Group, Config) ->
	Config.
	
end_per_group(_Group, _Config) ->
	ok.

%% Tests.
fuse_time_seq(_Config) ->
	qc(fuse_time_eqc:prop_seq()).

fuse_time_par(_Config) ->
	qc(fuse_time_eqc:prop_par()).
