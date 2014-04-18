-module(t).

-export([z/0, seq/0, par/0]).
-export([recheck/1]).

min(N) -> N * 60.

z() ->
    make:all([load]),
    [] = eqc:module({testing_time, min(5)}, mon_eqc),
    [] = eqc:module({testing_time, min(1)}, model_time),
    [] = eqc:module({testing_time, min(1)}, fuse_time),
    [] = eqc:module({testing_time, min(5)}, fuse_time_eqc).
    
seq() ->
	fuse_eqc:r(seq, {30, min}).
	
recheck(seq) ->
	eqc:recheck(eqc_statem:show_states(fuse_eqc:prop_model_seq()));
recheck(par) ->
	eqc:recheck(eqc_statem:show_states(fuse_eqc:prop_model_par()));
recheck(pulse) ->
	eqc:recheck(eqc_statem:show_states(fuse_eqc:x_prop_model_pulse())).
	

par() ->
	fuse_eqc:r(par, {30, min}).

	