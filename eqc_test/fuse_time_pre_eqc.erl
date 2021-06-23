%%% @author Thomas Arts 
%%% @copyright (C) 2014, Quviq AB
%%% @doc Showing that the fuse_time_mock module behaves as expected
%%%
%%% @end
%%% Created : 26 Mar 2014 by Thomas Arts <thomas.arts@quviq.com>

-module(fuse_time_pre_eqc).
-compile(export_all).

-ifdef(EQC_TESTING).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

initial_state() ->
  -10000.

monotonic_time() ->
    fuse_time_mock:monotonic_time().
    
monotonic_time_args(_S) -> [].

monotonic_time_next(_S, NewTime, []) ->
  NewTime.

monotonic_time_post(S, [], NewTime) ->
  S =< NewTime.

elapse_time(Inc) ->
    fuse_time_mock:elapse_time(Inc).
    
elapse_time_args(_S) ->
    [?LET(N, choose(0, 1000*1000*1000), N+1)].

elapse_time_post(S, [_], NewTime) ->
  less(S,NewTime).

less(X,Y) when X=<Y -> true;
less(X,Y) -> {X,'>=',Y}.
  
prop_seq() ->
  ?FORALL(Cmds, commands(?MODULE),
	  begin
	    fuse_time_mock:start(0),
	    {H, S, Res} = run_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).

prop_par() ->
  ?FORALL(Cmds, parallel_commands(?MODULE),
	  begin
	    fuse_time_mock:start(-10000),
	    {H, S, Res} = run_parallel_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).





-endif.
