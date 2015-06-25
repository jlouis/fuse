%%% @author Thomas Arts 
%%% @copyright (C) 2014, Quviq AB
%%% @doc Showing that the fuse_time module behaves as expected
%%%
%%% @end
%%% Created : 26 Mar 2014 by Thomas Arts <thomas.arts@quviq.com>

-module(fuse_time_eqc).
-compile(export_all).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

initial_state() ->
  {0,0,0}.

timestamp_command(_S) -> 
  {call, fuse_time_mock, timestamp, []}.

timestamp_next(_S, NewTime, []) ->
  NewTime.

timestamp_post(S, [], NewTime) ->
  S =< NewTime.

time_inc() ->
    ?LET(N, choose(0, 1000*1000),
        N+1).

elapse_time_command(_S) ->
  {call, fuse_time_mock, elapse_time, [time_inc()]}.

elapse_time_post(S, [_], NewTime) ->
  less(S,NewTime).

less(X,Y) when X=<Y -> true;
less(X,Y) -> {X,'>=',Y}.
  

prop_seq() ->
  ?FORALL(Cmds, commands(?MODULE),
	  begin
	    fuse_time_mock:start({0,0,0}),
	    {H, S, Res} = run_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).

prop_par() ->
  ?FORALL(Cmds, parallel_commands(?MODULE),
	  begin
	    fuse_time_mock:start({0,0,0}),
	    {H, S, Res} = run_parallel_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).





-endif.
