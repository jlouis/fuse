%%% @author Thomas Arts 
%%% @copyright (C) 2014, Quviq AB
%%% @doc Showing that os:timestamp does not behave as expected
%%%
%%% @end
%%% Created : 26 Mar 2014 by Thomas Arts <thomas.arts@quviq.com>

-module(fuse_time_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

initial_state() ->
  {0,0,0}.

timestamp_command(_S) -> 
  {call, fuse_time, timestamp, []}.

timestamp_next(_S, NewTime, []) ->
  NewTime.

timestamp_post(S, [], NewTime) ->
  S < NewTime.

elapse_time_command(_S) ->
  {call, fuse_time, elapse_time, [?LET(N,nat(),N+1)]}.

elapse_time_post(S, [_], NewTime) ->
  less(S,NewTime).


less(X,Y) when X<Y ->
  true;
less(X,Y) ->
  {X,'>=',Y}.
  

prop_os() ->
  ?FORALL(Cmds, commands(?MODULE),
	  begin
	    {H, S, Res} = run_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).

prop_os_par() ->
  ?FORALL(Cmds, parallel_commands(?MODULE),
	  begin
	    fuse_time:start(),
	    {H, S, Res} = run_parallel_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).
