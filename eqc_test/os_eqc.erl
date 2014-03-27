%%% @author Thomas Arts 
%%% @copyright (C) 2014, Quviq AB
%%% @doc Showing that os:timestamp does not behave as expected
%%%
%%% @end
%%% Created : 26 Mar 2014 by Thomas Arts <thomas.arts@quviq.com>

-module(os_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

initial_state() ->
  {0,0,0}.

timestamp_command(_S) -> 
  {call, os, timestamp, []}.

timestamp_next(_S, NewTime, []) ->
  NewTime.

timestamp_post(S, [], NewTime) ->
  S < NewTime.

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
	    {H, S, Res} = run_parallel_commands(?MODULE,Cmds),
	    pretty_commands(?MODULE, Cmds, {H, S, Res},
			    Res == ok)
	  end).
