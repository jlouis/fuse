%%%-------------------------------------------------------------------
%%% @author Thomas Arts <>
%%% @copyright (C) 2014, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2014 by Thomas Arts <>
%%%-------------------------------------------------------------------
-module(fuse_time).

-include_lib("eqc/include/eqc.hrl").

-export([start/0, init/1]).
-export([timestamp/0,elapse_time/1, forward_to/1]).
-export([inc/2]).

-export([prop_inc/0]).

-define(UNIT,1000).

timestamp() ->
  ?MODULE ! {timestamp, self()},
  receive
     {timestamp, Time} -> Time
  after 1000 ->
      exit(timeout)
  end.

elapse_time(N) ->
  ?MODULE ! {elapse, self(), N},
  receive
     {timestamp, Time} -> Time
  after 1000 ->
      exit(timeout)
  end.

forward_to(TP) ->
  ?MODULE ! {forward_to, self(), TP},
  receive
    ok -> ok;
    {error, Reason} -> {error, Reason}
  after 200 ->
  	exit(timeout)
  end.

start() ->
  case whereis(?MODULE) of
    Pid when is_pid(Pid) ->
      Pid ! {reset, self()},
      receive
          ok -> ok
      end;
    undefined ->
      spawn_link(fuse_time, init, [self()]),
      receive 
	ok -> ?MODULE
      end
  end.

init(From) ->
  register(?MODULE, self()),
  From ! ok,
  loop({0,0,0}).

loop(Time) ->
  receive
    {timestamp, From} ->
      From ! {timestamp, Time},
      loop(inc(Time,0));
    {forward_to, From, NewTime} when NewTime >= Time ->
      From ! ok,
      loop(NewTime);
    {forward_to, From, NewTime} when NewTime < Time ->
      From ! {error, clock_skew_backwards},
      loop(Time);
    {elapse, From, N} ->
      NewTime = inc(Time,N),
      From ! {timestamp, NewTime},
      loop(NewTime);
    {reset, From} ->
      From ! ok,
      loop({0,0,0});
    stop ->
      Time
  end.

inc({Mega,One,Mili},N) ->
  inc({Mega,One,Mili},N,?UNIT).
       
inc({Mega,One,Mili}, N, Unit) ->
  NewTime = Mega*Unit*Unit + One*Unit + Mili + N,
  NMega = NewTime div (Unit*Unit),
  NOne  = (NewTime rem (Unit*Unit)) div Unit,
  NMili = NewTime rem Unit,
  {NMega, NOne, NMili}.

prop_inc() ->
  ?FORALL(Base, choose(2,10),
  ?FORALL({Time,N}, {{choose(0,Base-1),choose(0,Base-1),choose(0,Base-1)},choose(0,Base*Base+1)},
	  begin
	    {Me,One,Mi} = inc(Time,N,Base),
	    ?WHENFAIL(io:format("Computed: ~p\n",[{Me,One,Mi}]),
		      Me<Base*Base andalso One<Base andalso Mi<Base)
	  end)).
  
  
  
  
