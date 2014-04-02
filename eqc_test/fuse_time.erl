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
-export([timestamp/0,elapse_time/1]).
-export([send_after/3, cancel_timer/1]).
-export([inc/2]).

-export([prop_inc/0]).

-define(UNIT,1000*1000).

send_after(_When, _Target, _Msg) -> make_ref().
cancel_timer(_Ref) -> 0.

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
       
inc({Mega,One,Micro}, N, Unit) ->
  NewTime = Mega*Unit*Unit + One*Unit + Micro + N,
  NMega = NewTime div (Unit*Unit),
  NOne  = (NewTime rem (Unit*Unit)) div Unit,
  NMicro = NewTime rem Unit,
  {NMega, NOne, NMicro}.

prop_inc() ->
  ?FORALL(Base, choose(2,10),
  ?FORALL({Time,N}, {{choose(0,Base-1),choose(0,Base-1),choose(0,Base-1)},choose(0,Base*Base+1)},
	  begin
	    {Me,One,Mi} = inc(Time,N,Base),
	    ?WHENFAIL(io:format("Computed: ~p\n",[{Me,One,Mi}]),
		      Me<Base*Base andalso One<Base andalso Mi<Base)
	  end)).
  
  
  
  
