%%%-------------------------------------------------------------------
%%% @author Thomas Arts <>
%%% @copyright (C) 2014, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2014 by Thomas Arts <>
%%%-------------------------------------------------------------------
-module(fuse_time_mock).

-include_lib("eqc/include/eqc.hrl").
-include_lib("pulse_otp/include/pulse_otp.hrl").

-export([start/1, init/2]).
-export([monotonic_time/0,elapse_time/1]).
-export([convert_time_unit/3]).
-export([send_after/3, cancel_timer/1]).

send_after(_When, _Target, _Msg) -> make_ref().
cancel_timer(_Ref) -> 0.
convert_time_unit(X, _, _) -> X.

monotonic_time() ->
  ?MODULE ! {monotonic_time, self()},
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

start(Time) ->
  case whereis(?MODULE) of
    Pid when is_pid(Pid) ->
      Pid ! {reset, self(), Time},
      receive
          ok -> ok
      end;
    undefined ->
      spawn_link(?MODULE, init, [self(), Time]),
      receive
	ok -> ?MODULE
      end
  end.

init(From, Time) ->
  register(?MODULE, self()),
  From ! ok,
  loop(Time).

loop(Time) ->
  receive
    {monotonic_time, From} ->
      From ! {timestamp, Time},
      loop(Time);
    {elapse, From, N} ->
      NewTime = Time + N,
      From ! {timestamp, NewTime},
      loop(NewTime);
    {reset, From, RTime} ->
      From ! ok,
      loop(RTime);
    stop ->
      Time
  end.
