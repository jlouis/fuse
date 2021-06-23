%%% @doc Wrap erlang timing, so we have a mock-factoring-point
%%% @private
-module(fuse_time).

-export([monotonic_time/0, monotonic_time/1, convert_time_unit/3]).
-export([unique_integer/0, unique_integer/1]).
-export([cancel_timer/1, send_after/3]).

cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

send_after(Timeout, Target, Msg) ->
    erlang:send_after(Timeout, Target, Msg).

monotonic_time() ->
	erlang:monotonic_time().

monotonic_time(Unit) ->
	erlang:monotonic_time(Unit).

convert_time_unit(Time, FromUnit, ToUnit) ->
	erlang:convert_time_unit(Time, FromUnit, ToUnit).

unique_integer() ->
	erlang:unique_integer().

unique_integer(Modifiers) ->
	erlang:unique_integer(Modifiers).
