-module(fuse_time).
-compile(nowarn_deprecated_function).

-export([monotonic_time/0, monotonic_time/1, convert_time_unit/3]).
-export([unique_integer/0, unique_integer/1]).
-export([cancel_timer/1, send_after/3]).

cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

send_after(Timeout, Target, Msg) ->
    erlang:send_after(Timeout, Target, Msg).

monotonic_time() ->
    try
	erlang:monotonic_time()
    catch
	error:undef ->
	    %% Use Erlang system time as monotonic time
	    erlang_system_time_fallback()
    end.

monotonic_time(Unit) ->
    try
	erlang:monotonic_time(Unit)
    catch
	error:badarg ->
	    erlang:error(badarg, [Unit]);
	error:undef ->
	    %% Use Erlang system time as monotonic time
	    STime = erlang_system_time_fallback(),
	    try
		convert_time_unit_fallback(STime, native, Unit)
	    catch
		error:bad_time_unit -> erlang:error(badarg, [Unit])
	    end
    end.

convert_time_unit(Time, FromUnit, ToUnit) ->
    try
	erlang:convert_time_unit(Time, FromUnit, ToUnit)
    catch
	error:undef ->
	    try
		convert_time_unit_fallback(Time, FromUnit, ToUnit)
	    catch
		_:_ ->
		    erlang:error(badarg, [Time, FromUnit, ToUnit])
	    end;
	error:Error ->
	    erlang:error(Error, [Time, FromUnit, ToUnit])
    end.

unique_integer() ->
    try
	erlang:unique_integer()
    catch
	error:undef ->
	    {MS, S, US} = erlang:now(),
	    (MS*1000000+S)*1000000+US
    end.

unique_integer(Modifiers) ->
    try
	erlang:unique_integer(Modifiers)
    catch
	error:badarg ->
	    erlang:error(badarg, [Modifiers]);
	error:undef ->
	    case is_valid_modifier_list(Modifiers) of
		true ->
		    %% now() converted to an integer
		    %% fullfill the requirements of
		    %% all modifiers: unique, positive,
		    %% and monotonic...
		    {MS, S, US} = erlang:now(),
		    (MS*1000000+S)*1000000+US;
		false ->
		    erlang:error(badarg, [Modifiers])
	    end
    end.

%%
%% Internal functions
%%

integer_time_unit(native) -> 1000*1000;
integer_time_unit(nano_seconds) -> 1000*1000*1000;
integer_time_unit(micro_seconds) -> 1000*1000;
integer_time_unit(milli_seconds) -> 1000;
integer_time_unit(seconds) -> 1;
integer_time_unit(I) when is_integer(I), I > 0 -> I;
integer_time_unit(BadRes) -> erlang:error(bad_time_unit, [BadRes]).

erlang_system_time_fallback() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

convert_time_unit_fallback(Time, FromUnit, ToUnit) ->
    FU = integer_time_unit(FromUnit),
    TU = integer_time_unit(ToUnit),
    case Time < 0 of
	true -> TU*Time - (FU - 1);
	false -> TU*Time
    end div FU.

is_valid_modifier_list([positive|Ms]) ->
    is_valid_modifier_list(Ms);
is_valid_modifier_list([monotonic|Ms]) ->
    is_valid_modifier_list(Ms);
is_valid_modifier_list([]) ->
    true;
is_valid_modifier_list(_) ->
    false.