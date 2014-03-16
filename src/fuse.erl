%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-export([
	ask/1,
	install/2,
	melt/1, melt/2,
	reset/1,
	run/2
]).

-type fuse_strategy() :: {standard, pos_integer(), pos_integer()}.
-type fuse_refresh() :: {reset, pos_integer()}.
-type fuse_options() ::
	{fuse_strategy(), fuse_refresh()}.

%% @doc install/2 adds a new fuse to the running system.
%% A call `install(N, Os)' will add a new fuse under the name `N' with options given by `Os'. Note that the options must match
%% the correct type, or a `badarg' error will be thrown.
%% @end
-spec install(Name, Options) -> ok | reset | {error, Reason}
	when
	  Name :: atom(),
	  Options :: fuse_options(),
	  Reason :: any().
install(Name, Options) ->
    options_ok(Options),
    fuse_srv:install(Name, Options).
    
%% @doc run/2 runs a thunk under a given fuse
%% Calling `run(Name, Func)' will run `Func' protected by the fuse `Name'
%% @end
-spec run(Name, fun (() -> {ok, Result} | {melt, Result}) ) -> {ok, Result} | blown | {error, no_such_fuse_name}
    when
      Name :: atom(),
      Result :: any().
run(Name, Func) ->
    case ask(Name) of
        blown -> blown;
        ok ->
          case Func() of
              {ok, Result} -> {ok, Result};
              {melt, Result} ->
                  melt(Name),
                  {ok, Result}
          end;
        {error, Reason} ->
          {error, Reason}
    end.

%% @doc ask/1 queries the state of a fuse
%% Given `ask(N)' we ask the fuse state of the name `N'
%% @end
-spec ask(Name) -> ok | blown | {error, no_such_fuse_name}
  when Name :: atom().
ask(Name) ->
    fuse_srv:ask(Name).

%% @doc reset/1 resets the internal counter of a given fuse
%% Given `reset(N)' we ask the system to reset the fuse `N'
%% @end
-spec reset(Name) -> ok | {error, no_such_fuse_name}
  when Name :: atom().
reset(Name) ->
    fuse_srv:reset(Name).

%% @doc melt/1 melts a fuse a little bit
%% A call to `melt(N)' will melt fuse `N'. This call always returns `ok' and it is currently implemented synchronously.
%% @end
-spec melt(Name) -> ok
  when Name :: atom().
melt(Name) ->
	melt(Name, os:timestamp()).
	
%% melt/2 allows to call with a specific timestamp
%% @private
melt(Name, Ts) ->
	fuse_srv:melt(Name, Ts).

%% Internal functions
%% -----------------------

options_ok({{standard, MaxR, MaxT}, {reset, Time}})
    when
      is_integer(MaxR), MaxR >= 0,
      is_integer(MaxT), MaxT >= 0,
      is_integer(Time), Time >= 0 -> ok;
options_ok(_) ->
    error(badarg).

