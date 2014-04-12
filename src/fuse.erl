%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-export([
	ask/1,
	install/2,
	melt/1,
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
-spec run(Name, fun (() -> {ok, Result} | {melt, Result}) ) -> {ok, Result} | blown | {error, not_found}
    when
      Name :: atom(),
      Result :: any().
run(Name, Func) -> fuse_srv:run(Name, Func).


%% @doc ask/1 queries the state of a fuse
%% Given `ask(N)' we ask the fuse state for the name `N'. Returns the fuse state, either `ok' or `blown'.
%% If there is no such fuse, returns `{error, not_found}'
%% @end
-spec ask(Name) -> ok | blown | {error, not_found}
  when Name :: atom().
ask(Name) -> fuse_srv:ask(Name).

%% @doc reset/1 resets a fuse
%% Given `reset(N)' this resets the fuse under the name `N'. The fuse will be unbroken with no melts.
%% @end
-spec reset(Name) -> ok | {error, not_found}
  when Name :: atom().
reset(Name) ->
    fuse_srv:reset(Name).

%% @doc melt/1 melts a fuse a little bit
%% A call to `melt(N)' will melt fuse `N'. This call always returns `ok' and it is currently implemented synchronously.
%% @end
-spec melt(Name) -> ok
  when Name :: atom().
melt(Name) ->
	fuse_srv:melt(Name).

%% Internal functions
%% -----------------------

options_ok({{standard, MaxR, MaxT}, {reset, Time}})
    when
      is_integer(MaxR), MaxR > 0,
      is_integer(MaxT), MaxT >= 0,
      is_integer(Time), Time >= 0 -> ok;
options_ok(_) ->
    error(badarg).

