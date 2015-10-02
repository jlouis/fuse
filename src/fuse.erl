%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-export([
	ask/2,
	install/2,
	melt/1,
	remove/1,
	reset/1,
	run/3
]).

-type fuse_context() :: sync | async_dirty.
-type fuse_strategy() :: {standard, pos_integer(), pos_integer()}.
-type fuse_refresh() :: {reset, pos_integer()}.
-type fuse_options() ::
	{fuse_strategy(), fuse_refresh()}.

-export_type([fuse_context/0, fuse_options/0]).

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
    fuse_server:install(Name, Options).

%% @doc run/2 runs a thunk under a given fuse
%% Calling `run(Name, Func)' will run `Func' protected by the fuse `Name'
%% @end
-spec run(Name, fun (() -> {ok, Result} | {melt, Result}), fuse_context() ) -> {ok, Result} | blown | {error, not_found}
    when
      Name :: atom(),
      Result :: any().
run(Name, Func, Context) -> fuse_server:run(Name, Func, Context).


%% @doc ask/1 queries the state of a fuse
%% Given `ask(N)' we ask the fuse state for the name `N'. Returns the fuse state, either `ok' or `blown'.
%% If there is no such fuse, returns `{error, not_found}'
%% @end
-spec ask(Name, fuse_context()) -> ok | blown | {error, not_found}
  when Name :: atom().
ask(Name, Context) -> fuse_server:ask(Name, Context).

%% @doc reset/1 resets a fuse
%% Given `reset(N)' this resets the fuse under the name `N'. The fuse will be unbroken with no melts.
%% @end
-spec reset(Name) -> ok | {error, not_found}
  when Name :: atom().
reset(Name) ->
    fuse_server:reset(Name).

%% @doc melt/1 melts a fuse a little bit
%% A call to `melt(N)' will melt fuse `N'. This call always returns `ok' and it is currently implemented synchronously.
%% @end
-spec melt(Name) -> ok
  when Name :: atom().
melt(Name) ->
	fuse_server:melt(Name).

%% @doc remove/1 removs a fuse
%% Given `remove(N)' this removes the fuse under the name `N'. This fuse will no longer exist.
%% @end
-spec remove(Name) -> ok
  when Name :: atom().
remove(Name) ->
    fuse_server:remove(Name).

%% Internal functions
%% -----------------------

options_ok({{standard, MaxR, MaxT}, {reset, Time}})
    when
      is_integer(MaxR), MaxR > 0,
      is_integer(MaxT), MaxT >= 0,
      is_integer(Time), Time >= 0 -> ok;
options_ok(_) ->
    error(badarg).

