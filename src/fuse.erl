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

-export([
	circuit_enable/1,
	circuit_disable/1
]).

-type fuse_context() :: sync | async_dirty.
-type fault_rate() :: float().
-type fuse_strategy() ::
	{standard, pos_integer(), pos_integer()}
	| {fault_injection, fault_rate(), pos_integer(), pos_integer()}.
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

%% @doc circuit_disable/1 administratively disable a circuit
%% <p>This function is intended to be used administratively, when you want to break the fuse
%% before you do administration on the service which the fuse protects. This can be used to
%% e.g., carry out database maintenance. After maintenance, the administrator can reenable
%% the circuit again.</p>
%% <p>Disabling a circuit dominates every other operation, except `remove/1`.</p>
%% @end.
-spec circuit_disable(Name) -> ok
   when Name :: atom().
circuit_disable(Name) ->
    fuse_server:circuit(Name, disable).

%% @doc circuit_enable/1 administratively (re-)enables a fuse
%% <p>This call is used to reenable a disabled circuit again. Always returns ok and is idempotent.</p>
%% <p>Use this command at the point in time where you are done with administrative fixes and want
%% to resume normal operation of the fuse.</p>
%% @end
-spec circuit_enable(Name) -> ok
  when Name :: atom().
circuit_enable(Name) ->
    fuse_server:circuit(Name, enable).

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
options_ok({{fault_injection, Rate, MaxR, MaxT}, {reset, Time}})
    when
      is_integer(MaxR), MaxR > 0,
      is_integer(MaxT), MaxT >= 0,
      is_integer(Time), Time >= 0,
      is_float(Rate), 0.0 < Rate, Rate =< 1.0 -> ok;
options_ok(_) ->
    error(badarg).

