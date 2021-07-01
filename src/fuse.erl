%%% @doc Fuse implements a circuit breaker pattern for Erlang.
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
    {standard, non_neg_integer(), pos_integer()}
    | {fault_injection, fault_rate(), pos_integer(), pos_integer()}.
-type fuse_refresh() :: {reset, pos_integer()}.
-type fuse_options() ::
    {fuse_strategy(), fuse_refresh()}.

-export_type([fuse_context/0, fuse_options/0]).

%% @doc Adds a new fuse to the running system.
%% <p>A call `install(N, Os)' will add a new fuse under the name `N' with options given by `Os'. Note that the options must match
%% the correct type, or a `badarg' error will be thrown.</p>
%% @end
%% install/2
-spec install(Name, Options) -> ok | reset | {error, Reason}
    when
      Name :: term(),
      Options :: fuse_options(),
      Reason :: any().
install(Name, Options) ->
    options_ok(Options),
    fuse_server:install(Name, Options).

%% @doc Administratively disables a circuit.
%% <p>This function is intended to be used administratively, when you want to break the fuse
%% before you do administration on the service which the fuse protects. This can be used to
%% e.g., carry out database maintenance. After maintenance, the administrator can reenable
%% the circuit again.</p>
%% <p>Disabling a circuit dominates every other operation, except `remove/1'.</p>
%% @end.
%% circuit_disable/1
-spec circuit_disable(Name) -> ok
   when Name :: term().
circuit_disable(Name) ->
    fuse_server:circuit(Name, disable).

%% @doc Administratively (re-)enables a fuse.
%% <p>This call is used to reenable a disabled circuit again. Always returns ok and is idempotent.</p>
%% <p>Use this command at the point in time where you are done with administrative fixes and want
%% to resume normal operation of the fuse.</p>
%% @end
%% circuit_enable/1
-spec circuit_enable(Name) -> ok
  when Name :: term().
circuit_enable(Name) ->
    fuse_server:circuit(Name, enable).

%% @doc Runs a thunk under a given fuse.
%% <p>Calling `run(Name, Func)' will run `Func' protected by the fuse `Name'.</p>
%% @end
%% run/3
-spec run(Name, fun (() -> {ok, Result} | {melt, Result}), fuse_context() ) -> {ok, Result} | blown | {error, not_found}
    when
      Name :: term(),
      Result :: any().
run(Name, Func, Context) -> fuse_server:run(Name, Func, Context).


%% @doc Queries the state of a fuse.
%% <p>Given `ask(N)' we ask the fuse state for the name `N'. Returns the fuse state, either `ok' or `blown'.
%% If there is no such fuse, returns `{error, not_found}'.</p>
%% @end
%% ask/2
-spec ask(Name, fuse_context()) -> ok | blown | {error, not_found}
  when Name :: term().
ask(Name, Context) -> fuse_server:ask(Name, Context).

%% @doc Resets a fuse.
%% <p>Given `reset(N)' this resets the fuse under the name `N'. The fuse will be unbroken with no melts.</p>
%% @end
%% reset/1
-spec reset(Name) -> ok | {error, not_found}
  when Name :: term().
reset(Name) ->
    fuse_server:reset(Name).

%% @doc Melts a fuse a little bit.
%% <p>A call to `melt(N)' will melt fuse `N'. This call always returns `ok' and it is currently implemented synchronously.</p>
%% @end
%% melt/1
-spec melt(Name) -> ok
  when Name :: term().
melt(Name) ->
    fuse_server:melt(Name).

%% @doc Removes a fuse.
%% <p>Given `remove(N)' this removes the fuse under the name `N'. This fuse will no longer exist.</p>
%% @end
%% remove/1
-spec remove(Name) -> ok
  when Name :: term().
remove(Name) ->
    fuse_server:remove(Name).

%% Internal functions
%% -----------------------

options_ok({{standard, MaxR, MaxT}, {reset, Time}})
    when
      is_integer(MaxR), MaxR >= 0,
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
