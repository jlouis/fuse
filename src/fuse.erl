%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-export([
	ask/1, ask/2,
	install/2,
	melt/1,
	reset/1,
	run/2, run/3
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
run(Name, Func) -> run(Name, Func, []).

%% @doc run/3 runs a thunk under a given fuse
%% The difference from `run/2' is that this variant allows you to specify the options
%% to `ask/2' which gives the ability to ask synchronously.
%% @end
-spec run(Name, fun (() -> {ok, Result} | {melt, Result}), [] | [sync] ) -> {ok, Result} | blown | {error, not_found}
  when
    Name :: atom(),
    Result :: any().
run(Name, Func, Opts) ->
    fuse_srv:run(Name, Func, Opts).

%% @equiv ask(N, [])
-spec ask(Name) -> ok | blown | {error, not_found}
  when Name :: atom().
ask(Name) ->
    ask(Name, []).

%% @doc ask/2 queries the state of a fuse
%% Given `ask(N, [])' we ask the fuse state for the name `N'. The invocation `ask(N, [sync])'
%% does the same but uses synchronized calling and factors through a fuse server to get a definite answer.
%% This is useful if one MUST know the current state of the fuse for good. Returns the fuse state, either `ok' or `blown'.
%% If there is no such fuse, returns `{error, not_found}'
%% @end
-spec ask(Name, [] | [sync]) -> ok | blown | {error, not_found}
  when Name :: atom().
ask(N, []) -> fuse_srv:ask(N, []);
ask(N, [sync]) -> fuse_srv:ask(N, [sync]);
ask(_N, _Otherwise) -> error(badarg).

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

