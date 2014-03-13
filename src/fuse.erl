%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-export([install/2, ask/1]).

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
    
%% @doc ask/1 queries the state of a fuse
%% Given `ask(N)' we ask the fuse state of the name `N'
%% @end
-spec ask(Name) -> ok | blown | {error, no_such_fuse_name}
  when Name :: atom().
ask(Name) ->
    fuse_srv:ask(Name).

options_ok({{standard, MaxR, MaxT}, {reset, Time}})
    when
      is_integer(MaxR), MaxR >= 0,
      is_integer(MaxT), MaxT >= 0,
      is_integer(Time), Time >= 0 -> ok;
options_ok(_) ->
    error(badarg).

