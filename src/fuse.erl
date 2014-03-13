%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-export([install/2, ask/1]).

-type fuse_policy() :: {counter, pos_integer()}.

-type fuse_options() ::
	[ {policy, fuse_policy()} ].

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

options_ok([]) ->
    ok;
options_ok([{policy, Pol} | Opts]) ->
   policy_ok(Pol),
   options_ok(Opts);
options_ok(_) ->
	error(badarg).
   
policy_ok({counter, N}) when N > 0 -> ok;
policy_ok(_) ->
	error(badarg).
