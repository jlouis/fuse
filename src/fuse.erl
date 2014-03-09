%%% @doc Fuse implements a circuit breaker pattern for Erlang
%%% @end
-module(fuse).

-export([install/2]).

-type fuse_policy() :: {counter, pos_integer()}.

-type fuse_options() ::
	[ {policy, fuse_policy()} ].

-spec install(Name, Options) -> ok | reset | {error, Reason}
	when
	  Name :: atom(),
	  Options :: fuse_options(),
	  Reason :: any().
install(Name, Options) ->
    options_ok(Options),
    fuse_srv:install(Name, Options).
    
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
