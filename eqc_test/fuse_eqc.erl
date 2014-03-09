-module(fuse_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state, {}).

g_name() ->
	oneof([fuse_a, fuse_b, fuse_c, fuse_d]).

g_options() ->
	return([{policy, {counter, 5}}]).

install(Name, Opts) ->
	fuse:install(Name, Opts).

install_args(_S) ->
	[g_name(), g_options()].

prop_model() ->
	?FORALL(Cmds, commands(?MODULE, #state{}),
	  begin
	  	application:stop(fuse),
	  	application:start(fuse),
	  	{H, S, R} = run_commands(?MODULE, Cmds),
	  	?WHENFAIL(
	  		io:format("History: ~p\nState: ~p\nResult: ~p\n", [H, S, R]),
	  		aggregate(command_names(Cmds), R == ok))
	  end).
