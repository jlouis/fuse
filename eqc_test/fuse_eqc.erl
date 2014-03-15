-module(fuse_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state, {
	installed = []
}).

fuses() -> [phineas, ferb, candace, perry, heinz].

valid_fuse(F) ->
	lists:member(F, fuses()).

g_atom() ->
	oneof([a,b,c,d,e,f]).

g_name() ->
	  oneof(fuses()).

g_strategy() ->
	fault(
		{g_atom(), int(), int()},
		{standard, 60, int()}
	).

g_refresh() ->
	{reset, 60000}.
	
g_options() ->
	{g_strategy(), g_refresh()}.

%%% install/2 puts a new fuse into the system
%%% ---------------------
install(Name, Opts) ->
	try fuse:install(Name, Opts) of
		ok -> ok
	catch
		error:badarg ->
			badarg
	end.

install_args(_S) ->
	[g_name(), g_options()].

install_next(#state{ installed = Is } = S, _V, [Name, Opts]) ->
	case valid_opts(Opts) of
	    false ->
	        S;
	    true ->
	        {{_, Count, _}, _} = Opts,
	        T = {Name, Count},
	        S#state { installed = lists:keystore(Name, 1, Is, T) }
	end.

install_post(_S, [_Name, Opts], R) ->
	case valid_opts(Opts) of
	    true -> eq(R, ok);
	    false -> eq(R, badarg)
	end.

%%% reset/1 resets a fuse back to its policy standard
reset(Name) ->
	fuse:reset(Name).
	
reset_pre(#state { installed = [] }) -> false;
reset_pre(#state { installed = [_|_] }) -> true.

reset_args(S) ->
	[oneof(installed_names(S))].

reset_post(S, [Name], Ret) ->
    case is_installed(Name, S) of
        true -> eq(Ret, ok);
        false -> eq(Ret, {error, no_such_fuse_name})
    end.

is_installed(N, #state { installed = Is }) -> lists:keymember(N, 1, Is).

%%% ask/1 asks about the state of a fuse that exists
%%% ---------------------
ask(Name) ->
	fuse:ask(Name).
	
ask_pre(#state { installed = [] }) -> false;
ask_pre(#state { installed = [_|_]}) -> true.

ask_args(_S) ->
	[g_name()].
	
ask_post(S, [Name], Ret) ->
	case is_installed(Name, S) of
	    true ->
	        eq(Ret, count_state(count(Name, S)));
	    false ->
	        eq(Ret, {error, no_such_fuse_name})
	end.

%%% run/1 runs a function (thunk) on the circuit breaker
%%% ---------------------
run(Name, _Result, Fun) ->
	fuse:run(Name, Fun).
	
run_pre(S) ->
	has_fuses_installed(S).

run_args(S) ->
    ?LET({N, Result}, {oneof(installed_names(S)), oneof([ok, melt])},
      [N, Result, function0({Result, int()})] ).

%%% melt/1 melts the fuse a little bit
%%% ---------------------
melt(Name) ->
	fuse:melt(Name).
	
melt_pre(#state { installed = [] }) -> false;
melt_pre(#state { installed = [_|_]}) -> true.

melt_args(S) ->
 	[oneof(installed_names(S))].

melt_next(#state { installed = Is } = S, _V, [Name]) ->
	{Name, Count} = lists:keyfind(Name, 1, Is),
	S#state { installed = lists:keystore(Name, 1, Is, {Name, case Count of 0 -> 0; N -> N-1 end}) }.

melt_post(_S, _, Ret) ->
	eq(Ret, ok).

%%% Property
prop_model() ->
    fault_rate(1, 10,
	?FORALL(Cmds, commands(?MODULE, #state{}),
	  begin
	  	application:stop(fuse),
	  	{ok, _} = application:ensure_all_started(fuse),
	  	{H, S, R} = run_commands(?MODULE, Cmds),
	  	?WHENFAIL(
	  		io:format("History: ~p\nState: ~p\nResult: ~p\n", [H, S, R]),
	  		aggregate(command_names(Cmds), R == ok))
	  end)).

%%% INTERNALS
%%% ---------------------

%% Pick amongst the installed names
installed_names(#state { installed = Is }) ->
	[N || {N, _} <- Is].

%% Determine if opts is valid
valid_opts({{standard, K, R}, {reset, T}})
    when K >= 0, R >= 0, T >= 0 ->
	true;
valid_opts(_) ->
	false.
	
count(Name, #state { installed = Inst }) ->
	{Name, Count} = lists:keyfind(Name, 1, Inst),
	Count.

count_state(0) -> blown;
count_state(_N) -> ok.

has_fuses_installed(#state { installed = [] }) -> false;
has_fuses_installed(#state { installed = [_|_]}) -> true.
