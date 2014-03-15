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
		{standard, int(), int()}
	).

g_refresh() ->
	{reset, 60000}.
	
g_options() ->
	{g_strategy(), g_refresh()}.

%%% install/2 puts a new fuse into the system
%%% ---------------------
install(Name, Opts) ->
	catch fuse:install(Name, Opts).

install_args(_S) ->
	[g_name(), g_options()].

install_next(#state{ installed = Is } = S, _V, [Name, Opts]) ->
	{{standard, Count, _}, _} = Opts,
	T = {Name, Count},
	S#state { installed = lists:keystore(Name, 1, Is, T) }.

install_post(_S, _, R) ->
	eq(R, ok).

%%% reset/1 resets a fuse back to its policy standard
reset(Name) ->
	fuse:reset(Name).
	
reset_pre(#state { installed = [] }) -> false;
reset_pre(#state { installed = [_|_] }) -> true.

reset_args(S) ->
	[oneof(names(S))].

reset_post(_S, _, Ret) ->
	eq(Ret, ok).

%%% ask_pos/1 asks about the state of a fuse that exists
%%% ---------------------
ask_pos(Name) ->
	fuse:ask(Name).
	
ask_pos_pre(#state { installed = [] }) -> false;
ask_pos_pre(#state { installed = [_|_]}) -> true.

ask_pos_args(S) ->
	[oneof(names(S))].

count(Name, #state { installed = Inst }) ->
	{Name, Count} = lists:keyfind(Name, 1, Inst),
	Count.

count_state(0) -> blown;
count_state(_N) -> ok.

ask_pos_post(S, [Name], Ret) ->
	eq(Ret, count_state(count(Name, S))).
	
%%% ask_neg/1 asks about the state of a fuse which does not exist
ask_neg(Name) ->
	fuse:ask(Name).
	
ask_neg_pre(S) ->
	case fuses() -- names(S) of
	    [] -> false;
	    [_|_] -> true
	end.
	
ask_neg_args(S) ->
	[oneof(fuses() -- names(S))].
	
ask_neg_post(_S, _, Ret) ->
	eq(Ret, {error, no_such_fuse_name}).
	
%%% melt/1 melts the fuse a little bit
%%% ---------------------
%% melt(Name) ->
%% 	fuse:melt(Name).
%% 	
%% melt_pre(#state { installed = [] }) -> false;
%% melt_pre(#state { installed = [_|_]}) -> true.
%% 
%% melt_args(S) ->
%% 	[oneof(names(S))].
%% 
%% melt_next(#state { installed = Is } = S, _V, [Name]) ->
%% 	{Name, Count} = lists:keyfind(Name, 1, Is),
%% 	S#state { installed = lists:keystore(Name, 1, Is, {Name, case Count of 0 -> 0; N -> N-1 end}) }.
%% 
%% melt_post(_S, _, Ret) ->
%% 	eq(Ret, ok).

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

names(#state { installed = Is }) ->
	[N || {N, _} <- Is].
