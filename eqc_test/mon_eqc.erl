-module(mon_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-record(state, {
	history = orddict:new(),
	alarms = []
}).

%% API specification
api_spec() -> 
	#api_spec {
		language = erlang,
		modules = []
	}.

%% Generators
fuses() ->
	[heinz, phineas, perry].

g_fuse() ->
	oneof(fuses()).

g_installed(#state { history = Is }) ->
	oneof([N || {N, _} <- Is]).

g_state() ->
	oneof([ok, blown]).

%% Initial state
initial_state() -> #state{}.

%% Install a new fuse
install(_Name, _FuseSt) ->
	ok.
	
install_args(_S) ->
	[g_fuse(), g_state()].

install_callouts(_S, [Name, FuseSt]) ->
	?SELFCALL(track_state_history, [Name, FuseSt]).
	
%% Remove a fuse
remove(_Name) ->
	ok.
	
remove_pre(#state { history = [] }) -> false;
remove_pre(#state{}) -> true.

remove_args(S) ->
	[g_installed(S)].

remove_next(#state { history = Is} = S, _V, [Name]) ->
	Update = orddict:erase(Name, Is),
	S#state { history = Update }.

%% Update the fuse state
update(_Name, _FuseSt) ->
	ok.
	
update_pre(#state { history = [] }) -> false;
update_pre(#state{}) -> true.

update_args(S) ->
	[g_installed(S), g_state()].
	
update_callouts(_S, [Name, FuseSt]) ->
	?SELFCALL(track_state_history, [Name, FuseSt]).

process(Entries) ->
	make_table(Entries),
	fuse_mon ! timeout,
	fuse_mon:sync(). 
	
process_args(#state { history = Hs }) ->
	Entries = mk_entries(Hs),
	[Entries].

process_callouts(#state { alarms = Alarms, history = History}, [_Hs]) ->
	callouts_from_history(Alarms, History).

callouts_from_history(_Alarms, []) -> ?EMPTY;
callouts_from_history(Alarms, [{Name, Hist} | Rest]) ->
	case transition_alarms(lists:member(Name, Alarms), Hist) of
	    set ->
	    	?PAR(
	    		?SEQ(
	    		  ?CALLOUT(alarm_handler, set_alarm, [{Name, fuse_blown}], ok),
	    		  ?SELFCALL(set, [Name])),
	    		callouts_from_history(Alarms, Rest));
	    clear ->
	    	?PAR(
	    		?SEQ(
	    		  ?CALLOUT(alarm_handler, clear_alarm, [Name], ok),
	    		  ?SELFCALL(clear, [Name])),
	    		callouts_from_history(Alarms, Rest));
	    noop ->
	    	callouts_from_history(Alarms, Rest)
	end.

transition_alarms(Triggered, History) ->
	Blowns = length([H || H <- History, H == blown]),
	case Triggered of
	    false when Blowns > 0 -> set;
	    false -> noop;
	    true when Blowns > 0 -> noop;
	    true when Blowns == 0 -> clear
	end.
	    
%%% Internals SELFCALLS
set_next(#state { alarms = As } = S, _V, [Name]) ->
	S#state { alarms = [Name | As] }.
	
clear_next(#state { alarms = As } = S, _V, [Name]) ->	
	S#state { alarms = [A || A <- As, A /= Name]}.

track_state_history_next(#state { history = Installed } = S, _V, [Name, FuseSt]) ->
	Update = orddict:update(Name,
	    fun(History) ->
	        case [FuseSt | History] of
	          Hist when length(Hist) > 3 -> take(3, Hist);
	          Hist -> Hist
	        end
	    end,
	    [FuseSt],
	    Installed),
	S#state { history = Update }.

startup() ->
	{ok, _Pid} = fuse_mon:start_link(manual),
	ok.
	
cleanup() ->
	process_flag(trap_exit, true),
	exit(whereis(fuse_mon), stoppitystop),
	receive
		{'EXIT', _Pid, stoppitystop} -> ok
	end,
	process_flag(trap_exit, false),
	ok.
	
%%% The property of the model
prop_component_correct() ->
	?SETUP(fun() ->
		ets:new(fuse_srv, [named_table, public]),
		eqc_mocking:start_mocking(api_spec()),
		fun() -> ets:delete(fuse_srv), ok end
	end,
	?FORALL(Cmds, commands(?MODULE),
	  begin
	  	ok = startup(),
	  	{H, S, Result} = run_commands(?MODULE, Cmds),
	  	ok = cleanup(),
	  	pretty_commands(?MODULE, Cmds, {H, S, Result},
	  		Result == ok)
	  end)).
	  
%%% Internals
make_table(Entries) ->
	true = ets:delete_all_objects(fuse_srv),
	true = ets:insert_new(fuse_srv, Entries),
	ok.
	
mk_entries([{Name, [St | _]} | Rest]) ->
	[{Name, St} | mk_entries(Rest)];
mk_entries([{_Name, []} | Rest]) ->
	mk_entries(Rest);
mk_entries([]) -> [].

take(N, L) ->
	{T, _} = lists:split(N, L),
	T.
