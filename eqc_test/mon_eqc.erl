-module(mon_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-record(state, {
	installed = [],
	history = orddict:new(),
	alarms = []
}).

%% API specification
api_spec() -> 
	#api_spec {
		language = erlang,
		modules = [
                    #api_module {
                       name = alarm_handler,
                       functions = [ #api_fun { name = set_alarm, arity = 1},
                                     #api_fun { name = clear_alarm, arity = 1}
                                   ]
                    }
                ]
	}.

%% Generators
fuses() ->
	[heinz, phineas, ferb, isabella, candace, vanessa, major_monogram, perry].

g_fuse() ->
	elements(fuses()).


%% Bad idea to generate from the state in this way, rather use _args, 
%% since then precondition is considered!
%% g_installed(#state { installed = Is }) ->
%%	elements(Is).

%% elements, since we want to shrink blown to default state (ok)
g_state() ->
	elements([ok, blown]).

%% Initial state
initial_state() -> #state{ }.

%% Install a new fuse
install(_Name) ->
	ok.

%% Only install new fuses
%% Since there is no code related to installation, there is no purpose to test
%% re-installation of existing fuses.
install_pre(#state { installed = Is }, [Name]) ->
	not lists:member(Name, Is).

%% We might pick an existing fuse, but we will then filter with a precondition, 
%% which we anyway need for shrinking.
install_args(_S) ->
	[g_fuse()].

%% Add new fuse at the end for better shrinking behaviour
install_next(#state { installed = Is } = S, _V, [Name]) ->
        S#state { installed = Is ++ [Name] }.

process(Entries) ->
	make_table(Entries),
	fuse_mon ! timeout,
	fuse_mon:sync(). 
	
%% We even consider processing without any installed fuses
process_args(#state { installed = Is }) ->
        [ [{I, g_state()} || I<-Is] ].
        %% I like list comprehensions, since they make for readable code
	%% K = length(Is),
	%% ?LET(Vs, vector(K, g_state()),
	%%     [lists:zip(Is, Vs)]).

process_callouts(#state { alarms = Alarms, history = History }, [Entries]) ->
	?SEQ(track_entries(Entries) ++
	     callouts_from_history(Alarms, History, lists:sort(Entries))).
	         
track_entries(Entries) ->
    [?SELFCALL(track_history, [N, St]) || {N, St} <- Entries].

callouts_from_history(_Alarms, _History, []) -> [];
callouts_from_history(Alarms, History, [{N, gone} | Rest]) ->
	[?SEQ(
	    ?CALLOUT(alarm_handler, clear_alarm, [N], ok),
	    ?SELFCALL(clear, [N])) |
	callouts_from_history(Alarms, History, Rest)];
callouts_from_history(Alarms, History, [{N, V} | Rest]) ->
	case transition_alarms(lists:member(N, Alarms), V, lists:keyfind(N, 1, History)) of
	    set ->
	    		[?SEQ(
	    		  ?CALLOUT(alarm_handler, set_alarm, [{N, fuse_blown}], ok),
	    		  ?SELFCALL(set, [N])) |
	    		callouts_from_history(Alarms, History, Rest)];
	    clear ->
	    		[?SEQ(
	    		  ?CALLOUT(alarm_handler, clear_alarm, [N], ok),
	    		  ?SELFCALL(clear, [N])) |
	    		callouts_from_history(Alarms, History, Rest)];
	    noop ->
	    	callouts_from_history(Alarms, History, Rest)
	end.

transition_alarms(Triggered, V, false) -> transition_alarms(Triggered, V, []);
transition_alarms(Triggered, V, {_, Hs}) -> transition_alarms(Triggered, V, Hs);
transition_alarms(Triggered, V, HEs) ->
	RecordedHistory = take(3, [V|HEs]),
	Blowns = length([H || H <- RecordedHistory, H == blown]),
	case Triggered of
	    false when Blowns > 0 -> set;
	    false -> noop;
	    true when Blowns > 0 -> noop;
	    true when Blowns == 0 -> clear
	end.
	    
%%% Internal SELFCALLS
set_next(#state { alarms = As } = S, _V, [Name]) ->
	S#state { alarms = [Name | As] }.
	
clear_next(#state { alarms = As } = S, _V, [Name]) ->	
	S#state { alarms = [A || A <- As, A /= Name]}.

track_history_next(#state { history = Installed } = S, _V, [N, V]) ->
	Update = orddict:update(N,
     	  fun(Hist) ->
     	      case [V | Hist] of
     	        H when length(H) > 3 -> take(3, H);
     	        H -> H
     	      end
     	  end,
     	  [V],
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
        ?TRAPEXIT(
	  begin
	  	ok = startup(),
	  	{H, S, R} = run_commands(?MODULE, Cmds),
	  	ok = cleanup(),
	  	pretty_commands(?MODULE, Cmds, {H, S, R},
	  		aggregate(command_names(Cmds), R == ok))
	  end))).
	  
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

take(N, L) when length(L) < N -> L;
take(N, L) ->
	{T, _} = lists:split(N, L),
	T.
