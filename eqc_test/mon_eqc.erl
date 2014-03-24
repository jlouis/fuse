-module(mon_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-record(state, {
	installed = orddict:new()
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
	oneof([fuses()]).

g_installed(#state { installed = Is }) ->
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
	
install_next(#state { installed = Installed } = S, _V, [Name, FuseSt]) ->
	Update = orddict:update(Name,
	    fun(History) -> [FuseSt | History] end,
	    [FuseSt],
	    Installed),
	S#state { installed = Update }.
	
%% Remove a fuse
remove(_Name) ->
	ok.
	
remove_pre(#state { installed = [] }) -> false;
remove_pre(#state{}) -> true.

remove_args(S) ->
	[g_installed(S)].

remove_next(#state { installed = Is} = S, _V, [Name]) ->
	Update = orddict:erase(Name, Is),
	S#state { installed = Update }.

%% Update the fuse state
update(_Name, _FuseSt) ->
	ok.
	
update_pre(#state { installed = [] }) -> false;
update_pre(#state{}) -> true.

update_args(S) ->
	[g_installed(S), g_state()].
	
update_callouts(_S, [Name, FuseSt]) ->
	?SELFCALL(track_state_history, [Name, FuseSt]).

%%% Internals SELFCALLS
track_state_history_next(#state { installed = Installed } = S, _V, [Name, FuseSt]) ->
	Update = orddict:update(Name,
	    fun(History) -> [FuseSt | History] end,
	    [FuseSt],
	    Installed),
	S#state { installed = Update }.

%%% The property of the model
prop_component_correct() ->
	?SETUP(fun() -> eqc_mocking:start_mocking(api_spec()), fun() -> ok end end,
	?FORALL(Cmds, commands(?MODULE),
	  begin
	  	{H, S, Result} = run_commands(?MODULE, Cmds),
	  	pretty_commands(?MODULE, Cmds, {H, S, Result},
	  		Result == ok)
	  end)).
	  
%%% Internals
