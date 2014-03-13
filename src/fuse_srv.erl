%%% @doc Runs the fuse server in the system
%%% @private
-module(fuse_srv).
-behaviour(gen_server).

%% Lifetime API
-export([start_link/0]).

%% Operational API
-export([install/2, ask/1]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-define(TAB, fuse_state).

-record(state, { fuses = [] }).
-record(fuse, {
	name :: atom(),
	max_r :: integer(),
	max_t :: integer(),
	reset :: integer(),
	history = []
}).


%% ------
%% @doc Start up the manager server for the fuse system
%% This is assumed to be called by (@see fuse_sup).
%% @end
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------
%% @doc install/2 installs a new fuse into the running system
%% Install a new fuse under `Name' with options given by `Opts'.
%% We assume `Opts' are already in the right verified and validated format.
%% @end
install(Name, Opts) ->
	%% Assume options are already verified
	Fuse = init_state(Name, Opts),
	gen_server:call(?MODULE, {install, Fuse}).

%% @doc ask/1 asks about the current given fuse state
%% The documentation is (@see fuse:ask/2)
%% @end
-spec ask(atom()) -> ok | blown | {error, no_such_fuse_name}.
ask(Name) ->
    try ets:lookup_element(?TAB, Name, 2) of
        ok -> ok;
        blown -> blown
    catch
        error:badarg ->
            {error, no_such_fuse_name}
    end.
        
%% @private
init([]) ->
	_ = ets:new(?TAB, [named_table, protected, set, {read_concurrency, true}, {keypos, 1}]),
	{ok, #state{}}.

%% @private
handle_call({install, #fuse { name = Name} = Fuse}, _From, #state { fuses = Fs } = State) ->
	ok = mk_fuse_state(Name),
	{reply, ok, State#state { fuses = lists:keystore(Name, #fuse.name, Fs, Fuse) }};
handle_call(_M, _F, State) ->
	{reply, {error, unknown}, State}.
	
%% @private
handle_cast(_M, State) ->
	{noreply, State}.

%% @private
handle_info(_M, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Internal functions
%%% ------

mk_fuse_state(Name) ->
    true = ets:insert(?TAB, {Name, ok}),
    ok.

init_state(Name, {{standard, MaxR, MaxT}, {reset, Reset}}) ->
    #fuse { name = Name, max_r = MaxR, max_t = MaxT, reset = Reset }.
