%%% @doc Runs the fuse server in the system
%%% @private
-module(fuse_srv).
-behaviour(gen_server).

%% Lifetime API
-export([start_link/0]).

%% Operational API
-export([install/2]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {}).

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
	gen_server:call(?MODULE, {install, Name, Opts}).

%% @private
init([]) ->
	{ok, #state{}}.

%% @private
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
