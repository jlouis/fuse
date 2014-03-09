%%% @doc Runs the fuse server in the system
-module(fuse_srv).
-behaviour(gen_server).

%% Lifetime API
-export([start_link/0]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
init([]) ->
	{ok, #state{}}.

handle_call(_M, _F, State) ->
	{reply, {error, unknown}, State}.
	
handle_cast(_M, State) ->
	{noreply, State}.

handle_info(_M, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
