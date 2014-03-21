%%% @doc fuse_mon monitors the fuse state and reports it
%%% @private
-module(fuse_mon).
-behaviour(gen_server).

%% Lifetime
-export([start_link/1]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {
	timing = automatic
}).
-define(PERIOD, 60*1000).
%% Lifetime
start_link(Timing) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Timing], []).

%% Callbacks

%% @private
init([Timing]) when Timing == manual; Timing == automatic ->
	S = #state { timing = Timing },
	{ok, set_timer(S)}.
	
%% @private
handle_call(_M, _F, State) ->
	{reply, {error, unknown}, State}.

%% @private
handle_cast(_M, State) ->
	{noreply, State}.

%% @private
handle_info(timeout, State) ->
	NewState = handle_fuses(State),
	{noreply, set_timer(NewState)};
handle_info(_M, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%%% Internal API

handle_fuses(State) ->
	State.

set_timer(#state { timing = manual } = S) -> S;
set_timer(#state { timing = automatic } = S) ->
	erlang:send_after(?PERIOD, self(), timeout),
	S.