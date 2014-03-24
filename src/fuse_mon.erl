%%% @doc fuse_mon monitors the fuse state and reports it
%%% @private
-module(fuse_mon).
-behaviour(gen_server).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

%% Lifetime
-export([start_link/1]).

%% API
-export([sync/0]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {
	timing = automatic,
	history = []
}).

-define(PERIOD, 60*1000).

%% Lifetime
start_link(Timing) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Timing], []).

%% API
%% @private
sync() ->
	gen_server:call(?MODULE, sync).

%% Callbacks

%% @private
init([Timing]) when Timing == manual; Timing == automatic ->
	S = #state { timing = Timing },
	{ok, set_timer(S)}.
	
%% @private
handle_call(sync, _F, State) ->
	{reply, ok, State};
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
	Fuses = ets:match_object(fuse_srv, '_'),
	State2 = track_histories(Fuses, State),
	report(State2),
	State2.
	
track_histories(_Fuses, State) -> State.

report(_State) -> ok.

set_timer(#state { timing = manual } = S) -> S;
set_timer(#state { timing = automatic } = S) ->
	erlang:send_after(?PERIOD, self(), timeout),
	S.
