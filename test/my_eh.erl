-module(my_eh).

-behaviour(gen_event).

-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2, code_change/3]).

-record(state, {
	pid :: pid()
}).

init([Pid]) ->
	{ok, #state { pid = Pid }}.
	
handle_event(Evt, #state { pid = Pid } = State) ->
	Pid ! Evt,
	{ok, State}.
	
handle_call(Request, #state { pid = Pid } = State) ->
	Pid ! {call, Request},
	{ok, ok, State}.
	
handle_info(Info, #state { pid = Pid } = State) ->
	Pid ! {info, Info},
	{ok, State}.

terminate(_Arg, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
