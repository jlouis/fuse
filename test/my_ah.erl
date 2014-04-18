-module(my_ah).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	pid :: pid()
}).

init({Args, _Old}) -> init(Args);
init([Pid]) ->
	{ok, #state{ pid = Pid }}.
	
handle_event(Evt, #state { pid = Pid } = S) ->
	Pid ! Evt,
	{ok, S}.
	
handle_call(Request, #state { pid = Pid } = S) ->
	Pid ! {call, Request},
	{ok, ok, S}.
	
handle_info(Info, #state { pid = Pid } = S) ->
	Pid ! {info, Info},
	{ok, S}.
	
terminate(_Arg, _S) -> ok.

code_change(_Old, S, _Extra) -> {ok, S}.


	

