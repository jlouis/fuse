%%% @doc Application startup callback
-module(fuse_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	fuse_sup:start_link().
	
stop(_State) ->
	ok.
