%%% @doc Application startup callback
%%% @private
-module(fuse_app).
-behaviour(application).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-export([start/2, stop/1]).

start(_Type, _Args) ->
	fuse_sup:start_link().
	
stop(_State) ->
	ok.
