%%% @doc Event manager for fuses.
%%% @end
-module(fuse_event).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

%% Lifetime API
-export([start_link/0]).

%% Handler manipulation
-export([add_handler/2,
         delete_handler/2]).

%% Private API
-export([notify/1]).

-define(SERVER, ?MODULE).

%% @doc Starts up the event handler.
%% @end
%% start_link/0
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @doc Adds a new event handler.
%% <p>The documentation is @see //stdlib/gen_event. specific. So use that in
%% order to understand the interface here.</p>
%% @end
%% add_handler/2
-spec add_handler(atom() | pid(), [term()]) -> ok.
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%% @doc Adds a new event handler.
%% <p>The documentation is <a href="http://erlang.org/doc/man/gen_event.html">gen_event</a> specific. So use that in
%% order to understand the interface here.</p>
%% @end
%% delete_handler/2
-spec delete_handler(atom() | pid(), [term()]) -> ok.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%% @private
-spec notify(term()) -> ok.
notify(What) ->
    gen_event:notify(?SERVER, What).
