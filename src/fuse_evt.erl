%%% @doc Event manager for fuses
%%% @end
-module(fuse_evt).

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

%% @doc start_link/0 starts up the event handler
%% @end
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @doc add_handler/2 adds a new event handler
%% <p>The documentation is @see //stdlib/gen_event. specific. So use that in
%% order to understand the interface here.</p>
%% @end
-spec add_handler(atom() | pid(), [term()]) -> ok.
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%% @doc delete_handler/2 adds a new event handler
%% <p>The documentation is @see //stdlib/gen_event. specific. So use that in
%% order to understand the interface here.</p>
%% @end
-spec delete_handler(atom() | pid(), [term()]) -> ok.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%% @private
-spec notify(term()) -> ok.
notify(What) ->
    gen_event:notify(?SERVER, What).

