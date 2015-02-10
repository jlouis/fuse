%%% @doc Fuse main supervisor
%%% @private
-module(fuse_sup).
-behaviour(supervisor).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(CHILD(I, Args), {I, {I, start_link, Args},
                             permanent, 5000, worker, [I]}).

%% @doc Start the fuse supervisor
%% @end
-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------
%% @private
init([]) ->
    {ok, { {rest_for_one, 5, 3600},
           [?CHILD(fuse_server, []),
            ?CHILD(fuse_event, []),
            ?CHILD(fuse_monitor, [])]}}.
