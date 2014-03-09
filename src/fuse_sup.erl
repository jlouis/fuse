%%% @doc Fuse main supervisor
-module(fuse_sup).
-behaviour(supervisor).

-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(CHILD(I, Args), {I, {I, start_link, Args},
                             permanent, 5000, worker, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------
init([]) ->
	{ok, { {one_for_one, 5, 3600}, [?CHILD(fuse_srv, [])]}}.
