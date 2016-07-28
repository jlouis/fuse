%%% @doc fuse_stats_ets - maintain fuse counters in an ETS table.
-module(fuse_stats_ets).
-behaviour(fuse_stats_plugin).
-export([init/1, increment/2, counters/1]).

%% @doc Initializes `Name'.
%% <p>Creates the stats ETS table if it doesn't already exist.</p>
%% @end
%% init/1
-spec init(Name :: atom()) -> ok.
init(Name) ->
    _ = case ets:info(?MODULE) of
        undefined ->
            _ = ets:new(?MODULE, [named_table, public, set,
                                  {write_concurrency, true}]);
        _ ->
            ok
    end,
    _ = ets:insert(?MODULE, [{metric(Name, ok), 0},
                             {metric(Name, blown), 0},
                             {metric(Name, melt), 0}]),
    ok.

%% @doc Increments `Name''s `Counter'.
%% @end
%% increment/2
-spec increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
increment(Name, Counter) ->
    _ = ets:update_counter(?MODULE, metric(Name, Counter), 1),
    ok.

%% @doc Fetches `Name''s counters.
%% @end
%% counters/1
-spec counters(Name :: atom()) -> [proplists:property()].
counters(Name) ->
    [{Counter, ets:lookup_element(?MODULE, metric(Name, Counter), 2)} ||
        Counter <- [ok, blown, melt]].

%% Internal.
metric(Name, Counter) ->
    {Name, Counter}.
