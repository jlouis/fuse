%%% @doc fuse_stats_ets - maintain fuse counters in an ETS table.
-module(fuse_stats_ets).
-behaviour(fuse_stats_plugin).
-export([init/1, increment/2, counters/1]).

%% @doc Initialize `Name'.
%% Creates the stats ETS table if it doesn't already exist.
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

%% @doc Increment `Name's `Counter'.
-spec increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
increment(Name, Counter) ->
    _ = ets:update_counter(?MODULE, metric(Name, Counter), 1),
    ok.

%% @doc Fetch `Name's counters.
-spec counters(Name :: atom()) -> [proplists:property()].
counters(Name) ->
    [{Counter, ets:lookup_element(?MODULE, metric(Name, Counter), 2)} ||
        Counter <- [ok, blown, melt]].

%% Internal.
metric(Name, Counter) ->
    {Name, Counter}.
