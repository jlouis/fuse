%%% @doc fuse_stats_exometer - use exometer spirals for fuse stats.
%%% Assumes that you have already arranged to start exometer.
-module(fuse_stats_exometer).
-behaviour(fuse_stats_plugin).
-export([init/1, increment/2]).

%% @doc Initialize exometer for `Name'.
-spec init(Name :: atom()) -> ok.
init(Name) ->
    _ = exometer:new(metric(Name, ok), spiral),
    _ = exometer:new(metric(Name, blown), spiral),
    _ = exometer:new(metric(Name, melt), spiral),
    ok.

%% @doc Increment `Name's `Counter' spiral.
-spec increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
increment(Name, Counter) ->
    _ = exometer:update(metric(Name, Counter), 1),
    ok.

%% Internal.
metric(Name, Counter) ->
    [fuse, Name, Counter].
