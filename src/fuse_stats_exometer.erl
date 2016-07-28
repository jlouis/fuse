%%% @doc fuse_stats_exometer - use <a href="https://github.com/Feuerlabs/exometer">exometer</a> spirals for fuse stats.
%%% Assumes that you have already arranged to start exometer.
-module(fuse_stats_exometer).
-behaviour(fuse_stats_plugin).
-export([init/1, increment/2]).

%% @doc Initializes exometer for `Name'.
%% @end
%% init/1
-spec init(Name :: atom()) -> ok.
init(Name) ->
    _ = exometer:new(metric(Name, ok), spiral),
    _ = exometer:new(metric(Name, blown), spiral),
    _ = exometer:new(metric(Name, melt), spiral),
    ok.

%% @doc Increments `Name''s `Counter' spiral.
%% @end
%% increment/2
-spec increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
increment(Name, Counter) ->
    _ = exometer:update(metric(Name, Counter), 1),
    ok.

%% Internal.
metric(Name, Counter) ->
    [fuse, Name, Counter].
