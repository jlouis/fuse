%%% @doc fuse_stats_folsom - use <a href="https://github.com/boundary/folsom">folsom</a> spirals for fuse stats.
%%% Assumes that you have already arranged to start folsom.
-module(fuse_stats_folsom).
-behaviour(fuse_stats_plugin).
-export([init/1, increment/2]).

%% @doc Initializes folsom for `Name'.
%% @end
%% init/1
-spec init(Name :: atom()) -> ok.
init(Name) ->
    _ = folsom_metrics:new_spiral(metric(Name, ok)),
    _ = folsom_metrics:new_spiral(metric(Name, blown)),
    _ = folsom_metrics:new_spiral(metric(Name, melt)),
    ok.

%% @doc Increments `Name''s `Counter' spiral.
%% @end
%% increment/2
-spec increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
increment(Name, Counter) ->
    _ = folsom_metrics:notify({metric(Name, Counter), 1}),
    ok.

%% Internal.
metric(Name, Counter) ->
    B = iolist_to_binary([atom_to_list(Name), $., atom_to_list(Counter)]),
    binary_to_atom(B, utf8).
