%%% @doc fuse_stats_prometheus - use <a href="https://github.com/deadtrickster/prometheus.erl">prometheus</a> counters for fuse stats.
%%% Assumes that you have already arranged to start prometheus.
-module(fuse_stats_prometheus).
-behaviour(fuse_stats_plugin).
-export([init/1, increment/2]).

-define(RESPONSES_COUNTER_NAME(NameBin), <<NameBin/binary, "_responses_total">>).

-define(MELTS_COUNTER_NAME(NameBin), <<NameBin/binary, "_melts_total">>).

%% @doc Initializes prometheus counters for  `Name'.
%% Exports the following metrics:
%% <ul>
%% <li>`name_responses_total[type]'</li>
%% <li>`name_melts_total'.</li>
%% </ul>
%% Uses `default' registry.
%% @end
%% init/1
-spec init(Name :: atom()) -> ok.
init(Name) ->
    NameBin = atom_to_binary(Name, utf8),

    prometheus_counter:new([{name, ?RESPONSES_COUNTER_NAME(NameBin)},
                            {help, <<NameBin/binary, " fuse responses counter">>},
                            {labels, [type]}]),
    prometheus_counter:new([{name, ?MELTS_COUNTER_NAME(NameBin)},
                            {help, <<NameBin/binary, " fuse melts counter">>}]),
    ok.

%% @doc Increments `Name''s `Counter'.
%% @end
%% increment/2
-spec increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
increment(Name, Counter) ->
    NameBin = atom_to_binary(Name, utf8),
    case Counter of
        ok ->
            prometheus_counter:inc(?RESPONSES_COUNTER_NAME(NameBin), [ok]);
        blown ->
            prometheus_counter:inc(?RESPONSES_COUNTER_NAME(NameBin), [blown]);
        melt ->
            prometheus_counter:inc(?MELTS_COUNTER_NAME(NameBin))
    end,
    ok.
