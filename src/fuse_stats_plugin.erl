%%% @doc The fuse_stats_plugin behaviour.
%%% All {@link fuse} stats plugins implement the callbacks defined in this
%%% <a href="http://www.erlang.org/doc/design_principles/des_princ.html">
%%% behaviour</a>.
%%%
%%% As EDoc does not yet support @@doc tags for the `-callback' method of
%%% behaviour specification, documentation on the callbacks is included here.
%%% See the source for the callback type annotations.
%%%
%%% Note that since metrics export is very much a secondary function of fuse,
%%% plugins should consider their work "best effort" and as such not crash on
%%% e.g. a badmatch.
%%%
%%% === init/1 ===
%%% Handles plugin initialization.
%%% <ul>
%%% <li>`Name' is the fuse name.</li>
%%% </ul>
%%% The implementation must create counters and/or whatever is necessary to
%%% setup the plugin's statistics store.
%%%
%%% === increment/2 ===
%%% Increments a counter.
%%% <ul>
%%% <li>`Name' is the fuse name.</li>
%%% <li>`Counter' is one of `ok', `blown' or `melt'.</li>
%%% </ul>
%%% The implementation must update or notify counters, spirals or whatever
%%% the underlying implementation is.
-module(fuse_stats_plugin).

-callback init(Name :: atom()) -> ok.

-callback increment(Name :: atom(), Counter :: ok | blown | melt) -> ok.
