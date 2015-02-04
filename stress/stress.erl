-module(stress).

-export([run/0]).
-compile(inline).

-define(CORES, 8).
-define(COUNT, 3 * 1000*1000).
-define(TIMEOUT, 30*1000).
-define(FUSE, test_fuse).

setup() ->
	application:load(sasl),
	application:set_env(sasl, sasl_error_logger, false),
	application:set_env(sasl, errlog_type, error),
	error_logger:tty(false),
	application:start(sasl),
	application:start(fuse),
	ok.

run() ->
    setup(),
    ok = fuse:install(?FUSE, {{standard, 20, 60}, {reset, 30*1000}}),
    timer:tc(fun() -> run(?CORES) end).
    
run(CoreCount) ->
     Mgr = self(),
     Pids = [spawn_link(fun() -> loop(Mgr, ?COUNT) end) || _ <- lists:seq(1, CoreCount)],
     collect(Pids).
     
collect([]) -> ok;
collect([Pid | Pids]) ->
    receive
        {ok, Pid} -> collect(Pids)
    after ?TIMEOUT ->
        {error, timeout}
    end.

loop(Mgr, 0) ->
	Mgr ! {ok, self()};
loop(Mgr, N) ->
	ok = fuse:ask(?FUSE, async_dirty),
	loop(Mgr, N-1).
