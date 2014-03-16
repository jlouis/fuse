-module(fuse_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("pulse/include/pulse.hrl").
-include_lib("pulse_otp/include/pulse_otp.hrl").

-compile(export_all).

-record(state, {
	installed = []
}).

fuses() -> [phineas, ferb, candace, perry, heinz].

valid_fuse(F) ->
	lists:member(F, fuses()).

g_atom() ->
	oneof([a,b,c,d,e,f]).

g_name() ->
	  oneof(fuses()).

g_neg_int() ->
	?LET(N, nat(),
		-(N+1)).

g_strategy() ->
	fault(
		{frequency([
			{1, {g_atom(), int(), int()}},
			{1, {standard, g_neg_int(), int()}},
			{1, {standard, int(), g_neg_int()}}
		])},
		{standard, 60, int()}
	).

g_refresh() ->
	{reset, 60000}.
	
g_options() ->
	{g_strategy(), g_refresh()}.

initial_state() ->
	#state{}.

%%% install/2 puts a new fuse into the system
%%% ---------------------
install(Name, Opts) ->
	try fuse:install(Name, Opts) of
		ok -> ok
	catch
		error:badarg ->
			badarg
	end.

install_args(_S) ->
	[g_name(), g_options()].

install_next(#state{ installed = Is } = S, _V, [Name, Opts]) ->
	case valid_opts(Opts) of
	    false ->
	        S;
	    true ->
	        {{_, Count, _}, _} = Opts,
	        T = {Name, Count},
	        S#state { installed = lists:keystore(Name, 1, Is, T) }
	end.

install_post(_S, [_Name, Opts], R) ->
	case valid_opts(Opts) of
	    true -> eq(R, ok);
	    false -> eq(R, badarg)
	end.

%%% reset/1 resets a fuse back to its policy standard
reset(Name) ->
	fuse:reset(Name).
	
reset_pre(#state { installed = [] }) -> false;
reset_pre(#state { installed = [_|_] }) -> true.

reset_args(S) ->
	[oneof(installed_names(S))].

reset_post(S, [Name], Ret) ->
    case is_installed(Name, S) of
        true -> eq(Ret, ok);
        false -> eq(Ret, {error, no_such_fuse_name})
    end.

is_installed(N, #state { installed = Is }) -> lists:keymember(N, 1, Is).

%%% ask/1 asks about the state of a fuse that exists
%%% ---------------------
ask(Name) ->
	fuse:ask(Name).
	
ask_pre(#state { installed = [] }) -> false;
ask_pre(#state { installed = [_|_]}) -> true.

ask_args(_S) ->
	[g_name()].
	
ask_post(S, [Name], Ret) ->
	case is_installed(Name, S) of
	    true ->
	        eq(Ret, count_state(count(Name, S)));
	    false ->
	        eq(Ret, {error, no_such_fuse_name})
	end.

%%% run/1 runs a function (thunk) on the circuit breaker
%%% ---------------------
run(Name, _Result, _Return, Fun) ->
	fuse:run(Name, Fun).
	
run_pre(S) ->
	has_fuses_installed(S).

run_args(S) ->
    ?LET({N, Result, Return}, {oneof(installed_names(S)), oneof([ok, melt]), int()},
      [N, Result, Return, function0({Result, Return})] ).
      
run_next(S, _V, [_Name, ok, _, _]) -> S;
run_next(#state { installed = Is} = S, _V, [Name, melt, _, _]) ->
	case lists:keyfind(Name, 1, Is) of
	    {Name, Count} -> S#state { installed = lists:keystore(Name, 1, Is, {Name, case Count of 0 -> 0; N -> N-1 end}) };
	    false -> S
	end.

run_post(S, [Name, _Result, Return, _], Ret) ->
	case is_installed(Name, S) of
	    true ->
	        case count_state(count(Name, S)) of
	            ok -> eq(Ret, {ok, Return});
	            blown -> eq(Ret, blown)
	        end;
	    false ->
	        eq(Ret, {error, no_such_fuse_name})
	end.

%%% melt/1 melts the fuse a little bit
%%% ---------------------
melt(Name) ->
	fuse:melt(Name).
	
melt_pre(#state { installed = [] }) -> false;
melt_pre(#state { installed = [_|_]}) -> true.

melt_args(S) ->
 	[oneof(installed_names(S))].

melt_next(#state { installed = Is } = S, _V, [Name]) ->
	case lists:keyfind(Name, 1, Is) of
	    {Name, Count} -> S#state { installed = lists:keystore(Name, 1, Is, {Name, case Count of 0 -> 0; N -> N-1 end}) };
	    false -> S
	end.

melt_post(_S, _, Ret) ->
	eq(Ret, ok).

%%% Properties

%% Sequential test
prop_model_seq() ->
    fault_rate(1, 10,
	?FORALL(Cmds, commands(?MODULE, #state{}),
	  begin
	  	cleanup(),
	  	{H, S, R} = run_commands(?MODULE, Cmds),
	  	?WHENFAIL(
	  		io:format("History: ~p\nState: ~p\nResult: ~p\n", [H, S, R]),
	  		aggregate(command_names(Cmds), R == ok))
	  end)).

prop_model_par() ->
    fault_rate(1, 10,
      ?FORALL(Repetitions, ?SHRINK(1, [10]),
	?FORALL(ParCmds, parallel_commands(?MODULE, #state{}),
	  ?ALWAYS(Repetitions,
	  begin
	  	cleanup(),
	  	{H, S, R} = run_parallel_commands(?MODULE, ParCmds),
	  	?WHENFAIL(
	  		io:format("History: ~p\nState: ~p\nResult: ~p\n", [H, S, R]),
	  		aggregate(command_names(ParCmds), R == ok))
	  end)))).

prop_model_pulse() ->
  ?SETUP(fun() -> N = erlang:system_flag(schedulers_online, 1),
         	fun() -> erlang:system_flag(schedulers_online, N) end end,
  ?FORALL(Cmds, parallel_commands(?MODULE),
  ?PULSE(HSR={_, _, R},
    begin
      cleanup(),
      run_parallel_commands(?MODULE, Cmds)
    end,
    aggregate(command_names(Cmds),
    pretty_commands(?MODULE, Cmds, HSR,
      R == ok))))).

cleanup() ->
  error_logger:tty(false),
  (catch application:stop(fuse)),
  ok = application:start(fuse).

%%% INTERNALS
%%% ---------------------

%% Pick amongst the installed names
installed_names(#state { installed = Is }) ->
	[N || {N, _} <- Is].

%% Determine if opts is valid
valid_opts({{standard, K, R}, {reset, T}})
    when K >= 0, R >= 0, T >= 0 ->
	true;
valid_opts(_) ->
	false.
	
count(Name, #state { installed = Inst }) ->
	{Name, Count} = lists:keyfind(Name, 1, Inst),
	Count.

count_state(0) -> blown;
count_state(_N) -> ok.

has_fuses_installed(#state { installed = [] }) -> false;
has_fuses_installed(#state { installed = [_|_]}) -> true.


%% PULSE instrumentation,
the_prop() -> prop_model_pulse().

test({N, h})   -> test({N * 60, min});
test({N, min}) -> test({N * 60, sec});
test({N, s})   -> test({N, sec});
test({N, sec}) ->
  quickcheck(eqc:testing_time(N, the_prop()));
test(N) when is_integer(N) ->
  quickcheck(numtests(N, the_prop())).

test() -> test(100).

recheck() -> eqc:recheck(the_prop()).
check()   -> eqc:check(the_prop()).
check(CE) -> eqc:check(the_prop(), CE).

verbose()   -> eqc:check(eqc_statem:show_states(the_prop())).
verbose(CE) -> eqc:check(eqc_statem:show_states(the_prop(), CE)).

pulse_instrument() ->
  [ pulse_instrument(File) || File <- filelib:wildcard("../src/*.erl") ++ filelib:wildcard("../eqc_test/*.erl") ].

pulse_instrument(File) ->
  {ok, Mod} = compile:file(File, [{d, 'PULSE', true},
                                  {parse_transform, pulse_instrument},
                                  {pulse_side_effect, [{ets, '_', '_'}]}]),
  code:purge(Mod),
  code:load_file(Mod),
  Mod.
