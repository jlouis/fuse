-module(fuse_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("pulse/include/pulse.hrl").
-include_lib("pulse_otp/include/pulse_otp.hrl").

-compile(export_all).

-record(state, {
	time = undefined,
	melts = [],
	reset_points = [],
	installed = []
}).

-define(PERIOD, 10).

%% Time handling

%% divrem/2 returns the integer division and remainder
divrem(X, Y) ->  {X div Y, X rem Y}.

%% Generators of usecs, seconds, and megaseconds. Defaults to simpler versions.
g_usecs( ) ->
	frequency([
		{1, choose(0, 1000000-1)},
		{5, choose(0, 100)},
		{15, 0}]).
	
g_secs() ->
   default(0, nat()).
	
g_mega() ->
    default(0, nat()).

%% Produce an initial time point, based on the above generators
g_initial_time() ->
    ?LET({Mega, Secs, Micros}, {nat(), g_secs(), g_usecs()},
        {1300 + Mega, Secs, Micros}).

%% Produce a time interval suitable for addition
g_add() ->
    {g_mega(), g_secs(), g_usecs()}.

g_time() ->
    oneof([g_add(), g_initial_time()]).

%% Add two time points
time_add({M1, S1, U1}, {M2, S2, U2}) ->
    {UCarry, Us} = divrem(U1 + U2, 1000*1000),
    {MCarry, S} = divrem(S1 + S2 + UCarry, 1000*1000),
    M = M1 + M2 + MCarry,
    {M, S, Us}.
    
%% Obtain the microsecond count of two time points
micros({Megas, Secs, Us}) ->
    S = Megas * 1000 * 1000 + Secs,
    Us + S * 1000 * 1000.
    
%% Test the correctness of the time model by running an addition property over it
prop_add_correct() ->
	?FORALL({X, Y}, {g_time(), g_time()},
		begin
			Way1 = micros(time_add(X, Y)),
			Way2 = micros(X) + micros(Y),
			equals(Way1, Way2)
		end
	).

%% Time forms a group
prop_add_commut() ->
	?FORALL({X, Y}, {g_time(), g_time()},
		begin
			equals(time_add(X, Y), time_add(Y, X))
		end
	).

prop_add_assoc() ->
	?FORALL({X, Y, Z}, {g_time(), g_time(), g_time()},
		begin
			A = time_add(time_add(X, Y), Z),
			B = time_add(X, time_add(Y, Z)),
			equals(A, B)
		end
	).

prop_add_identity() ->
	?FORALL({X}, {g_time()},
		begin
			conjunction([
				{right_add, equals(X, time_add(X, {0, 0, 0}))},
				{left_add, equals(X, time_add({0, 0, 0}, X))}
			])
		end
	).

%% API Generators
fuses() -> [phineas, ferb, candace, isabella, vanessa, perry, heinz].

valid_fuse(F) ->
	lists:member(F, fuses()).

g_atom() ->
	oneof([a,b,c,d,e,f]).

g_name() ->
	  oneof(fuses()).

g_installed(S) ->
	fault(g_name(), oneof(installed_names(S))).

%% g_neg_int/0 Generates a negative integer, or 0
g_neg_int() ->
	?LET(N, nat(),
		-N).

g_strategy() ->
	fault(
		{frequency([
			{1, {g_atom(), int(), int()}},
			{1, {standard, g_neg_int(), int()}},
			{1, {standard, int(), g_neg_int()}},
			{1, {standard, int(), int()}}
		])},
		{standard, default(1, default(1, choose(1, 3))), ?PERIOD}
	).

g_refresh() ->
	{reset, 60000}.
	
g_options() ->
	{g_strategy(), g_refresh()}.

g_initial_state() ->
    ?LET(T, g_initial_time(),
    	#state { time = T }).

%%% advance_time/1 is model internal and advances the time point in the model
advance_time(_Add) -> ok.

advance_time_pre(#state { time = Now, reset_points = [{Next, _} | _] }) when Now < Next -> true;
advance_time_pre(#state { time = Now, reset_points = [{Next, _} | _] }) when Now >= Next -> false;
advance_time_pre(#state { reset_points = [] } ) -> true.

advance_time_args(#state { time = T, reset_points = [] }) ->
	?LET(Add, g_add(),
	    [time_add(T, Add)]);
advance_time_args(#state { time = T, reset_points = [{RP, _} | _] }) ->
	?LET(Add, g_add(),
	    begin
	        Future = time_add(T, Add),
	        case Future < RP of
	            true -> [oneof([Future, RP])];
	            false -> [RP]
	        end
	    end).
	
advance_time_next(S, _V, [NewPoint]) ->
	S#state { time = NewPoint }.

%%% fuse_reset/2 sends timer messages into the SUT
fuse_reset(Name, _Ts) ->
    fuse_srv ! {reset, Name},
    fuse_srv:sync(),
    ok.

fuse_reset_pre(#state { time = T, reset_points = [{T, _}|_]} = S) -> resets_ok(S);
fuse_reset_pre(_S) -> false.

fuse_reset_args(#state { reset_points = [{T, N} | _] }) ->
    [N, T].

fuse_reset_next(#state { reset_points = [{T, _} | _] = RPs } = S, _V, [Name, _Ts]) ->
    case lists:keytake(Name, 2, RPs) of
    	{value, _, NewRPs} ->
    		clear_melts(Name,
        		  S#state { reset_points = NewRPs, time = T });
        	false ->
        		S
    end.

fuse_reset_post(_S, [_Name, _Ts], R) ->
	eq(R, ok).

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
	        clear_melts(Name,
	          clear_resets(Name,
	            S#state { installed = lists:keystore(Name, 1, Is, T) }))
	end.

install_post(_S, [_Name, Opts], R) ->
	case valid_opts(Opts) of
	    true -> eq(R, ok);
	    false -> eq(R, badarg)
	end.

%%% reset/1 resets a fuse back to its policy standard
reset(Name) ->
	fuse:reset(Name).

reset_pre(S) ->
	resets_ok(S) andalso has_fuses_installed(S).

reset_args(S) ->
	[g_installed(S)].

reset_post(S, [Name], Ret) ->
    case is_installed(Name, S) of
        true -> eq(Ret, ok);
        false -> eq(Ret, {error, not_found})
    end.

reset_next(S, _V, [Name]) ->
    case is_installed(Name, S) of
        false -> S;
        true ->
        		clear_resets(Name,
        		  clear_melts(Name,
        		    S))
    end.

%%% ask/1 asks about the state of a fuse that exists
%%% ---------------------
ask(Name) ->
	fuse:ask(Name, [sync]).
	
ask_pre(S) ->
	resets_ok(S) andalso has_fuses_installed(S).

ask_args(S) ->
	[g_installed(S)].
	
ask_post(S, [Name], Ret) ->
	case is_installed(Name, S) of
	    true ->
	    	eq(Ret, case is_blown(Name, S) of true -> blown; false -> ok end);
	    false ->
	        eq(Ret, {error, not_found})
	end.

%%% run/1 runs a function (thunk) on the circuit breaker
%%% ---------------------
run(Name, Ts, _Result, _Return, Fun) ->
	fuse:run(Name, Ts, Fun, [sync]).
	
run_pre(S) ->
	resets_ok(S) andalso has_fuses_installed(S).

run_args(#state { time = Ts} = S) ->
    ?LET({N, Result, Return}, {g_installed(S), oneof([ok, melt]), int()},
        [N, Ts, Result, Return, function0({Result, Return})] ).

run_next(S, _V, [_Name, _, ok, _, _]) -> S;
run_next(S, _V, [Name, Ts, melt, _, _]) ->
	case is_installed(Name, S) of
		true ->
		    record_melt_history(Name,
		      expire_melts(?PERIOD,
		        record_melt(Name, Ts,
		          S#state { time = Ts })));
		false -> S
	end.

run_post(S, [Name, _Ts, _Result, Return, _], Ret) ->
	case is_installed(Name, S) of
	    true ->
		case is_blown(Name, S) of
		    false -> eq(Ret, {ok, Return});
		    true -> true
		end;
	    false ->
	        eq(Ret, {error, not_found})
	end.


%%% melt/1 melts the fuse a little bit
%%% ---------------------
melt(Name, Ts) ->
	fuse:melt(Name, Ts).

melt_pre(S) ->
    resets_ok(S) andalso has_fuses_installed(S).

melt_args(#state { time = T } = S) ->
 	[g_installed(S), T].

melt_next(S, _V, [Name, Ts]) ->
	case is_installed(Name, S) of
		true ->
		    record_melt_history(Name,
		      expire_melts(?PERIOD,
		        record_melt(Name, Ts,
		          S#state { time = Ts })));
		false -> S
	end.

melt_post(_S, _, Ret) ->
	eq(Ret, ok).

%%% Weight distribution
weight(#state { installed = [] }, install) -> 10;
weight(_S, install) -> 2;
weight(_, reset) -> 1;
weight(_, fuse_reset) -> 10;
weight(_, melt) -> 10;
weight(_, run) -> 10;
weight(_, ask) -> 10;
weight(#state { reset_points = [] }, advance_time) -> 1;
weight(_S, advance_time) -> 10.



%%% PROPERTIES
%%% ---------------------
%% Sequential test
prop_model_seq() ->
    fault_rate(1, 40,
    	?FORALL(St, g_initial_state(),
	?FORALL(Cmds, commands(?MODULE, St),
	  begin
	  	cleanup(),
	  	{H, S, R} = run_commands(?MODULE, Cmds),
	  	?WHENFAIL(
	  		io:format("History: ~p\nState: ~p\nResult: ~p\n", [H, S, R]),
	  		aggregate(command_names(Cmds), R == ok))
	  end))).

prop_model_par() ->
    fault_rate(1, 40,
     ?FORALL(St, g_initial_state(),
     ?FORALL(Repetitions, ?SHRINK(1, [10]),
	?FORALL(ParCmds, parallel_commands(?MODULE, St),
	  ?ALWAYS(Repetitions,
	  begin
	  	cleanup(),
	  	{H, S, R} = run_parallel_commands(?MODULE, ParCmds),
	  	?WHENFAIL(
	  		io:format("History: ~p\nState: ~p\nResult: ~p\n", [H, S, R]),
	  		aggregate(command_names(ParCmds), R == ok))
	  end))))).

x_prop_model_pulse() ->
  ?SETUP(fun() -> N = erlang:system_flag(schedulers_online, 1),
         	fun() -> erlang:system_flag(schedulers_online, N) end end,
  ?FORALL(St, g_initial_state(),
  ?FORALL(Cmds, parallel_commands(?MODULE, St),
  ?PULSE(HSR={_, _, R},
    begin
      cleanup(),
      run_parallel_commands(?MODULE, Cmds)
    end,
    aggregate(command_names(Cmds),
    pretty_commands(?MODULE, Cmds, HSR,
      R == ok)))))).

cleanup() ->
  error_logger:tty(false),
  (catch application:set_env(fuse, timing, manual)),
  (catch application:stop(fuse)),
  ok = application:start(fuse).

%%% INTERNALS
%%% ---------------------

%% installed_names/1 Picks amongst the installed names
installed_names(#state { installed = Is }) ->
	[N || {N, _} <- Is].

%% is_installed/2 determines if a given fuse is installed
is_installed(N, #state { installed = Is }) -> lists:keymember(N, 1, Is).


%% valid_opts/1 determines if the given options are valid
valid_opts({{standard, K, R}, {reset, T}})
    when K > 0, R >= 0, T >= 0 ->
	true;
valid_opts(_) ->
	false.
	
melt_state(Name, S) ->
	count_state(fuse_intensity(Name, S) - count_melts(Name, S)).

is_blown(Name, #state { reset_points = RPs }) ->
	lists:keymember(Name, 2, RPs).
	
fuse_intensity(Name, #state { installed = Inst }) ->
	{Name, Count} = lists:keyfind(Name, 1, Inst),
	Count.

count_state(N) when N < 0 -> blown;
count_state(_N) -> ok.

count_melts(Name, #state { melts = Ms }) ->
	length([N || {N, _} <- Ms, N == Name]).

has_fuses_installed(#state { installed = [] }) -> false;
has_fuses_installed(#state { installed = [_|_]}) -> true.

resets_ok(#state { reset_points = [] }) -> true;
resets_ok(#state { reset_points = [{Ts, _}|_], time = T }) ->
	T =< Ts.

record_melt(Name, Ts, #state { melts = Ms } = S) ->
	S#state { melts = [{Name, Ts} | Ms] }.

record_melt_history(Name, #state { time = Ts, reset_points = OldRPs } = S) ->
	case melt_state(Name, S) of
	    ok -> S;
	    blown ->
	        case is_reset_point(Name, S) of
	            true -> S; %% Can have at most 1 RP for a name
	            false ->
	            	RP = time_add(Ts, {0, ?PERIOD, 0}),
	        		S#state { reset_points =reset_store(RP, Name, OldRPs) }
	        	end
	end.

reset_store(RP, Name, []) -> [{RP, Name}];
reset_store(RP, Name, [{P, N} | Ps]) when RP =< P ->
	[{P, N} | reset_store(RP, Name, Ps)];
reset_store(RP, Name, [{P, N} | Ps]) when RP > P ->
	[{RP, Name}, {P, N} | Ps].

clear_resets(Name, #state { reset_points = Rs } = S) ->
	S#state { reset_points = [{T, N} || {T, N} <- Rs, N /= Name] }.
	
clear_melts(Name, #state { melts = Ms } = S) ->
	S#state { melts = [{N, Ts} || {N, Ts} <- Ms, N /= Name] }.

expire_melts(Period, #state { time = Now, melts = Ms } = S) ->
	S#state { melts = [{Name, Ts} || {Name, Ts} <- Ms, in_period(Ts, Now, Period)] }.

%% Alternative implementation of being inside the period, based on microsecond conversion.
in_period(Ts, Now, _) when Now < Ts -> false;
in_period(Ts, Now, Period) when Now >= Ts ->
	STs = micros(Ts) div (1000 * 1000),
	SNow = micros(Now) div (1000 * 1000),
	
	%% Difference in Seconds, by subtraction and then eradication of the microsecond parts.
	Secs = SNow - STs,
	Secs =< Period.

is_reset_point(Name, #state { reset_points = RPs }) ->
	lists:keymember(Name, 2, RPs).

%% PULSE instrumentation,
the_prop() -> x_prop_model_pulse().

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
