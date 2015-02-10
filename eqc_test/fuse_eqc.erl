%%% The fuse_eqc module implements a Quickcheck model for the Fuse main gen_server.
-module(fuse_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("pulse/include/pulse.hrl").
-include_lib("pulse_otp/include/pulse_otp.hrl").

-compile(export_all).

%%% Model state.
-record(state, {
          time = {0, 1000*1000 - 1, 0},  % Current time in the model. We track time to handle melting time points.
          melts = [], % History of current melts issued to the SUT
          blown = [], % List of currently blown fuses
          installed = [], % List of installed fuses, with their configuration.
          reqs = [] %% Record the requirements we test
}).

-define(CONTEXT, sync).

%% API Generators

%% fuses/0 is the list of fuses we support in the model for testing purposes.
fuses() -> [phineas, ferb, candace, isabella, vanessa, perry, heinz].

%% g_atom/0 generates a simple atom from a short list.
g_atom() ->
	oneof([a,b,c,d,e,f]).

%% g_name/0 generates one of the fuses at random.
%% fault injects provably invalid names
g_name() ->
	  fault(g_atom(), elements(fuses())).

%% Thomas says this is a bad idea, since we can rule out the name by a precondition (_pre/3)
%% As a result we stopped using functions like these.
%% g_installed(S) ->
%%	fault(g_name(), oneof(installed_names(S))).

%% g_neg_int/0 Generates a negative integer, or 0
g_neg_int() ->
	?LET(N, nat(),
		-N).

%% g_strategy/0 generates a random fuse configuration.
%% At times, this will generate a faulty strategy to make sure we correctly
%% reject incorrect strategies.
g_strategy() ->
	fault(
		{frequency([
			{1, {g_atom(), int(), int()}},
			{1, {standard, g_neg_int(), int()}},
			{1, {standard, int(), g_neg_int()}},
			{1, {standard, int(), int()}}
		])},
		{standard, choose(1, 2), choose(1, 3)}
	).

%% g_refresh()/0 generates a refresh setting.
g_refresh() ->
	{reset, 60000}.
	
%% g_options() generates install options
g_options() ->
	{g_strategy(), g_refresh()}.


%% g_time_inc/0 generates a time increment.
g_time_inc() ->
	choose(1, 4000*1000).

%% initial_state/0 generates the initial system state
initial_state() -> #state{}.

%% elapse_time
%% ---------------------------------------------------------------
%% Let time pass in the model. This increases time by an amount so calls will happen
%% at a later point than normally.
elapse_time(N) ->
	fuse_time:elapse_time(N).
	
elapse_time_args(_S) ->
	[g_time_inc()].

elapse_time_next(#state { time = T } = State, _V, [N]) ->
	State#state { time = fuse_time:inc(T, N) }.

elapse_time_post(#state { time = T }, [N], NewTime) ->
	eq(fuse_time:inc(T, N), NewTime).

%% fuse_reset/2 sends timer messages into the SUT
%% ---------------------------------------------------------------
%% Heal a fuse which has been blown in the system.
fuse_reset(Name) ->
    fuse_server ! {reset, Name},
    fuse_server:sync(), %% synchronize to avoid a race condition.
    ok.

%% You can reset a fuse if there is a blown fuse in the system.
fuse_reset_pre(#state { blown = [] }) -> false;
fuse_reset_pre(#state {}) -> true.

fuse_reset_args(#state { blown = Names }) ->
	[elements(Names)].

%% Fuses will only be reset if their state is among the installed and are blown.
%% Precondition checking is effective at shrinking down failing models.
fuse_reset_pre(#state { blown = Blown } = S, [Name]) ->
	is_installed(Name, S) andalso lists:member(Name, Blown).

%% Note: when a fuse heals, the internal state is reset.
fuse_reset_next(#state { blown = RPs } = S, _V, [Name]) ->
    case lists:member(Name, RPs) of
        false -> req("R01 Heal non-installed fuse", S);
        true ->
            req(lists:concat(["R02 Heal an installed fuse (blown ", is_blown(Name, S),")"]),
                clear_melts(Name, S#state { blown = lists:delete(Name, RPs) }))
    end.

fuse_reset_post(_S, [_Name], R) ->
	eq(R, ok).

%% install/2 puts a new fuse into the system
%% ---------------------------------------------------------------
install(Name, Opts) ->
	try fuse:install(Name, Opts) of
		ok -> ok
	catch
		error:badarg ->
			badarg
	end.

install_args(_S) ->
	[g_name(), g_options()].

%% When installing new fuses, the internal state is reset for the fuse.
%% Also, consider if the installed is valid at all.
install_next(#state{ installed = Is } = S, _V, [Name, Opts]) ->
	case valid_opts(Opts) of
	    false ->
	        req("R03 Installing an invalid fuse", S);
	    true ->
	        {{standard, Count, Period}, _} = Opts,
	        T = {Name, Count, Period},
                req(lists:concat(["R04 Installing fuse ", Count, " ", Period, " new:", is_installed(Name, S)]),
                    clear_melts(Name,
                      clear_blown(Name,
                         S#state { installed = lists:keystore(Name, 1, Is, T) })))
	end.

install_post(_S, [_Name, Opts], R) ->
	case valid_opts(Opts) of
	    true -> eq(R, ok);
	    false -> eq(R, badarg)
	end.

%% reset/1 resets a fuse back to its initial state
%% ---------------------------------------------------------------
reset(Name) ->
	fuse:reset(Name).

reset_pre(S) ->
	has_fuses_installed(S).

reset_args(_S) ->
	[g_name()].

reset_post(S, [Name], Ret) ->
    case is_installed(Name, S) of
        true -> eq(Ret, ok);
        false -> eq(Ret, {error, not_found})
    end.

%% Resetting a fuse resets its internal state
reset_next(S, _V, [Name]) ->
    case is_installed(Name, S) of
        false -> req("R05 Reset non-installed fuse", S);
        true ->
            req(lists:concat(["R06 Reset an installed fuse (blown ", is_blown(Name, S), ")"]),
        		clear_blown(Name,
        		  clear_melts(Name,
        		    S)))
    end.

%%% ask/1 asks about the state of a fuse that exists
%% ---------------------------------------------------------------
%% Split into two variants
ask_installed(Name) ->
	fuse:ask(Name, ?CONTEXT).
	
ask_installed_pre(S) -> has_fuses_installed(S).

ask_installed_args(_S) -> [g_name()].

ask_installed_pre(S, [Name]) -> is_installed(Name, S).

ask_installed_next(S, _V, [_Name]) -> req("R15 Ask installed fuse", S).

ask_installed_post(S, [Name], Ret) ->
    V = case is_blown(Name, S) of
    	true -> blown;
    	false -> ok
    end,
    eq(Ret, V).

ask(Name) ->
	fuse:ask(Name, ?CONTEXT).
	
ask_pre(S) ->
	has_fuses_installed(S).

ask_args(_S) ->
	[g_name()].
	
ask_next(S, _V, [Name]) ->
	case is_installed(Name, S) of
	    true -> req("R15 Ask installed fuse", S);
	    false -> req("R16 Ask uninstalled fuse", S)
	end.

ask_post(S, [Name], Ret) ->
	case is_installed(Name, S) of
	    true ->
	    	eq(Ret, case is_blown(Name, S) of true -> blown; false -> ok end);
	    false ->
	        eq(Ret, {error, not_found})
	end.

%%% run/1 runs a function (thunk) on the circuit breaker
%% ---------------------------------------------------------------
run(Name, _Result, _Return, Fun) ->
	fuse:run(Name, Fun, ?CONTEXT).
	
run_pre(S) ->
	has_fuses_installed(S).

run_args(_S) ->
    ?LET({N, Result, Return}, {g_name(), elements([ok, melt]), int()},
        [N, Result, Return, function0({Result, Return})] ).

run_next(S, _V, [_Name, ok, _, _]) ->
    req("R07 Run on ok fuse", S);
run_next(#state { time = Ts } = S, _V, [Name, melt, _, _]) ->
	case is_installed(Name, S) of
            true ->
                case is_blown(Name, S) of
                    true ->
                        req("R08 Run (melt) on blown fuse", S);
                    false ->
                        req("R09 Run (melt) on installed fuse ",
                            record_melt_history(Name,
                              expire_melts(period(Name, S), Name,
                                record_melt(Name, Ts, S))))
                end;
            false ->
                req("R10 Run uninstalled fuse ", S)
	end.

run_post(S, [Name, _Result, Return, _], Ret) ->
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
%% ---------------------------------------------------------------
%% There are two ways of melting. One way is guaranteed to pick an already installed
%% fuse while the other picks fuses arbitrarily. This enables to to easily prioritise the
%% fuses which are installed, since we assume the interesting aspects affects these.
melt_installed(Name) ->
	fuse:melt(Name).
	
melt_installed_pre(S) -> has_fuses_installed(S).

melt_installed_args(_S) -> [g_name()].

melt_installed_pre(S, [Name]) ->
	is_installed(Name, S).
	
melt_installed_next(#state { time = Ts } = S, _V, [Name]) ->
	req("R11 Melt installed fuse ",
	    record_melt_history(Name,
	        expire_melts(period(Name, S), Name,
	            record_melt(Name, Ts, S)))).

melt_installed_post(_S, _, Ret) ->
	eq(Ret, ok).

melt(Name) ->
	fuse:melt(Name).

melt_pre(S) ->
    has_fuses_installed(S).

melt_args(_S) ->
	[g_name()].

melt_next(#state { time = Ts } = S, _V, [Name]) ->
	case is_installed(Name, S) of
            true ->
                req("R11 Melt installed fuse ",
		    record_melt_history(Name,
		      expire_melts(period(Name, S), Name,
		        record_melt(Name, Ts,
		          S))));
            false ->
                req("R12 Melt uninstalled fuse", S)
	end.

melt_post(_S, _, Ret) ->
	eq(Ret, ok).

%%% Command weight distribution
%% ---------------------------------------------------------------
weight(_, elapse_time) -> 5;
weight(_, install) -> 1;
weight(_, reset) -> 2;
weight(_, run) -> 5;
weight(_, melt) -> 1;
weight(_, melt_installed) -> 40;
weight(_, fuse_reset) -> 100;
weight(_, ask) -> 1;
weight(_, ask_installed) -> 30.

%%% STATISTICS
%% ---------------------------------------------------------------
req(Req,S) ->
  S#state{reqs = lists:usort([Req|S#state.reqs])}.

%%% INVARIANT
%% ---------------------------------------------------------------

%% Check that the system is actually returning the values we want
%% invariant(#state { melts = Melts }) ->
%%     SUTMelts = [{N, length(L)} || {N, L} <- fuse_server:q_melts(), length(L) > 0],
%%     ModelMelts = compute_melt_times(Melts),
%%     eq(
%%     	lists:sort(SUTMelts),
%%     	lists:sort(ModelMelts)).

%% compute_melt_times(Ms) ->
%%     Grouped = group(lists:sort(Ms)),
%%     [{N, length(Melts)} || {N, Melts} <- Grouped].

%% group([])            -> [];
%% group([{E, K} | Es]) -> group(E, [K], Es).

%% group(E, Acc, []) -> [{E, Acc}];
%% group(E, Acc, [{E, K} | Next]) -> group(E, [K | Acc], Next);
%% group(E, Acc, [{X, K} | Next]) -> [{E, Acc} | group(X, [K], Next)].

%%% PROPERTIES
%% ---------------------------------------------------------------

%% Test the stateful system against a random sequential command sequence.
prop_model_seq() ->
    ?SETUP( fun() ->
                    setup(),
                    fun() -> ok end
            end,
    fault_rate(1, 40,
	?FORALL(Cmds, more_commands(2, commands(?MODULE)),
	  begin
              fuse_time:start({0, 1000*1000 - 1, 0}),
              cleanup(),
              {H, S, R} = run_commands(?MODULE, Cmds),
              aggregate(command_names(Cmds),
                aggregate(S#state.reqs,
                   pretty_commands(?MODULE, Cmds, {H, S, R}, R == ok)))
	  end))).

%% Test the stateful system against a random parallel command sequence with a sequential prefix.
prop_model_par() ->
    ?SETUP( fun() ->
                   setup(),
                   fun() -> ok end
           end,
    fault_rate(1, 40,
     ?LET(Shrinking, parameter(shrinking, false), 
	?FORALL(Cmds, more_commands(2, parallel_commands(?MODULE)),
	  ?ALWAYS(if not Shrinking -> 1;
                     Shrinking -> 20
		  end,
	  begin
              fuse_time:start({0, 1000*1000 - 1, 0}),
              cleanup(),
              {H, S, R} = run_parallel_commands(?MODULE, Cmds),
              aggregate(command_names(Cmds),
                  pretty_commands(?MODULE, Cmds, {H, S, R}, R == ok))
	  end))))).

%% Run a test under PULSE to randomize the process schedule as well.
x_prop_model_pulse() ->
   ?SETUP(fun() ->
                   setup(),
                   fun() -> ok end
           end,
  ?LET(Shrinking, parameter(shrinking, false),
  ?FORALL(Cmds, more_commands(2, parallel_commands(?MODULE)),
    ?ALWAYS(if not Shrinking -> 1; Shrinking -> 20 end,
      ?PULSE(HSR={_, _, R},
        begin
          fuse_time:start({0, 1000*1000 - 1, 0}),
          cleanup(),
          run_parallel_commands(?MODULE, Cmds)
        end,
        aggregate(command_names(Cmds),
        pretty_commands(?MODULE, Cmds, HSR, R == ok))))))).

-ifdef(WITH_PULSE).
setup() ->
  error_logger:tty(false),
  ok.
-else.
setup() ->
  error_logger:tty(false),
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  application:set_env(sasl, errlog_type, error),
  application:start(sasl).
-endif.

cleanup() ->
    (catch application:stop(fuse)),
    ok = application:start(fuse).

%%% Helpers
%%% ---------------------

sample() ->
	eqc_gen:sample(commands(?MODULE)).

%%% INTERNALS
%%% ---------------------

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

is_blown(Name, #state { blown = BlownFuses }) ->
	lists:member(Name, BlownFuses).
	
fuse_intensity(Name, #state { installed = Inst }) ->
	{Name, Count, _} = lists:keyfind(Name, 1, Inst),
	Count.

count_state(N) when N < 0 -> blown;
count_state(_N) -> ok.

count_melts(Name, #state { melts = Ms }) ->
	length([N || {N, _} <- Ms, N == Name]).

period(Name, #state { installed = Is }) ->
	{_, _, Period} = lists:keyfind(Name, 1, Is),
	Period.

has_fuses_installed(#state { installed = [] }) -> false;
has_fuses_installed(#state { installed = [_|_]}) -> true.

record_melt(Name, Ts, #state { melts = Ms } = S) ->
	S#state { melts = [{Name, Ts} | Ms] }.

record_melt_history(Name, #state { blown = OldRPs } = S) ->
	case melt_state(Name, S) of
	    ok -> S;
	    blown ->
	        case lists:member(Name, OldRPs) of
	            true -> S; %% Can have at most 1 RP for a name
	            false ->
	            	req("R13 Blowing fuse",
	            	  S#state { blown = OldRPs ++ [Name] })
	        	end
	end.

clear_blown(Name, #state { blown = Rs } = S) ->
	S#state { blown = [N || N <- Rs, N /= Name] }.
	
clear_melts(Name, #state { melts = Ms } = S) ->
	S#state { melts = [{N, Ts} || {N, Ts} <- Ms, N /= Name] }.

expire_melts(Period, Who, #state { time = Now, melts = Ms } = S) ->
    Updated =
        [{Name, Ts} || {Name, Ts} <- Ms, Name /= Who orelse in_period(Ts, Now, Period)],
    NewState = S#state { melts = Updated },
    case Ms /= Updated of
        true -> req("R14 Expiring melts", NewState);
        false -> NewState
    end.

%% Alternative implementation of being inside the period, based on microsecond conversion.
in_period(Ts, Now, _) when Now < Ts -> false;
in_period(Ts, Now, Period) when Now >= Ts ->
    STs = model_time:micros(Ts) div (1000 * 1000),
    SNow = model_time:micros(Now) div (1000 * 1000),
	
    %% Difference in Seconds, by subtraction
    case SNow - STs of
        T when T > Period ->
            false;
        _ ->
            true
    end.

t(seq, {T, Unit}) ->
    eqc:testing_time(eval_time(T, Unit), prop_model_seq());
t(seq, N) when is_integer(N) ->
    eqc:numtests(N, prop_model_seq());
t(par, {T, Unit}) ->
    eqc:testing_time(eval_time(T, Unit), prop_model_par());
t(par, N) when is_integer(N) ->
    eqc:numtests(N, prop_model_par());
t(pulse, {T, Unit}) ->
    eqc:testing_time(eval_time(T, Unit), x_prop_model_pulse());
t(pulse, N) when is_integer(N) ->
    eqc:numtests(N, x_prop_model_pulse()).
    

eval_time(N, h)   -> eval_time(N*60, min);
eval_time(N, min) -> eval_time(N*60, sec);
eval_time(N, sec) -> N.

r(What, T) ->
    eqc:quickcheck(t(What, T)).

rv(What, T) ->
    eqc:quickcheck(eqc_statem:show_states(t(What, T))).

pulse_instrument() ->
  [ pulse_instrument(File) || File <- filelib:wildcard("../src/*.erl") ++ filelib:wildcard("../eqc_test/*.erl") ],
  load_sasl().
  
load_sasl() ->
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  application:set_env(sasl, errlog_type, error),
  application:start(sasl),
  ok.

pulse_instrument(File) ->
    EffectFul = [
    	{ets, '_', '_'},
    	{alarm_handler, '_', '_'}],
    io:format("Compiling: ~p~n", [File]),
    {ok, Mod} = compile:file(File, [{d, 'PULSE', true}, {d, 'WITH_PULSE', true},
                                    {d, 'EQC_TESTING', true},
                                    {parse_transform, pulse_instrument},
                                    {pulse_side_effect, EffectFul}]),
  code:purge(Mod),
  code:load_file(Mod),
  Mod.
