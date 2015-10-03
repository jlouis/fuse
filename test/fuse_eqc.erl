%%% The fuse_eqc module implements a Quickcheck model for the Fuse main gen_server.
-module(fuse_eqc).
-compile(export_all).

-ifdef(EQC_TESTING).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("pulse/include/pulse.hrl").
-include_lib("pulse_otp/include/pulse_otp.hrl").

%%% Model state.
-record(state, {
          time = -10000,  % Current time in the model. We track time to handle melting time points.
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
    ?LET(N, nat(), -N).

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
    fuse_time_mock:elapse_time(N).

elapse_time_args(_S) -> [g_time_inc()].

elapse_time_next(#state { time = T } = State, _V, [N]) ->
    State#state { time = T + N }.

elapse_time_return(#state { time = T }, [N]) -> T+N.

elapse_time_features(_S, _A, _R) -> [{fuse_eqc, r00, elapse_time}].

%% fuse_reset/2 sends timer messages into the SUT
%% ---------------------------------------------------------------
%% Heal a fuse which has been blown in the system.
fuse_reset(Name) ->
    fuse_server ! {reset, Name},
    fuse_server:sync(), %% synchronize to avoid a race condition.
    ok.

%% You can reset a fuse if there is a blown fuse in the system.
fuse_reset_pre(#state { blown = Blown }) -> Blown /= [].

fuse_reset_args(#state { blown = Names }) ->
    [elements(Names)].

%% Fuses will only be reset if their state is among the installed and are blown.
%% Precondition checking is effective at shrinking down failing models.
fuse_reset_pre(#state { blown = Blown } = S, [Name]) ->
    is_installed(Name, S) andalso lists:member(Name, Blown).

%% Note: when a fuse heals, the internal state is reset.
fuse_reset_next(#state { blown = RPs } = S, _V, [Name]) ->
    case lists:member(Name, RPs) of
        false -> S;
        true -> clear_melts(Name, S#state { blown = lists:delete(Name, RPs) })
    end.

fuse_reset_features(#state { blown = RPs } = S, [Name], _Response) ->
    case lists:member(Name, RPs) of
        false -> [{fuse_eqc, r01, heal_non_installed}];
        true -> [{fuse_eqc, r02, {heal_installed_fuse, is_blown(Name, S)}}]
    end.

fuse_reset_return(_S, [_Name]) -> ok.

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
        false -> S;
        true ->
            {{standard, Count, Period}, _} = Opts,
            T = {Name, Count, Period},
            clear_melts(Name,
                clear_blown(Name,
                    S#state { installed = lists:keystore(Name, 1, Is, T) }))
    end.

install_features(S, [Name, Opts], _R) ->
    case valid_opts(Opts) of
        false -> [{fuse_eqc, r03, installing_invalid_fuse}];
        true ->
            {{standard, Count, Period}, _} = Opts,
            [{fuse_eqc, r03, {installing_fuse, Count, Period, {new, is_installed(Name, S)}}}]
    end.

install_return(_S, [_Name, Opts]) ->
    case valid_opts(Opts) of
        true -> ok;
        false -> badarg
    end.

%% reset/1 resets a fuse back to its initial state
%% ---------------------------------------------------------------
reset(Name) ->
    fuse:reset(Name).

reset_pre(S) ->
    has_fuses_installed(S).

reset_args(_S) ->
    [g_name()].

reset_return(S, [Name]) ->
    case is_installed(Name, S) of
        true -> ok;
        false -> {error, not_found}
    end.

%% Resetting a fuse resets its internal state
reset_next(S, _V, [Name]) ->
    case is_installed(Name, S) of
        false -> S;
        true ->
          clear_blown(Name,
            clear_melts(Name,
              S))
     end.

reset_features(S, [Name], _V) ->
    case is_installed(Name, S) of
        false -> [{fuse_eqc, r05, reset_uninstalled_fuse}];
        true -> [{fuse_eqc, r06, reset_installed, {blown, is_blown(Name, S)}}]
    end.

%%% ask/1 asks about the state of a fuse that exists
%% ---------------------------------------------------------------
%% Split into two variants
ask_installed(Name) ->
    fuse:ask(Name, ?CONTEXT).

ask_installed_pre(S) -> has_fuses_installed(S).

ask_installed_args(_S) -> [g_name()].

ask_installed_pre(S, [Name]) -> is_installed(Name, S).

ask_installed_features(_S, [_Name], _R) -> [{fuse_eqc, r15, ask_installed}].

ask_installed_return(S, [Name]) ->
    case is_blown(Name, S) of
    	true -> blown;
    	false -> ok
    end.

ask(Name) ->
    fuse:ask(Name, ?CONTEXT).

ask_pre(S) -> has_fuses_installed(S).

ask_args(_S) -> [g_name()].

ask_features(S, [Name], _V) ->
    case is_installed(Name, S) of
       true -> [{fuse_eqc, r15, ask_installed}];
       false -> [{fuse_eqc, r16, ask_uninstalled}]
    end.

ask_return(S, [Name]) ->
    case is_installed(Name, S) of
        true ->
            case is_blown(Name, S) of
                true -> blown;
                false -> ok
            end;
        false ->
            {error, not_found}
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

run_next(S, _V, [_Name, ok, _, _]) -> S;
run_next(#state { time = Ts } = S, _V, [Name, melt, _, _]) ->
    case is_installed(Name, S) of
            true ->
                case is_blown(Name, S) of
                    true -> S;
                    false ->
                        M = val(record_melt(Name, Ts, S)),
                        {_, NewState} =
                          bind(M, fun(S2) ->
                          bind(expire_melts(period(Name, S2), Name, S2), fun(S3) ->
                            record_melt_history(Name, S3) end) end),
                        NewState
                end;
            false -> S
    end.

run_features(_S, [_Name, ok, _, _], _R) -> [{fuse_eqc, r07, run_ok_fuse}];
run_features(#state { time = Ts } = S, [Name, melt, _, _], _R) ->
  case is_installed(Name, S) of
    true ->
      case is_blown(Name, S) of
        true -> [{fuse_eqc, r08, run_melt_on_blown_fuse}];
        false ->
           M = val(record_melt(Name, Ts, S)),
           {Features, _} =
             bind(M, fun(S2) ->
             bind(expire_melts(period(Name, S2), Name, S2), fun(S3) ->
               record_melt_history(Name, S3) end) end),
           Features ++ [{fuse_eqc, r09, run_melt_on_installed_fuse}]
      end;
    false ->
      [{fuse_eqc, r10, run_on_uninstalled_fuse}]
  end.

run_return(S, [Name, _Result, Return, _]) ->
    case is_installed(Name, S) of
        true ->
    	case is_blown(Name, S) of
    	    false -> {ok, Return};
    	    true -> blown
    	end;
        false ->
            {error, not_found}
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
    M = val(record_melt(Name, Ts, S)),
    {_, NewState} =
      bind(M, fun(S2) ->
      bind(expire_melts(period(Name, S2), Name, S2), fun(S3) ->
        record_melt_history(Name, S3) end) end),
    NewState.

melt_installed_features(#state { time = Ts } = S, [Name], _V) ->
    M = val(record_melt(Name, Ts, S)),
    {Features, _} =
      bind(M, fun(S2) ->
      bind(expire_melts(period(Name, S2), Name, S2), fun(S3) ->
        record_melt_history(Name, S3) end) end),
    [{fuse_eqc, r11, melt_installed_fuse}] ++ Features.

melt_installed_return(_S, _) -> ok.

melt(Name) ->
    fuse:melt(Name).

melt_pre(S) ->
    has_fuses_installed(S).

melt_args(_S) ->
    [g_name()].

melt_next(#state { time = Ts } = S, _V, [Name]) ->
    case is_installed(Name, S) of
            true ->
              M = val(record_melt(Name, Ts, S)),
              {_, NewState} =
                bind(M, fun(S2) ->
                bind(expire_melts(period(Name, S2), Name, S2), fun(S3) ->
                  record_melt_history(Name, S3) end) end),
              NewState;
            false -> S
    end.

melt_features(#state { time = Ts } = S, [Name], _V) ->
    case is_installed(Name, S) of
        true ->
              M = val(record_melt(Name, Ts, S)),
              {Features, _} =
                bind(M, fun(S2) ->
                bind(expire_melts(period(Name, S2), Name, S2), fun(S3) ->
                  record_melt_history(Name, S3) end) end),
              [{fuse_eqc, r11, melt_installed_fuse}] ++ Features;
        false -> [{fuse_eqc, r12, melt_uninstalled_fuse}]
    end.

melt_return(_S, _) -> ok.

%% remove/1 removes a fuse
%% ---------------------------------------------------------------
remove(Name) ->
    fuse:remove(Name).

%% Generate arguments to remove from a fuse that's actually installed vs
%% fuses that are not.
remove_args(#state { installed = Is } = _S) ->
    frequency(
      [ {20, ?LET(F, elements(Is), [element(1, F)])} || Is /= [] ] ++
      [ {1, ?SUCHTHAT([F], [g_name()], lists:keymember(F, 1, Is) == false)} ]).

remove_return(S, [Name]) ->
    case is_installed(Name, S) of
        true -> ok;
        false -> {error, not_found}
    end.

%% Removing a fuse, removes it from the list of installed fuses.
remove_next(#state{ installed = Is } = S, _V, [Name]) ->
    case is_installed(Name, S) of
        false -> S;
        true ->
            S#state { installed = lists:keydelete(Name, 1, Is) }
     end.

remove_features(S, [Name], _V) ->
    case is_installed(Name, S) of
        false -> [{fuse_eqc, r17, remove_uninstalled_fuse}];
        true -> [{fuse_eqc, r18, remove_installed_fuse}]
    end.

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
weight(_, ask_installed) -> 30;
weight(_, remove) -> 1.

%%% PROPERTIES
%% ---------------------------------------------------------------

postcondition_common(S, Call, Res) ->
    eq(Res, return_value(S, Call)).

%% Test the stateful system against a random sequential command sequence.
prop_model_seq() ->
    ?SETUP( fun() ->
                    setup(),
                    fun() -> ok end
            end,
    fault_rate(1, 40,
    ?FORALL(Cmds, more_commands(2, commands(?MODULE)),
      begin
        fuse_time_mock:start(-10000),
        cleanup(),
        {H, S, R} = run_commands(?MODULE, Cmds),
        pretty_commands(?MODULE, Cmds, {H,S,R},
            aggregate(with_title('Commands'), command_names(Cmds),
            aggregate(with_title('Features'), eqc_statem:call_features(H),
            features(eqc_statem:call_features(H),
                R == ok))))
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
              fuse_time_mock:start(-10000),
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
          fuse_time_mock:start(-10000),
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
    {ok, _Apps} = application:ensure_all_started(fuse).

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
        ok -> {[], S};
        blown ->
            case lists:member(Name, OldRPs) of
                true -> {[], S}; %% Can have at most 1 RP for a name
                false ->
                    {[{fuse_eqc, r13, blowing_fuse}], S#state { blown = OldRPs ++ [Name] }}
            end
    end.

expire_melts(Period, Who, #state { time = Now, melts = Ms } = S) ->
    Updated = [{Name, Ts} || {Name, Ts} <- Ms, Name /= Who orelse in_period(Ts, Now, Period)],
    NewState = S#state { melts = Updated },
    case Ms /= Updated of
        true -> {[{fuse_eqc, r14, expiring_melts}], NewState};
        false -> {[], NewState}
    end.

clear_blown(Name, #state { blown = Rs } = S) ->
    S#state { blown = [N || N <- Rs, N /= Name] }.

clear_melts(Name, #state { melts = Ms } = S) ->
    S#state { melts = [{N, Ts} || {N, Ts} <- Ms, N /= Name] }.


%% Alternative implementation of being inside the period, based on microsecond conversion.
in_period(Ts, Now, _) when Now < Ts -> false;
in_period(Ts, Now, Period) when Now >= Ts -> (Now - Ts) < Period.

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

%% A little monad action:
bind(M, F) ->
    {Write, S} = M,
    {Write2, S2} = F(S),
    {Write ++ Write2, S2}.

val(V) -> {[], V}.

-endif.
