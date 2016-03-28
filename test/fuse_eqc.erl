%%% The fuse_eqc module implements a Quickcheck model for the Fuse main gen_server.
-module(fuse_eqc).
-compile(export_all).

-ifdef(EQC_TESTING).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

%%% Model state.
-record(state, {
          melts = [], % History of current melts issued to the SUT
          blown = [], % List of currently blown fuses
          disabled = [], % List of fuses which are currently manually disabled
          installed = [], % List of installed fuses, with their configuration.
          reqs = [] %% Record the requirements we test
}).

-define(CONTEXT, sync).

%% -- MOCKING ------------------------------------------------

api_spec() ->
    #api_spec {
        language = erlang,
        modules = [
            #api_module {
                name = fuse_rand,
                functions = [#api_fun { name = uniform, arity = 0 } ]
    }]}.

%% API Generators

-define(Q, 10000000000000000000).
-define(EPSILON, 0.000001).

g_uniform_real() ->
    ?LET(K, choose(1,?Q-1), K / ?Q).

g_split_float(Pivot) ->
   frequency([
       {10, g_uniform_real() },
       {10, Pivot - ?EPSILON },
       {10, Pivot + ?EPSILON }
   ]).


%% fuses/0 is the list of fuses we support in the model for testing purposes.
fuses() -> [phineas, ferb, candace, isabella, vanessa, perry, heinz].

%% g_atom/0 generates a simple atom from a short list.
g_atom() ->
    oneof([a,b,c,d,e,f]).

%% g_name/0 generates one of the fuses at random.
%% fault injects provably invalid names
g_name() ->
      fault(g_atom(), elements(fuses())).

g_disabled_name(S) ->
    elements(S#state.disabled).

%% Thomas says this is a bad idea, since we can rule out the name by a precondition (_pre/3)
%% As a result we stopped using functions like these.
%% g_installed(S) ->
%%    fault(g_name(), oneof(installed_names(S))).

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
            {1, {standard, int(), int()}},
            {1, {fault_injection, oneof([real(), int(), g_atom()]), int(), int()}}
        ])},
        oneof([
            {standard, choose(1, 2), choose(1, 3)},
            {fault_injection, g_uniform_real(), choose(1,2), choose(1,3)}
        ])
    ).

g_cmd() ->
    oneof(
      [{delay, ?LET(N, nat(), N+1)},
       {barrier, g_atom()},
       {gradual, g_uniform_real()},
       heal]).

%% g_refresh()/0 generates a refresh setting.
g_refresh() ->
    oneof([{reset, choose(1, 60000)}]).

%% g_options() generates install options
g_options() ->
    {g_strategy(), g_refresh()}.


%% g_time_inc/0 generates a time increment.
g_time_inc() ->
    choose(1, 4000*1000).

%% initial_state/0 generates the initial system state
initial_state() -> #state{}.

%% fuse_reset/2 sends timer messages into the SUT
%% ---------------------------------------------------------------
%% Heal a fuse which has been blown in the system.
fuse_reset(Name, _TRef) ->
    fuse_server ! {reset, Name},
    fuse_server:sync(), %% synchronize to avoid a race condition.
    ok.

%% You can reset a fuse if there is a blown fuse in the system.
fuse_reset_pre(#state { blown = Blown }) -> Blown /= [].

fuse_reset_args(#state { blown = Blown }) ->
    ?LET({N, T}, elements(Blown), [N, T]).

%% Fuses will only be reset if their state is among the installed and are blown.
%% Precondition checking is effective at shrinking down failing models.
fuse_reset_pre(S, [Name, _]) ->
    is_installed(S, Name) andalso is_blown(S, Name).

%% Note: when a fuse heals, the internal state is reset.
fuse_reset_callouts(S, [Name, TRef]) ->
    ?APPLY(fuse_time_eqc, trigger, [TRef]),
    case is_blown(S, Name) of
        false -> ?EMPTY;
        true ->
            ?APPLY(clear_blown, [Name]),
            ?APPLY(clear_melts, [Name])
    end,
    ?RET(ok).

fuse_reset_features(S, [Name, _], _Response) ->
    case is_blown(S, Name) of
        false -> [{fuse_eqc, r01, heal_non_installed}];
        true -> [{fuse_eqc, r02, {heal_installed_fuse, is_blown(S, Name)}}]
    end.

fuse_reset_return(_S, [_Name, _TRef]) -> ok.

%% -- INSTALLATION ------------------------------------------------------

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
install_callouts(_S, [Name, Opts]) ->
    #{ period := P } = parse_opts(Opts),
    ?APPLY(fuse_time_eqc, convert_time_unit, [P, milli_seconds, native]),
    case valid_opts(Opts) of
        false -> ?RET(badarg);
        true ->
            T = {Name, parse_opts(Opts)},
            ?APPLY(install_fuse, [Name, T]),
            ?APPLY(clear_blown, [Name]),
            ?APPLY(clear_melts, [Name]),
            ?RET(ok)
    end.

%% Internal helper
install_fuse_next(#state { installed = Is } = S, _, [Name, T]) ->
    S#state { installed = lists:keystore(Name, 1, Is, T) }.

install_features(S, [Name, Opts], _R) ->
    case valid_opts(Opts) of
        false -> [{fuse_eqc, r03, installing_invalid_fuse}];
        true ->
            case Opts of
                {{standard, Count, Period}, _} ->
                    [{fuse_eqc, r03, {installing_fuse, Count, Period, {new, is_installed(S, Name)}}}];
                {{fault_injection, _, Count, Period}, _} ->
                    [{fuse_eqc, r03, {installing_fuse, Count, Period, {new, is_installed(S, Name)}}}]
            end
    end.

%% -- DISABLING AND ENABLING CIRCUITS ----------------------------------

%% circuit_disable/1 disables a fuse manually
%%
circuit_disable(Name) ->
    fuse:circuit_disable(Name).

circuit_disable_pre(S) ->
    has_fuses_installed(S).

circuit_disable_args(_S) ->
    [g_name()].

circuit_disable_callouts(S, [Name]) ->
    case is_installed(S, Name) of
        false -> ?RET({error, not_found});
        true ->
            case is_disabled(S, Name) of
                false ->
                    ?APPLY(add_disabled, [Name]),
                    ?APPLY(clear_melts, [Name]),
                    ?APPLY(clear_blown, [Name]),
                    ?RET(ok);
                true ->
                    ?RET(ok)
            end
    end.

circuit_disable_features(S, [Name], _V) ->
    case is_installed(S, Name) of
        false -> [{fuse_eqc, r17, disable_uninstalled_fuse}];
        true -> [{fuse_eqc, r18, disable_installed, {blown, is_blown(S, Name)}}]
    end.

%% circuit_enable/1 reenables a disabled fuse
%%
circuit_enable(Name) ->
    fuse:circuit_enable(Name).

circuit_enable_pre(S) ->
    has_fuses_installed(S).

circuit_enable_args(S) ->
    Fuse = frequency(
        [{10, g_disabled_name(S)} || has_disabled(S)] ++
        [{1, g_name()}]),
    [Fuse].

circuit_enable_callouts(S, [Name]) ->
    case is_installed(S, Name) of
       false -> ?RET({error, not_found});
       true ->
           case is_disabled(S, Name) of
               false -> ?RET(ok);
               true ->
                   ?APPLY(remove_disabled, [Name]),
                   ?APPLY(clear_melts, [Name]),
                   ?APPLY(clear_blown, [Name]),
                   ?RET(ok)
           end
    end.

circuit_enable_features(S, [Name], _V) ->
    case is_installed(S, Name) of
        false -> [{fuse_eqc, r19, enable_uninstalled_fuse}];
        true -> [{fuse_eqc, r20, enable_installed, {blown, is_blown(S, Name)}}]
    end.

%% -- NORMAL OPERATION -----------------------------------------------

%% reset/1 resets a fuse back to its initial state
%% ---------------------------------------------------------------
reset(Name) ->
    fuse:reset(Name).

reset_pre(S) ->
    has_fuses_installed(S).

reset_args(_S) ->
    [g_name()].

%% Resetting a fuse resets its internal state
reset_callouts(S, [Name]) ->
    case is_installed(S, Name) of
        false -> ?RET({error, not_found});
        true ->
            ?APPLY(clear_melts, [Name]),
            ?APPLY(clear_blown, [Name]),
            ?RET(ok)
     end.

reset_features(S, [Name], _V) ->
    case is_installed(S, Name) of
        false -> [{fuse_eqc, r05, reset_uninstalled_fuse}];
        true -> [{fuse_eqc, r06, reset_installed, {blown, is_blown(S, Name)}}]
    end.

%%% ask/1 asks about the state of a fuse that exists
%% ---------------------------------------------------------------
%% Split into two variants

%% ask/1 on a fuse which is known to be installed
ask_installed(Name) ->
    fuse:ask(Name, ?CONTEXT).

ask_installed_pre(S) -> has_fuses_installed(S).

ask_installed_args(_S) -> [g_name()].

ask_installed_pre(S, [Name]) -> is_installed(S, Name).

ask_installed_features(_S, [_Name], _R) ->
    [{fuse_eqc, r15, ask_installed}].

ask_installed_callouts(_S, [Name]) ->
    ?MATCH(Res, ?APPLY(lookup, [Name])),
    ?RET(Res).

%% plain ask/1
ask(Name) ->
    fuse:ask(Name, ?CONTEXT).

ask_pre(S) -> has_fuses_installed(S).

ask_args(_S) -> [g_name()].

ask_callouts(_S, [Name]) ->
    ?MATCH(Res, ?APPLY(lookup, [Name])),
    ?RET(Res).

ask_features(S, [Name], _V) ->
    case is_installed(S, Name) of
       true -> [{fuse_eqc, r15, ask_installed}];
       false -> [{fuse_eqc, r16, ask_uninstalled}]
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

run_callouts(_S, [Name, Result, Return, _Fun]) ->
    ?MATCH(Res, ?APPLY(lookup, [Name])),
    case Res of
        {error, not_found} ->
            ?RET({error, not_found});
        blown ->
            ?RET(blown);
        ok ->
            case Result of
               ok -> ?EMPTY;
               melt ->
                 ?MATCH(Ts, ?APPLY(fuse_time_eqc, monotonic_time, [])),
                 ?APPLY(process_melt, [Name, Ts])
            end,
            ?RET({ok, Return})
    end.


run_features(_S, [_Name, ok, _, _], _R) -> [{fuse_eqc, r07, run_ok_fuse}];
run_features(S, [Name, melt, _, _], _R) ->
  case is_installed(S, Name) of
    true ->
      case is_blown(S, Name) of
        true -> [{fuse_eqc, r08, run_melt_on_blown_fuse}];
        false ->
           Disables = case is_disabled(S, Name) of
               true -> [{fuse_eqc, r21, run_melt_on_disabled_fuse}];
               false -> []
           end,
           Disables ++ [{fuse_eqc, r09, run_melt_on_installed_fuse}]
      end;
    false ->
      [{fuse_eqc, r10, run_on_uninstalled_fuse}]
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
    is_installed(S, Name).

melt_installed_callouts(_S, [Name]) ->
    ?MATCH(Ts, ?APPLY(fuse_time_eqc, monotonic_time, [])),
    ?APPLY(process_melt, [Name, Ts]),
    ?RET(ok).

melt_installed_features(_S, [_Name], _V) ->
    [{fuse_eqc, r11, melt_installed_fuse}].

melt(Name) ->
    fuse:melt(Name).

melt_pre(S) ->
    has_fuses_installed(S).

melt_args(_S) ->
    [g_name()].

melt_callouts(S, [Name]) ->
    ?MATCH(Ts, ?APPLY(fuse_time_eqc, monotonic_time, [])),
    case is_installed(S, Name) of
        false -> ?EMPTY;
        true ->
            ?APPLY(process_melt, [Name, Ts])
    end,
    ?RET(ok).

melt_features(S, [Name], _V) ->
    case is_installed(S, Name) of
        true ->
              Disabled = case is_disabled(S, Name) of
                  true -> [{fuse_eqc, r21, melt_on_disabled_fuse}];
                  false -> []
              end,
              [{fuse_eqc, r11, melt_installed_fuse}] ++ Disabled;
        false -> [{fuse_eqc, r12, melt_uninstalled_fuse}]
    end.

%% Internal helper call for melt processing
process_melt_callouts(_S, [Name, Ts]) ->
    ?APPLY(record_melt, [Name, Ts]),
    ?MATCH(Period, ?APPLY(fuse_period, [Name])),
    ?APPLY(expire_melts, [Name, Period, Ts]),
    ?APPLY(record_melt_history, [Name]).

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
    case is_installed(S, Name) of
        true -> ok;
        false -> {error, not_found}
    end.

%% Removing a fuse, removes it from the list of installed fuses.
remove_callouts(S, [Name]) ->
    case is_installed(S, Name) of
        false -> ?RET({error, not_found});
        true ->
            ?APPLY(remove_disabled, [Name]),
            ?APPLY(uninstall, [Name]),
            ?RET(ok)
     end.

uninstall_next(#state { installed = Is } = S, _, [Name]) ->
    S#state { installed = lists:keydelete(Name, 1, Is) }.
    
remove_features(S, [Name], _V) ->
    case is_installed(S, Name) of
        false -> [{fuse_eqc, r17, remove_uninstalled_fuse}];
        true -> [{fuse_eqc, r18, remove_installed_fuse}]
    end.

%% -- LOOKUP FUSE STATE (INTERNAL CALL) --------------------------------------------------------

lookup_callouts(S, [Name]) ->
    case lookup_fuse(S, Name) of
        not_found ->
            ?RET({error, not_found});
        {_, disabled} ->
            ?RET(blown);
        {_, blown} ->
            ?RET(blown);
        {standard, ok} ->
            ?RET(ok);
        {fault_injection, {gradual, X}} ->
            ?MATCH(Rand, ?CALLOUT(fuse_rand, uniform, [], g_split_float(X))),
            case Rand < X of
                true -> ?RET(blown);
                false -> ?RET(ok)
            end
    end.


%% -- RECORD MELT (INTERNAL CALL) -----------------------------
record_melt_next(#state { melts = Ms } = S, _, [Name, Ts]) ->
    S#state { melts = [{Name, Ts} | Ms] }.

%% -- EXPIRE MELTS (INTERNAL CALL) ---------------------------
%%
expire_melts_next(#state { melts = Ms } = S, _, [Who, Period, Now]) ->
    Updated = [{Name, Ts} || {Name, Ts} <- Ms, Name /= Who orelse in_period(Ts, Now, Period)],
    S#state { melts = Updated }.
    
expire_melts_features(#state { melts = Ms }, [Who, Period, Now], _) ->
    Updated = [{Name, Ts} || {Name, Ts} <- Ms, Name /= Who orelse in_period(Ts, Now, Period)],
    case Ms /= Updated of
        true -> [{fuse_eqc, r14, expiring_melts}];
        false -> []
    end.

%% -- COMPUTING FUSE PERIODS (INTERNAL CALL) ------------------------------
fuse_period_return(#state { installed = Is }, [Name]) ->
    {_, #{ period := Period }} = lists:keyfind(Name, 1, Is),
    Period.

%% -- RECORD MELT HISTORY (INTERNAL CALL) -------------------------
%%
record_melt_history_callouts(S, [Name]) ->
    ?WHEN(melt_state(S, Name) == blown
		andalso not is_blown(S, Name)
		andalso not is_disabled(S, Name),
        ?APPLY(blow_fuse, [Name])).

%% -- VARIOUS SMALLER INTERNAL CALLS --------------------------------
%%
clear_blown_callouts(S, [Name]) ->
    case blown_ref(S, Name) of
        not_found -> ?EMPTY;
        Ref -> ?APPLY(fuse_time_eqc, cancel_timer, [Ref])
    end.

clear_blown_next(#state { blown = Rs } = S, _, [Name]) ->
    S#state { blown = [{N, TRef} || {N, TRef} <- Rs, N /= Name] }.

clear_melts_next(#state { melts = Ms } = S, _, [Name]) ->
    S#state { melts = [{N, Ts} || {N, Ts} <- Ms, N /= Name] }.

add_disabled_next(#state { disabled = Ds } = State, _, [Name]) ->
    case lists:member(Name, Ds) of
       true -> State;
       false -> State#state { disabled = Ds ++ [Name] }
    end.

remove_disabled_next(#state { disabled = Ds } = State, _, [Name]) ->
    State#state { disabled = Ds -- [Name] }.

blow_fuse_callouts(S, [Name]) ->
    HealTime = heal_time(S, Name),
    ?MATCH(TRef, ?APPLY(fuse_time_eqc, send_after, [HealTime, ?WILDCARD, {reset, Name}])),
    ?APPLY(add_blown, [Name, TRef]).

blow_fuse_features(_S, _, _) ->
    [{fuse_eqc, r13, blowing_fuse}].

add_blown_next(#state { blown = Blown } = S, _, [Name, TRef]) ->
    S#state { blown = Blown ++ [{Name, TRef}] }.

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
weight(_, remove) -> 1;
weight(_, circuit_disable) -> 1;
weight(_, circuit_enable) -> 1.

%%% PROPERTIES
%% ---------------------------------------------------------------

postcondition_common(S, Call, Res) ->
    eq(Res, return_value(S, Call)).

%% Test the stateful system against a random sequential command sequence.
prop_component() ->
    ?SETUP( fun() ->
        eqc_mocking:start_mocking(api_spec()),
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

cleanup() ->
    (catch application:stop(fuse)),
    {ok, _Apps} = application:ensure_all_started(fuse).

setup() ->
  error_logger:tty(false),
  application:load(fuse),
  application:set_env(fuse, monitor, false),
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  application:set_env(sasl, errlog_type, error),
  application:start(sasl).

%%% Helpers
%%% ---------------------

sample() ->
    eqc_gen:sample(commands(?MODULE)).

%%% INTERNALS
%%% ---------------------

%% is_installed/2 determines if a given fuse is installed
is_installed(#state { installed = Is }, N) -> lists:keymember(N, 1, Is).

%% valid_opts/1 determines if the given options are valid
valid_opts({{standard, K, R}, {reset, T}})
  when K > 0, R >= 0, T >= 0 ->
    true;
valid_opts({{fault_injection, Rate, K, R}, {reset, T}})
  when K > 0, R >= 0, T >= 0, is_float(Rate), 0 < Rate, Rate =< 1.0 ->
    true;
valid_opts(_) ->
    false.

melt_state(S, Name) ->
    count_state(fuse_intensity(S, Name) - count_melts(S, Name)).

lookup_fuse(#state { installed = Fs } = S, Name) ->
    case is_disabled(S, Name) of
        true -> {Name, disabled};
        false ->
            case lists:keyfind(Name, 1, Fs) of
                false -> not_found;
                {_, #{ fuse_type := standard }} ->
                    Blown = case is_blown(S, Name) of
                        true -> blown;
                        false -> ok
                    end,
                    {standard, Blown};
                {_, #{ fuse_type := fault_injection, rate := Rate }} ->
                    Blown = case is_blown(S, Name) of
                        true -> blown;
                        false -> {gradual, Rate}
                    end,
                    {fault_injection, Blown}
            end
    end.

is_blown(#state { blown = Blown }, Name) ->
    lists:keymember(Name, 1, Blown).

blown_ref(#state { blown = Blown }, Name) ->
    case lists:keyfind(Name, 1, Blown) of
        false -> not_found;
        {_, R} -> R
    end.

is_disabled(#state { disabled = Ds }, Name) ->
    lists:member(Name, Ds).

has_disabled(#state { disabled = Ds }) -> Ds /= [].

fuse_intensity(#state { installed = Inst }, Name) ->
    {Name, #{ count := Count } } = lists:keyfind(Name, 1, Inst),
    Count.

heal_time(#state { installed = Inst }, Name) ->
    {Name, #{ reset := R }} = lists:keyfind(Name, 1, Inst),
    R.

count_state(N) when N < 0 -> blown;
count_state(_N) -> ok.

count_melts(#state { melts = Ms }, Name) ->
    length([N || {N, _} <- Ms, N == Name]).

has_fuses_installed(#state { installed = [] }) -> false;
has_fuses_installed(#state { installed = [_|_]}) -> true.

parse_opts({{standard, C, P},Cmds}) ->
    #{ fuse_type => standard, count => C, period => P, reset => parse_cmds(Cmds) };
parse_opts({{fault_injection, Rate, C, P}, Cmds}) ->
    #{ fuse_type => fault_injection, rate => Rate, count => C, period => P, reset => parse_cmds(Cmds) }.

parse_cmds({reset, N}) -> N.

%% Alternative implementation of being inside the period, based on microsecond conversion.
in_period(Ts, Now, _) when Now < Ts -> false;
in_period(Ts, Now, Period) when Now >= Ts -> (Now - Ts) < Period.

-endif.
