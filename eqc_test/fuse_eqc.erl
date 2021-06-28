%%% The fuse_eqc module implements a Quickcheck model for the Fuse main gen_server.
%%
-module(fuse_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

%%% State of Fuses.
-record(fuse, {
          type,
          count :: non_neg_integer(),
          period :: pos_integer(),
          state = ok,
          timer = undefined,
          configuration,
          disabled = false
}).

%%% Model state.
-record(state, {
          melts = [], % History of current melts issued to the SUT
          installed = [] :: [{term(), #fuse{} }] % List of installed fuses, with their configuration.
}).

%% Fuses require sync context for their correct operations w.r.t linearization.
%% Otherwise, you might get stale reads.
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


%% g_fuse_name/0 generates fuse names for use in the system
g_fuse_name() ->
    frequency([
        {1, elements([phineas, ferb, candace, isabella, vanessa, perry, heinz])},
        {1, nat()},
        {1, {nat(), nat()}}
    ]).

%% g_atom/0 generates a simple atom from a short list.
%% It's important that these atoms doesn't occur in g_fuse_name/0.
g_atom() ->
    oneof([invalid_a,
           invalid_b,
           invalid_c,
           invalid_d,
           invalid_e,
           invalid_f]).

%% g_name/0 generates one of the fuses at random.
%% fault injects provably invalid names
g_name() ->
      fault(g_atom(), g_fuse_name()).

%% g_disabled_name/1 generated a fuse which has been disabled
g_disabled_name(#state { installed = IS }) ->
    elements([N || {N, F} <- IS, F#fuse.disabled == true]).

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
            {standard, choose(1, 2), choose(1, 30000)},
            {fault_injection, g_uniform_real(), choose(1,2), choose(1,30000)}
        ])
    ).

%% g_cmd/0 generates a command for the internal command language of the fuse
%% system.
g_cmd() ->
    oneof(
      [
       {delay, ?LET(N, nat(), N+1)},
       {barrier, g_atom()},
       {gradual, g_uniform_real()},
       heal]).

%% g_refresh/0 generates a refresh setting.
g_refresh() ->
    oneof([{reset, choose(1, 60000)}]).

%% g_options/0 generates install options
g_options() ->
    {g_strategy(), g_refresh()}.


%% g_time_inc/0 generates a time increment.
g_time_inc() ->
    choose(1, 4000*1000).

%% initial_state/0 generates the initial system state
initial_state() -> #state{}.

%% ---- RECEIVING RESET TIMER MESSAGES MESSAGES ------------------------------------------------------

%% inject_reset/2 sends timer messages into the SUT
%% ---------------------------------------------------------------
%% Heal a fuse which has been blown in the system.
inject_reset(Name, _TRef) ->
    fuse_server ! {reset, Name},
    fuse_server:sync(), %% synchronize to avoid a race condition.
    ok.

%% Return the fuses which have timers, because these are the ones you
%% can reset explicitly.
fuses_with_timers(#state { installed = Installed }) ->
    [{N, Ref} || {N, #fuse{ timer = Ref }} <- Installed,
                 Ref /= undefined].

%% You can reset a fuse if there is a blown fuse in the system.
inject_reset_pre(S) ->
    fuses_with_timers(S) /= [].

%% Generate one of the fuses with a timer
inject_reset_args(S) ->
    ?LET({Name, TRef}, elements(fuses_with_timers(S)),
         [Name, TRef]).

%% Fuses will only be reset if their state is among the installed and
%% are blown.
inject_reset_pre(S, [Name, _]) ->
    is_installed(S, Name) andalso is_blown(S, Name).

%% Note: when a fuse heals, the internal state is reset.
inject_reset_callouts(S, [Name, TRef]) ->
    ?APPLY(fuse_time_eqc, trigger, [TRef]),
    case is_blown(S, Name) of
        false ->
            ?EMPTY;
        true ->
            ?APPLY(exec_reset, [Name]),
            ?APPLY(remove_timer, [Name, TRef])
    end,
    ?RET(ok).

inject_reset_features(S, [Name, _], _Response) ->
    case is_installed(S, Name) of
        false -> [{fuse_eqc, r01, inject_reset_non_installed}];
        true -> [{fuse_eqc, r02, {inject_reset_installed_fuse, is_blown(S, Name)}}]
    end.


%% ---- INSTALLATION ------------------------------------------------------

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
%% Also, consider if the installed fuse is valid at all.
install_callouts(_S, [Name, Opts]) ->
    #fuse{ period = P } = Fuse = parse_fuse(Opts),
    ?APPLY(fuse_time_eqc, convert_time_unit, [P, milli_seconds, native]),
    case valid_opts(Opts) of
        false ->
            ?RET(badarg);
        true ->
            ?APPLY(install_fuse, [Name, Fuse]),
            ?APPLY(clear_blown, [Name]),
            ?APPLY(clear_melts, [Name]),
            ?RET(ok)
    end.

%% Internal helper
install_fuse_next(#state { installed = Is } = S, _, [Name, Fuse]) ->
    %% Copy the disabled state from a fuse which is already here,
    %% if applicable. Also copy the timer state into the updated fuse.
    %% This allows the clear_blown call to correctly remove the timer
    %% from the timing system. Otherwise, there's no timer and the
    %% model will fail because of an unexpected timer cancellation.
    NewFuse = case lists:keysearch(Name, 1, Is) of
                  false -> Fuse;
                  {value, {Name, Old}} -> Fuse#fuse{ disabled = Old#fuse.disabled,
                                                     timer = Old#fuse.timer }
              end,
    S#state { installed = lists:keystore(Name, 1, Is, {Name, NewFuse}) }.

install_features(S, [Name, Opts], _R) ->
    case valid_opts(Opts) of
        false -> [{fuse_eqc, r03, installing_invalid_fuse}];
        true ->
            case Opts of
                {{standard, _, _}, _} ->
                    [{fuse_eqc, r03, {installing_fuse, {standard, is_installed(S, Name)}}}];
                {{fault_injection, _, _, _}, _} ->
                    [{fuse_eqc, r03, {installing_fuse, {fault_injection, is_installed(S, Name)}}}]
            end
    end.

%% ---- DISABLING AND ENABLING CIRCUITS ----------------------------------

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

%% ---- MANUALLY RESET THE FUSE STATE -----------------------------------------------

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

%% ---- ASK ABOUT FUSE STATE -----------------------------------------------

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
    ?APPLY(lookup, [Name]).

%% plain ask/1
ask(Name) ->
    fuse:ask(Name, ?CONTEXT).

ask_pre(S) -> has_fuses_installed(S).

ask_args(_S) -> [g_name()].

ask_callouts(_S, [Name]) ->
    ?APPLY(lookup, [Name]).

ask_features(S, [Name], _V) ->
    case is_installed(S, Name) of
       true -> [{fuse_eqc, r15, ask_installed}];
       false -> [{fuse_eqc, r16, ask_uninstalled}]
    end.

%% ---- RUN HELPER -----------------------------------------------

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

%% ---- MELT -----------------------------------------------


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

%% ---- REMOVE -----------------------------------------------

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

%% ---- LOOKUP FUSE STATE (INTERNAL CALL) --------------------------------

lookup_callouts(S, [Name]) ->
    case lookup_fuse(S, Name) of
        not_found -> ?RET({error, not_found});
        disabled -> ?RET(blown);
        blown -> ?RET(blown);
        ok -> ?RET(ok);
        {gradual, X} ->
            ?MATCH(Rand, ?CALLOUT(fuse_rand, uniform, [], g_split_float(X))),
            case Rand < X of
                true -> ?RET(blown);
                false -> ?RET(ok)
            end
    end.


%% ---- RECORD MELT (INTERNAL CALL) -----------------------------
record_melt_next(#state { melts = Ms } = S, _, [Name, Ts]) ->
    S#state { melts = [{Name, Ts} | Ms] }.

%% ---- EXPIRE MELTS (INTERNAL CALL) ---------------------------
expire_melts_next(#state { melts = Ms } = S, _, [Who, Period, Now]) ->
    Updated = [{Name, Ts} || {Name, Ts} <- Ms, Name /= Who orelse in_period(Ts, Now, Period)],
    S#state { melts = Updated }.

expire_melts_features(#state { melts = Ms }, [Who, Period, Now], _) ->
    Updated = [{Name, Ts} || {Name, Ts} <- Ms, Name /= Who orelse in_period(Ts, Now, Period)],
    case Ms /= Updated of
        true -> [{fuse_eqc, r14, expiring_melts}];
        false -> []
    end.

%% ---- COMPUTING FUSE PERIODS (INTERNAL CALL) ------------------------------
fuse_period_return(#state { installed = Is }, [Name]) ->
    {_, #fuse{ period = Period }} = lists:keyfind(Name, 1, Is),
    Period.

%% ---- RECORD MELT HISTORY (INTERNAL CALL) -------------------------
record_melt_history_callouts(S, [Name]) ->
    ?WHEN(melt_state(S, Name) == blown
		andalso not is_blown(S, Name)
		andalso not is_disabled(S, Name),
        ?APPLY(blow_fuse, [Name])).

%% ---- VARIOUS SMALLER INTERNAL CALLS --------------------------------
clear_blown_callouts(S, [Name]) ->
    case blown_ref(S, Name) of
        not_found -> ?EMPTY;
        no_timer -> ?EMPTY;
        Ref ->
            ?APPLY(fuse_time_eqc, cancel_timer, [Ref])
    end,
    ?APPLY(clear_blown_fuse, [Name]).

clear_blown_fuse_next(S, _, [Name]) ->
    with_fuse(S, Name, fun(F) -> F#fuse{ state = ok, timer = undefined } end).

clear_melts_next(#state { melts = Ms } = S, _, [Name]) ->
    S#state { melts =
                  [{N, Ts} || {N, Ts} <- Ms, N /= Name] }.

set_fuse_state(#state { installed = IS} = State, Name, Setting) ->
    case lists:keytake(Name, 1, IS) of
        false -> State;
        {value, {_, F}, IS2} ->
            Stored = lists:keystore(Name, 1, IS2, {Name, F#fuse { disabled = Setting }}),
            State#state { installed = Stored }
    end.

add_disabled_next(State, _, [Name]) ->
    set_fuse_state(State, Name, true).

remove_disabled_next(State, _, [Name]) ->
    set_fuse_state(State, Name, false).

blow_fuse_callouts(_S, [Name]) ->
    ?APPLY(add_blown, [Name]),
    ?APPLY(exec_reset, [Name]).

blow_fuse_features(_S, _, _) ->
    [{fuse_eqc, r13, blowing_fuse}].

add_blown_next(S, _, [Name]) ->
    with_fuse(S, Name,
              fun
                  (F) -> F#fuse { state = {blown, F#fuse.configuration}}
              end).

exec_reset_callouts(_S, [Name]) ->
    ?APPLY(process_commands, [Name]).

process_commands_callouts(_S, [Name]) ->
    ?MATCH(Next, ?APPLY(next_command, [Name])),
    case Next of
        done ->
            ?APPLY(clear_blown, [Name]),
            ?APPLY(clear_melts, [Name]);
        {delay, Ms} ->
            ?APPLY(send_after, [Name, Ms])
    end.

%% Assert the model has no timers on a given fuse.
check_no_timer_callouts(#state { installed = IS }, [Name]) ->
    case lists:keyfind(Name, 1, IS) of
        false -> ?EMPTY;
        {_, #fuse { timer = undefined }} -> ?EMPTY;
        {_, #fuse { timer = TRef }} -> ?FAIL({timer_present, TRef})
    end.

add_timer_next(S, _, [Name, TRef]) ->
    with_fuse(S, Name,
              fun(#fuse { state = {blown, Cmds}, timer = undefined } = F) ->
                      F#fuse { state = {blown, Cmds}, timer = TRef}
              end).

send_after_callouts(_S, [Name, Ms]) ->
    ?MATCH(TRef,
           ?APPLY(fuse_time_eqc, send_after, [Ms, ?WILDCARD, {reset, Name}])),
    ?APPLY(check_no_timer, [Name]),
    ?APPLY(add_timer, [Name, TRef]).

remove_timer_next(S, _, [Name, TRef]) ->
    with_fuse(S, Name,
              fun
                  (#fuse { timer = undefined } = F) ->
                      F;
                  (#fuse { timer = Ref } = F) when Ref == TRef ->
                      F#fuse { timer = undefined };
                  (#fuse { timer = _ } = F) ->
                      exit({wrong_timer, F})
              end).

next_command_callouts(S, [Name]) ->
    #fuse { state = {blown, Cs} } = fuse(S, Name),
    case Cs of
        [] -> ?RET(done);
        [{gradual, Level}|_] -> ?RET({gradual, Level});
        [{barrier, Term}|_] -> ?RET({barrier, Term});
        [{delay, Ms}|_] -> ?RET({delay, Ms})
    end.

next_command_next(S, _, [Name]) ->
    with_fuse(S, Name,
              fun
                  (#fuse { state = {blown, Cmds}} = F) ->
                      case Cmds of
                          [] -> F#fuse { state = {blown, []} };
                          [_|Xs] -> F#fuse { state = {blown, Xs}}
                      end
              end).

next_command_features(S, [Name], _) ->
    #fuse { state = FuseState } = fuse(S, Name),
    case FuseState of
        {blown, []} -> [{?MODULE, r22, next_command, heal}];
        {blown, [{delay, _} | _]} -> [{?MODULE, r23, next_command, delay}]
    end.

%%% Command weight distribution
%% ---------------------------------------------------------------
weight(_, elapse_time) -> 5;
weight(_, install) -> 1;
weight(_, reset) -> 2;
weight(_, run) -> 5;
weight(_, melt) -> 1;
weight(_, melt_installed) -> 40;
weight(_, inject_reset) -> 100;
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
    count_state(
      fuse_intensity(S, Name) - count_melts(S, Name)).

lookup_fuse(#state { installed = Fs } = S, Name) ->
    case is_disabled(S, Name) of
        true -> disabled;
        false ->
            case lists:keyfind(Name, 1, Fs) of
                false -> not_found;
                {_, #fuse { type = standard, configuration = CL }} ->
                    lookup_blown(S, Name, ok, CL);
                {_, #fuse { type = {fault_injection, Rate}, configuration = CL } } ->
                    lookup_blown(S, Name, {gradual, Rate}, CL)
            end
    end.

lookup_blown(S, Name, OK, Cmds) ->
    case is_blown(S, Name) of
        false -> OK;
        true ->
            case Cmds of
                [{delay, _} | _] -> blown
            end

    end.

is_blown(S, Name) ->
    #fuse { state = FS } = fuse(S, Name),
    case FS of
        {blown, _} -> true;
        _ -> false
    end.

blown_ref(S, Name) ->
    case fuse(S, Name) of
        not_found ->
            not_found;
        #fuse { timer = undefined } ->
            no_timer;
        #fuse { timer = Ref } ->
            Ref
    end.

installed_fuse_names(#state { installed = Is }) ->
    [N || {N, _} <- Is].

blown_fuses(S) ->
    Names = installed_fuse_names(S),
    [N || N <- Names,
          is_blown(S, N)].

is_disabled(#state { installed = IS }, Name) ->
    case lists:keyfind(Name, 1, IS) of
        false -> false;
        {_, #fuse { disabled = D }} -> D
    end.

has_disabled(#state { installed = IS }) ->
    Disabled = [x || {_, #fuse { disabled = true }} <- IS],
    Disabled /= [].

fuse_intensity(#state { installed = Inst }, Name) ->
    {Name, #fuse{ count = Count } } = lists:keyfind(Name, 1, Inst),
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

parse_fuse({{standard, C, P},Cmds}) ->
    #fuse {
       state = ok,
       type = standard,
       count = C,
       period = P,
       configuration = parse_cmds(Cmds)
      };
parse_fuse({{fault_injection, Rate, C, P}, Cmds}) ->
    #fuse {
       state = ok,
       type = {fault_injection, Rate},
       count = C,
       period = P,
       configuration = parse_cmds(Cmds)
      }.

fuse(S, Name) ->
    case lists:keyfind(Name, 1, S#state.installed) of
        false -> not_found;
        {_, Conf} -> Conf
    end.

with_fuse(S, Name, Fun) ->
    F = fuse(S, Name),
    NF = Fun(F),
    S#state { installed =
                  lists:keystore(Name, 1, S#state.installed, {Name, NF})
            }.

parse_cmds({reset, K}) -> [{delay, K}];
parse_cmds(Cmds) -> Cmds.

%% Alternative implementation of being inside the period, based on microsecond conversion.
in_period(Ts, Now, _) when Now < Ts -> false;
in_period(Ts, Now, Period) when Now >= Ts ->
    (Now - Ts) < Period.
