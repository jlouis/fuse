-module(fuse_time_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-type time() :: integer().
-type time_ref() :: integer().

-record(timer, {
    point :: time(),
    ref :: time_ref(),
    owner :: pid(),
    msg :: term()
}).
-type timer() :: #timer{}.

-record(state, {
	time = 0 :: time(),
	timers = [] :: [timer()],
	time_ref = 0 :: time_ref()
}).

api_spec() ->
    #api_spec {
      language = erlang,
      modules = [
          #api_module {
          	name = fuse_time,
          	functions = [
          		#api_fun { name = monotonic_time, arity = 0 },
          		#api_fun { name = convert_time_unit, arity = 3 },
          		#api_fun { name = send_after, arity = 3 },
          		#api_fun { name = cancel_timer, arity = 1 }
          	] } ]
    }.

initial_state() -> #state{}.

%% -- ADVANCING TIME --------------------------
%%

advance_time(_) -> ok.

advance_time_pre(#state { timers = [] }) -> false;
advance_time_pre(#state {}) -> true.

advance_time_args(#state{ time = T }) ->
    ?LET(A, frequency([
                 {10, ?LET(K, nat(), K+1)},
                 {10, ?LET({K, N}, {nat(), nat()}, (N+1)*1000 + K)},
                 {10, ?LET({K, N, M}, {nat(), nat(), nat()}, (M+1)*60*1000 + N*1000 + K)},
                 {1, ?LET({K, N, Q}, {nat(), nat(), nat()}, (Q*17)*60*1000 + N*1000 + K)}
                ]),
        [T + A]).

advance_time_next(State, _, [T]) ->
    State#state { time = T }.

advance_time_return(_S, _) -> ok.

advance_time_features(_, _, _) -> [{fuse_time, r00, advance_time}].

%% -- PRECONDITIONS --------------------------------------------------------
%% This is to be used by another component as:
%% ?APPLY(fuse_time_eqc, trigger, []) in a callout specification. This ensures the given command can
%% only be picked if you can trigger the timer.

can_fire(#state { time = T, timers = TS }, Ref) ->
     case lists:keyfind(Ref, #timer.ref, TS) of
         false -> false;
         {TP, _, _, _} -> T >= TP
     end.

%% General trigger of a timer with a message
trigger_pre(S, [{tref, Ref}]) -> can_fire(S, Ref).

trigger_return(#state { timers = TS }, [{tref, Ref}]) ->
    case lists:keyfind(Ref, #timer.ref, TS) of
        {_TP, _Ref, _Pid, Msg} -> Msg
    end.

trigger_next(#state { timers = TS } = S, _, [{tref, Ref}]) ->
    S#state{ timers = lists:keydelete(Ref, #timer.ref, TS) }.

can_fire_msg(#state { time = T, timers = TS }, Msg) ->
    case lists:keyfind(Msg, #timer.msg, TS) of
        false -> false;
        {TP, _, _, _} -> T >= TP
    end.

%% Targeted trigger. Used to check if a given particular message
%% can be fired. It can be used to "select" among several possible
%% messages
trigger_msg_pre(S, [Msg]) -> can_fire_msg(S, Msg).
trigger_msg_return(_S, [Msg]) -> Msg.

trigger_msg_next(#state { timers = TS } = S, _, [Msg]) ->
    {_, Ref, _, _} = lists:keyfind(Msg, #timer.msg, TS),
    S#state{ timers = lists:keydelete(Ref, #timer.ref, TS) }.

%% INTERNAL CALLS IN THE MODEL
%% -------------------------------------------
%%
%% All these calls are really "wrappers" such that if you call into the timing model, you obtain
%% faked time.

monotonic_time_callers() -> [fuse_server].

monotonic_time_callouts(#state {time = T }, []) ->
    ?CALLOUT(fuse_time, monotonic_time, [], T),
    ?RET(T).

monotonic_time_return(#state { time = T }, []) -> T.

convert_time_unit_callers() -> [fuse_server].

convert_time_unit_callouts(_S, [T, From, To]) ->
    ?CALLOUT(fuse_time, convert_time_unit, [T, From, To], T),
    case {From, To} of
        {native, milli_seconds} -> ?RET(T);
        {milli_seconds, native} -> ?RET(T);
        FT -> ?FAIL({convert_time_unit, FT})
    end.

send_after_callers() -> [fuse_server].

send_after_callouts(#state { time_ref = Ref}, [Timeout, Reg, Msg]) when is_atom(Reg) ->
    ?CALLOUT(fuse_time, send_after, [Timeout, Reg, Msg], {tref, Ref}),
    ?RET({tref, Ref});
send_after_callouts(#state { time_ref = Ref}, [Timeout, Pid, Msg]) when is_pid(Pid) ->
    ?CALLOUT(fuse_time, send_after, [Timeout, ?WILDCARD, Msg], {tref, Ref}),
    ?RET({tref, Ref}).

send_after_next(#state { time = T, time_ref = Ref, timers = TS } = S, _, [Timeout, Pid, Msg]) ->
    TriggerPoint = T + Timeout,
    S#state { time_ref = Ref + 1, timers = TS ++ [{TriggerPoint, Ref, Pid, Msg}] }.

cancel_timer_callers() -> [fuse_server].

cancel_timer_callouts(S, [{tref, TRef}]) ->
    Return = cancel_timer_rv(S, TRef),
    ?CALLOUT(fuse_time, cancel_timer, [{tref, TRef}], Return),
    ?RET(Return).

cancel_timer_rv(#state { time = T, timers = TS }, TRef) ->
    case lists:keyfind(TRef, #timer.ref, TS) of
        false -> false;
        {TriggerPoint, TRef, _Pid, _Msg} -> monus(TriggerPoint, T)
    end.

cancel_timer_next(#state { timers = TS } = S, _, [{tref, TRef}]) ->
    S#state { timers = lists:keydelete(TRef, #timer.ref, TS) }.

%% HELPER ROUTINES
%% ----------------------------------------

%% A monus operation is a subtraction for natural numbers
monus(A, B) when A > B -> A - B;
monus(A, B) when A =< B -> 0.

%% PROPERTY
%% ----------------------------------

%% The property here is a pretty dummy property as we don't need a whole lot for this to work.

%% Use a common postcondition for all commands, so we can utilize the valid return
%% of each command.
postcondition_common(S, Call, Res) ->
    eq(Res, return_value(S, Call)).

%% Main property, just verify that the commands are in sync with reality.
prop_component_correct() ->
    ?SETUP(fun() ->
        eqc_mocking:start_mocking(api_spec()),
        fun() -> ok end
    end,
    ?FORALL(Cmds, commands(?MODULE),
      begin
        {H,S,R} = run_commands(?MODULE, Cmds),
        pretty_commands(?MODULE, Cmds, {H,S,R},
            aggregate(with_title('Commands'), command_names(Cmds),
            collect(eqc_lib:summary('Length'), length(Cmds),
            aggregate(with_title('Features'), eqc_statem:call_features(H),
            features(eqc_statem:call_features(H),
                R == ok)))))
      end)).

%% Helper for showing states of the output:
t() -> t(5).

t(Secs) ->
    eqc:quickcheck(eqc:testing_time(Secs, eqc_statem:show_states(prop_component_correct()))).

