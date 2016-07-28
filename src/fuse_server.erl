%%% @doc Runs the fuse server in the system
%%% @private
-module(fuse_server).
-behaviour(gen_server).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.


%% Lifetime API
-export([start_link/0]).

%% Operational API
-export([
    ask/2,
    install/2,
    melt/1,
    remove/1,
    reset/1,
    run/3]).

%% Administrative API
-export([
    circuit/2
]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%% Private
-export([sync/0, q_melts/0]).

-define(TAB, fuse_state).

-record(state, { fuses = [] }).
-record(fuse, {
    name :: atom(),
    intensity :: integer(),
    period :: integer(),
    heal_time :: integer(),
    melt_history = [],
    ty = ok :: 'ok' | {gradual, float()},
    timer_ref = none,
    enabled = true
}).

%% -- API -----------------------------------------------------

%% ------
%% @doc Start up the manager server for the fuse system
%% This is assumed to be called by (@see fuse_sup). The `Timing' parameter controls how the system manages timing.
%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------
%% @doc install/2 installs a new fuse into the running system
%% Install a new fuse under `Name' with options given by `Opts'.
%% We assume `Opts' are already in the right verified and validated format.
%% @end
install(Name, Opts) ->
    %% Assume options are already verified
    Fuse = init_state(Name, Opts),
    gen_server:call(?MODULE, {install, Fuse}).

%% @doc ask/2 asks about the current given fuse state in a given context setting
%% The documentation is (@see fuse:ask/1)
%% @end
-spec ask(atom(), fuse:fuse_context()) -> ok | blown | {error, not_found}.
ask(Name, sync) -> gen_server:call(?MODULE, {ask, Name});
ask(Name, async_dirty) -> ask_(Name).

%% ask_/1 is the real ask function.
ask_(Name) ->
    StatsPlugin = application:get_env(fuse, stats_plugin, fuse_stats_ets),
    try ets:lookup_element(?TAB, Name, 2) of
        ok ->
          _ = StatsPlugin:increment(Name, ok),
          ok;
        blown ->
          _ = StatsPlugin:increment(Name, blown),
          blown;
        {gradual, Rate} ->
            case fuse_rand:uniform() of
                K when K < Rate ->
                    blown;
                _ ->
                    ok
            end
    catch
        error:badarg ->
            {error, not_found}
    end.

%% @doc reset/1 resets the fuse
%% The documentation is (@see fuse:reset/1)
%% @end
-spec reset(atom()) -> ok | {error, not_found}.
reset(Name) ->
    gen_server:call(?MODULE, {reset, Name}).

%% @doc circuit/2 is used to manually override the fuse state
%% @end
-spec circuit(atom(), enable | disable) -> ok.
circuit(Name, Switch) ->
    gen_server:call(?MODULE, {circuit, Name, Switch}).

%% @doc melt/2 melts the fuse at a given point in time
%% For documentation, (@see fuse:melt/2)
%% @end
-spec melt(Name) -> ok
    when Name :: atom().
melt(Name) ->
    gen_server:call(?MODULE, {melt, Name}).

%% @doc remove/1 removes the fuse
%% The documentation is (@see fuse:remove/1)
%% @end
-spec remove(atom()) -> ok | {error, not_found}.
remove(Name) ->
    gen_server:call(?MODULE, {remove, Name}).

%% sync/0 syncs the server. For internal use only in tests
%% @private
sync() ->
    gen_server:call(?MODULE, sync).

q_melts() ->
    gen_server:call(?MODULE, q_melts).

%% run/3 runs a thunk under a given fuse in a given context
%% @doc Documentation is (@see fuse:run/3)
%% @end
%% @private
-spec run(Name, fun(() -> {ok, Result} | {melt, Result}), fuse:fuse_context()) ->
    {ok, Result} | blown | {error, not_found}
  when
    Name :: atom(),
    Result :: any().

run(Name, Func, Context) ->
    case ask(Name, Context) of
        blown -> blown;
        ok ->
          case Func() of
              {ok, Result} -> {ok, Result};
              {melt, Result} ->
                  melt(Name),
                  {ok, Result}
          end;
        {error, Reason} ->
          {error, Reason}
    end.

%% -- CALLBACKS --------------------------------------------

%% @private
init([]) ->
    _ = ets:new(?TAB, [named_table, protected, set, {read_concurrency, true}, {keypos, 1}]),
    {ok, #state{ }}.

%% @private
handle_call({install, #fuse { name = Name } = F}, _From, #state { fuses = Fs } = State) ->
        case lists:keytake(Name, #fuse.name, Fs) of
            false ->
                install_metrics(F),
                fix(F),
                {reply, ok, State#state { fuses = lists:keystore(Name, #fuse.name, Fs, F)}};
            {value, OldFuse, _Otherfuses} ->
                EF = F#fuse { enabled = OldFuse#fuse.enabled },
                fix(EF),
                _ = reset_timer(OldFuse), %% For effect only
                %% Transplant the enabled-state from the old fuse
                {reply, ok, State#state {
                    fuses = lists:keystore(Name, #fuse.name, Fs, EF) }}
        end;
handle_call({circuit, Name, Switch}, _From, State) ->
    {Reply, State2} = handle_circuit(Name, Switch, State),
    {reply, Reply, State2};
handle_call({reset, Name}, _From, State) ->
    {Reply, State2} = handle_reset(Name, State, reset),
    {reply, Reply, State2};
handle_call({remove, Name}, _From, State) ->
    {Reply, State2} = handle_remove(Name, State),
    {reply, Reply, State2};
handle_call({melt, Name}, _From, State) ->
    Now = fuse_time:monotonic_time(),
    {Res, State2} = with_fuse(Name, State, fun(F) -> add_restart(Now, F) end),
    case Res of
      ok ->
            StatsPlugin = application:get_env(fuse, stats_plugin, fuse_stats_ets),
            _ = StatsPlugin:increment(Name, melt),
        {reply, ok, State2};
      not_found -> {reply, ok, State2}
    end;
handle_call({ask, Name}, _F, State) ->
    {reply, ask_(Name), State};
handle_call(sync, _F, State) ->
    {reply, ok, State};
handle_call(q_melts, _From, #state { fuses = Fs } = State) ->
        {reply, [{N, Ms} || #fuse { name = N, melt_history = Ms } <- Fs], State};
handle_call(q_disabled, _From, #state { fuses = Fs } = State) ->
    {reply, [Name || #fuse { name = Name, enabled = false } <- Fs], State};
handle_call(_M, _F, State) ->
    {reply, {error, unknown}, State}.

%% @private
handle_cast(_M, State) ->
    {noreply, State}.

%% @private
handle_info({reset, Name}, State) ->
    {_Reply, State2} = handle_reset(Name, State, timeout),
    {noreply, State2};
handle_info(_M, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
%%% ------

handle_reset(Name, State, ResetType) ->
    Reset = fun(#fuse { timer_ref = TRef } = F) ->
            case ResetType of
                reset ->
                    fix(F),
                    NewF = reset_timer(F),
                    {ok, NewF#fuse { melt_history = [] }};
                timeout when TRef /= none ->
                    fix(F),
                    NewF = reset_timer(F),
                    {ok, NewF#fuse { melt_history = [], timer_ref = none }};
                timeout when TRef == none ->
                    %% No timer installed on this fuse, it is a async fluke
                    {ok, F}
            end
    end,
    {Res, State2} = with_fuse(Name, State, Reset),
    case Res of
      ok -> {ok, State2};
      not_found -> {{error, not_found}, State2}
    end.

handle_circuit(Name, Switch, State) ->
    Fn = fun(#fuse { enabled = En } = F) ->
        case Switch of
            enable when En -> {ok, F};
            enable ->
                F1 = F#fuse { enabled = true },
                fix(F1),
                {ok, F1#fuse { melt_history = [] }};
            disable when not En -> {ok, F};
            disable ->
                blow(F),
                NewF = reset_timer(F),
                {ok, NewF#fuse { enabled = false }}
        end
    end,
    {Res, State2} = with_fuse(Name, State, Fn),
    case Res of
        ok -> {ok, State2};
        not_found -> {{error, not_found}, State2}
    end.

handle_remove(Name, #state { fuses = Fs } = State) ->
    case lists:keytake(Name, #fuse.name, Fs) of
        false -> {{error, not_found}, State};
        {value, F, OtherFs} ->
            delete(F),
            {ok, State#state { fuses = OtherFs }}
    end.

init_state(Name, {{fault_injection, Rate, MR, MT}, {reset, Reset}}) ->
    init_state(Name, {gradual, Rate}, MR, MT, {reset, Reset});
init_state(Name, {{standard, MR, MT}, {reset, Reset}}) ->
    init_state(Name, ok, MR, MT, {reset, Reset}).

init_state(Name, Ty, MR, MT, {reset, Reset}) ->
    NativePeriod = fuse_time:convert_time_unit(MT, milli_seconds, native),
    #fuse {
      name = Name,
      intensity = MR,
      period = NativePeriod,
      heal_time = Reset,
      ty = Ty
    }.

with_fuse(Name, #state { fuses = Fs} = State, Fun) ->
    case lists:keytake(Name, #fuse.name, Fs) of
        false -> {not_found, State};
        {value, F, OtherFs} ->
            {R, FF} = Fun(F),
            {R, State#state { fuses = [FF | OtherFs] }}
    end.

add_restart(Now, #fuse { intensity = I, period = Period, melt_history = R } = Fuse) ->
    R1 = add_restart_([Now | R], Now, Period),
    NewF = Fuse#fuse { melt_history = R1 },
    case length(R1) of
        CurI when CurI =< I ->
            {ok, NewF};
        _ ->
            blow(NewF),
            {ok, install_timer(NewF)}
    end.

add_restart_([R|Restarts], Now, Period) ->
    case in_period(R, Now, Period) of
        true -> [R|add_restart_(Restarts, Now, Period)];
        false -> []
    end;
add_restart_([], _, _) -> [].

in_period(Time, Now, Period) when (Now - Time) >= Period -> false;
in_period(_, _, _) -> true.

%% -- FUSE MANIPULATION ------------------
blow(#fuse { enabled = false }) -> ok;
blow(#fuse { name = Name }) ->
    ets:insert(?TAB, {Name, blown}),
    fuse_event:notify({Name, blown}).

fix(#fuse { enabled = false }) -> ok;
fix(#fuse { name = Name, ty = TY }) ->
    ets:insert(?TAB, {Name, TY}),
    fuse_event:notify({Name, TY}),
    ok.

delete(#fuse { name = Name }) ->
    ets:delete(?TAB, Name),
    fuse_event:notify({Name, removed}),
    ok.

%% -- TIMERS ----
install_timer(#fuse { enabled = false } = F) -> F;
install_timer(#fuse { name = Name, timer_ref = none, heal_time = HealTime } = F) ->
    TRef = fuse_time:send_after(HealTime, self(), {reset, Name}),
    F#fuse{ timer_ref = TRef };
install_timer(F) ->
    F.

reset_timer(#fuse { timer_ref = none } = F) -> F;
reset_timer(#fuse { timer_ref = TRef } = F) ->
    _ = fuse_time:cancel_timer(TRef),
    F#fuse { timer_ref = none }.

install_metrics(#fuse { name = N }) ->
    StatsPlugin = application:get_env(fuse, stats_plugin, fuse_stats_ets),
    _ = StatsPlugin:init(N),
    ok.
