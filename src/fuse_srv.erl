%%% @doc Runs the fuse server in the system
%%% @private
-module(fuse_srv).
-behaviour(gen_server).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

%% Lifetime API
-export([start_link/1]).

%% Operational API
-export([install/2, ask/1, reset/1, melt/2]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%% Private
-export([sync/0]).

-define(TAB, fuse_state).

-record(state, { fuses = [], timing = automatic }).
-record(fuse, {
	name :: atom(),
	intensity :: integer(),
	period :: integer(),
	heal_time :: integer(),
	melt_history = [],
        timer_ref = none
}).


%% ------
%% @doc Start up the manager server for the fuse system
%% This is assumed to be called by (@see fuse_sup). The `Timing' parameter controls how the system manages timing.
%% @end
start_link(Timing) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Timing], []).

%% ------
%% @doc install/2 installs a new fuse into the running system
%% Install a new fuse under `Name' with options given by `Opts'.
%% We assume `Opts' are already in the right verified and validated format.
%% @end
install(Name, Opts) ->
	%% Assume options are already verified
	Fuse = init_state(Name, Opts),
	gen_server:call(?MODULE, {install, Fuse}).

%% @doc ask/1 asks about the current given fuse state
%% The documentation is (@see fuse:ask/2)
%% @end
-spec ask(atom()) -> ok | blown | {error, not_found}.
ask(Name) ->
    try ets:lookup_element(?TAB, Name, 2) of
        ok -> ok;
        blown -> blown
    catch
        error:badarg ->
            {error, not_found}
    end.

%% @doc reset/1 resets the fuse
%% The documentation is (@see fuse:ask/2)
%% @end
-spec reset(atom()) -> ok | {error, not_found}.
reset(Name) ->
	gen_server:call(?MODULE, {reset, Name}).
   
%% @doc melt/2 melts the fuse at a given point in time
%% For documentation, (@see fuse:melt/2)
%% @end
-spec melt(Name, Ts) -> ok
    when Name :: atom(), Ts :: erlang:timestamp().
melt(Name, Ts) ->
	gen_server:call(?MODULE, {melt, Name, Ts}).
    
%% sync/0 syncs the fuse_srv. For internal use only in tests
%% @private
sync() ->
    gen_server:call(?MODULE, sync).

%% @private
init([Timing]) when Timing == manual; Timing == automatic ->
	_ = ets:new(?TAB, [named_table, protected, set, {read_concurrency, true}, {keypos, 1}]),
	{ok, #state{ timing = Timing }}.

%% @private
handle_call({install, #fuse { name = Name } = Fuse}, _From, #state { fuses = Fs } = State) ->
        case lists:keytake(Name, #fuse.name, Fs) of
            false ->
                fix(Fuse);
            {value, OldFuse, _Otherfuses} ->
                fix(OldFuse),
                reset_timer(OldFuse)
        end,
        {reply, ok, State#state { fuses = lists:keystore(Name, #fuse.name, Fs, Fuse)}};
handle_call({reset, Name}, _From, State) ->
	{Reply, State2} = handle_reset(Name, State, reset),
	{reply, Reply, State2};
handle_call({melt, Name, Now}, _From, State) ->
	{Res, State2} = with_fuse(Name, State, fun(F) -> add_restart(Now, F, State) end),
	case Res of
	  ok -> {reply, ok, State2};
	  not_found -> {reply, ok, State2}
	end;
handle_call(sync, _F, State) ->
	{reply, ok, State};
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
	Reset = fun(F) ->
            case ResetType of
                reset ->
                    fix(F),
                    NewF = reset_timer(F),
                    {ok, NewF#fuse { melt_history = [] }};
                timeout ->
                    fix(F),
                    {ok, F#fuse { melt_history = [], timer_ref = none }}
            end
	end,
	{Res, State2} = with_fuse(Name, State, Reset),
	case Res of
	  ok -> {ok, State2};
	  not_found -> {{error, not_found}, State2}
	end.

init_state(Name, {{standard, MaxR, MaxT}, {reset, Reset}}) ->
    #fuse { name = Name, intensity = MaxR, period = MaxT, heal_time = Reset }.

with_fuse(Name, #state { fuses = Fs} = State, Fun) ->
    case lists:keytake(Name, #fuse.name, Fs) of
        false -> {not_found, State};
        {value, F, OtherFs} ->
            {R, FF} = Fun(F),
            {R, State#state { fuses = [FF | OtherFs] }}
    end.

add_restart(Now, #fuse { intensity = I, period = Period, melt_history = R, heal_time = Heal, name = Name } = Fuse, #state{} = S) ->
    R1 = add_restart_([Now | R], Now, Period),
    NewF = Fuse#fuse { melt_history = R1 },
    case length(R1) of
        CurI when CurI =< I ->
            {ok, NewF};
            _ ->
              blow(Fuse),
              TRef = add_reset_timer(Name, S, Heal),
              {ok, NewF#fuse { timer_ref = TRef }}
    end.


add_restart_([R|Restarts], Now, Period) ->
    case in_period(R, Now, Period) of
        true ->
            [R|add_restart_(Restarts, Now, Period)];
        false ->
            []
    end;
add_restart_([], _, _) ->
    [].
    
in_period(Time, Now, Period) ->
    case difference(Time, Now) of
        T when T > Period -> false;
        _ -> true
    end.

%%
%% Time = {MegaSecs, Secs, MicroSecs} (NOTE: MicroSecs is ignored)
%% Calculate the time elapsed in seconds between two timestamps.
%% If MegaSecs is equal just subtract Secs.
%% Else calculate the Mega difference and add the Secs difference,
%% note that Secs difference can be negative, e.g.
%%      {827, 999999, 676} diff {828, 1, 653753} == > 2 secs.
%%
difference({TimeM, TimeS, _}, {CurM, CurS, _}) when CurM > TimeM ->
    ((CurM - TimeM) * 1000000) + (CurS - TimeS);
difference({_, TimeS, _}, {_, CurS, _}) ->
    CurS - TimeS.

blow(#fuse { name = Name }) ->
    ets:insert(?TAB, {Name, blown}).

fix(#fuse { name = Name }) ->
    ets:insert(?TAB, {Name, ok}).

reset_timer(#fuse { timer_ref = none } = F) -> F;
reset_timer(#fuse { timer_ref = TRef } = F) ->
    erlang:cancel_timer(TRef),
    F#fuse { timer_ref = none }.

add_reset_timer(_Name, #state { timing = manual }, _HealTime) -> none;
add_reset_timer(Name, #state { timing = automatic}, HealTime) ->
    erlang:send_after(HealTime, self(), {reset, Name}).
