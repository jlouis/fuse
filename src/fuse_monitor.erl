%%% @doc fuse_monitor monitors the fuse state and reports it
%%% @private
-module(fuse_monitor).
-behaviour(gen_server).

-ifdef(PULSE).
-include_lib("pulse_otp/include/pulse_otp.hrl").
-endif.

-define(TAB, fuse_state).

%% Lifetime
-export([start_link/0]).

%% API
-export([sync/0]).

%% Callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {
    alarms = [],
    history = []
}).

-define(PERIOD, 60*1000).

%% Lifetime
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API
%% @private
sync() ->
    gen_server:call(?MODULE, sync).

%% Callbacks

%% @private
init([]) ->
    S = #state {},
    {ok, set_timer(S)}.

%% @private
handle_call(sync, _F, State) ->
    {reply, ok, State};
handle_call(_M, _F, State) ->
    {reply, {error, unknown}, State}.

%% @private
handle_cast(_M, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, State) ->
    NewState = handle_fuses(State),
    {noreply, set_timer(NewState)};
handle_info(_M, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal API
handle_fuses(#state { history = Hist } = State) ->
    Fuses = ets:match_object(?TAB, '_'),
    TrackedHistory = track_histories(Fuses, Hist),
    {AlarmsToChange, UpdatedState} = analyze(State#state { history = TrackedHistory }),
    process_alarms(AlarmsToChange),
    UpdatedState.

%% process_alarms/2 raises and clears appropriate alarms
process_alarms([{A, set} | As]) -> alarm_handler:set_alarm({A, fuse_blown}), process_alarms(As);
process_alarms([{A, clear} | As]) -> alarm_handler:clear_alarm(A), process_alarms(As);
process_alarms([]) -> ok.

track_histories(Fuses, Hist) ->
    lists:foldl(fun update/2, Hist, Fuses).

update({Name, blown}, Hist) ->
    E = {Name, 3},
    [ E | lists:keydelete(Name, 1, Hist) ];
update({Name, _}, Hist) ->
    case lists:keytake(Name, 1, Hist) of
        false -> Hist;
        {value, {Name, V}, Remain} -> [{Name, clamp(0, V-1, 3)} | Remain]
    end.

clamp(Lo, V, _Hi) when V < Lo -> Lo;
clamp(_Lo, V, Hi) when V > Hi -> Hi;
clamp(_Lo, V, _Hi) -> V.

analyze(#state { history = Hs, alarms = CurAlarms } = S) ->
    {ToClear, ToKeep} = lists:partition(fun({_, V}) -> V == 0 end, Hs),
    {Clear, Set} = {names(ToClear), names(ToKeep)},
    NewAlarms = Set -- CurAlarms,
    PruneAlarms = intersect(CurAlarms, Clear),
    ChangeList = [{A, set} || A <- NewAlarms] ++ [{A, clear} || A <- PruneAlarms],
    %% Note we sort the alarm change list here. This forces alarm changes in a certain order
    %% which makes it easier to reason about the alarm changes in an EQC model
    { lists:sort(ChangeList),
      S#state { history = ToKeep, alarms = (CurAlarms ++ NewAlarms) -- PruneAlarms }}.

names(Xs) -> [element(1, X) || X <- Xs].

set_timer(#state { } = S) ->
    fuse_time:send_after(?PERIOD, self(), timeout),
    S.

intersect([A | As], Others) ->
    case lists:member(A, Others) of
        true -> [A | intersect(As, Others)];
        false -> intersect(As, Others)
    end;
intersect([], _) -> [].
