%%% @doc Erlang QuickCheck library functions
%%% Kept as one big module for ease of development.
%%% @end
-module(eqc_lib).
-vsn("1.3.0").
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%%% BIT INTEGERS
%%% ---------------------------------------------------------------
%%%

%% @doc pow_2_int/0 generates integers close to a power of two
%% It turns out that integers around powers of two are often able to mess up stuff
%% because of their bitwise representation. This generator generates integers close
%% to a power of two deliberately.
%% @end
pow_2_int() ->
    ?LET({Sign, Exponent, Perturb}, {sign(), choose(0, 128), choose(-3, 3)},
        Sign * pow(2, Exponent) + Perturb).

sign() -> elements([1, -1]).

pow(0, 0) -> 0;
pow(_Base, 0) -> 1;
pow(Base, N) -> Base * pow(Base, N-1).

%%% HEX STRING
%%% ---------------------------------------------------------------

%% @doc hex_char() generates a hexadecimal character
%% @end
hex_char() ->
    elements([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $a, $b, $c, $d, $e, $f]).

%% @doc hex_string/0 generates a hex string
%% @end
hex_string() -> list(hex_char()).

%% @doc hex_string/1 generates a hexadecimal string of length `N'
%% @end
hex_string(N) ->
    vector(N, hex_char()).

%%% UUID
%%% ---------------------------------------------------------------

%% @doc uuid_v4() generates a v4 UUID
%% @end
uuid_v4() ->
    ?LET(
        {S1, S2, S3, S4, S5},
        {hex_string(8), hex_string(4), hex_string(3), hex_string(3), hex_string(12)},
            iolist_to_binary([S1, $-, S2, $-, $4, S3, $-, $a, S4, $-, S5])).

%%% SORTING
%%% ---------------------------------------------------------------
%%%

%% @doc sort/1 is a total sort function
%% The built-in lists:sort/1 is not total, because 0 == 0.0. Since the sort function
%% is also *stable* it can't be used to force a unique order on terms. This variant
%% of sort has the property of total order with INTEGER < FLOAT.
%% @end
sort(L) ->
    lists:sort(fun(X, Y) -> erts_internal:cmp_term(X, Y) < 0 end, L).

prop_sorted() ->
    ?FORALL(L, maps_eqc:map_list(),
        begin
            Sorted = sort(L),
            conjunction([
              {size, equals(length(L), length(Sorted))},
              {ordering, ordered(Sorted)}
            ])
        end).
        
ordered([]) -> true;
ordered([_]) -> true;
ordered([X,Y|T]) ->
    case cmp_term(X,Y) of
        true -> ordered([X|T]);
        false -> false
    end.

%% The following implement term comparison in Erlang to test an alternative implementation
%% of erts_internal:cmp_term/2 
cmp_term(T1, T2) when is_integer(T1), is_integer(T2) -> T1 < T2;
cmp_term(T1, _) when is_integer(T1) -> true;
cmp_term(T1, T2) when is_float(T1), is_float(T2) -> T1 < T2;
cmp_term(T1, _) when is_float(T1) -> true;
cmp_term(T1, T2) when is_atom(T1), is_atom(T2) -> T1 < T2;
cmp_term(T1, _) when is_atom(T1) -> true;
cmp_term(T1, T2) when is_reference(T1), is_reference(T2) -> T1 < T2;
cmp_term(T1, _) when is_reference(T1) -> true;
cmp_term(T1, T2) when is_function(T1), is_function(T2) -> T1 < T2;
cmp_term(T1, _) when is_function(T1) -> true;
cmp_term(T1, T2) when is_port(T1), is_port(T2) -> T1 < T2;
cmp_term(T1, _) when is_port(T1) -> true;
cmp_term(T1, T2) when is_pid(T1), is_pid(T2) -> T1 < T2;
cmp_term(T1, _) when is_pid(T1) -> true;
cmp_term(T1, T2) when is_tuple(T1), is_tuple(T2) -> cmp_term(tuple_to_list(T1), tuple_to_list(T2));
cmp_term(T1, _) when is_tuple(T1) -> true;
cmp_term(T1, T2) when is_list(T1), is_list(T2) -> cmp_term_list(T1, T2);
cmp_term(T1, _) when is_list(T1) -> true;
cmp_term(T1, T2) when is_bitstring(T1), is_bitstring(T2) -> T1 < T2;
cmp_term(_, _) -> false.

cmp_term_list([], []) -> false;
cmp_term_list([], _) -> true;
cmp_term_list(_, []) -> false;
cmp_term_list([X|Xs], [Y|Ys]) when X =:= Y -> cmp_term_list(Xs, Ys);
cmp_term_list([X|_], [Y|_]) -> cmp_term(X, Y).


%% STEM AND LEAF PLOTS
%% ------------------------------------------------------
%%
%% If you are collecting lots of values, you may often want to show the distribution of those
%% values. A stem & leaf plot allows you to handle this easily. Use it like you would use the
%% with_title/1 printer:
%%
%% collect(stem_and_leaf('Command Length'), length(Cmds), …)
%%
stem_and_leaf(Title) ->
  fun(Counts) ->
    io:format("~s", [
    [atom_to_list(Title), $\n, $\n,
     "Stem | Leaf\n",
     "----------------\n",
     (out_stem_and_leaf(stem_and_leaf_collect(Counts, #{})))]])
  end.
    
stem_and_leaf_collect([{C, 1}|Cs], Bins) ->
    stem_and_leaf_collect(Cs, store_bin(C div 10, C rem 10, Bins));
stem_and_leaf_collect([{C, K} | Cs], Bins) ->
    stem_and_leaf_collect([{C, K-1} | Cs], store_bin(C div 10, C rem 10, Bins));
stem_and_leaf_collect([], Bins) -> Bins.

store_bin(D, R, Bins) ->
    case maps:find(D, Bins) of
        {ok, L} -> maps:put(D, [R | L], Bins);
        error -> maps:put(D, [R], Bins)
    end.

out_stem_and_leaf(Bins) ->
    out_sl(lists:sort(maps:to_list(Bins))).
    
out_sl([]) -> [];
out_sl([{C, Elems} | Next]) ->
    Line = io_lib:format("~4.B | ~ts~n", [C, leaves(lists:sort(Elems))]),
    [Line | out_sl(Next)].

leaves([E | Es] = Elems) when length(Elems) > 66 -> ["*** ", rle(Es, E, 1)];
leaves(Elems) ->
    [E + $0 || E <- Elems].

rle([E | Es], E, Cnt) ->
    rle(Es, E, Cnt+1);
rle([Ez | Es], E, Cnt) ->
    [rle_out(E, Cnt), " " | rle(Es, Ez, 1)];
rle([], E, Cnt) ->
    [rle_out(E, Cnt)].

rle_out(E, Cnt) ->
    [integer_to_list(E), <<"·("/utf8>>, integer_to_list(Cnt), ")"].

%% SUMMARY PLOTS
%% ------------------------------------------------------
%%
%% Summarize a data set like in R
%%
summary(Title) ->
  fun(Values) ->
    Stats = summary_stats(Values),
    Out = [atom_to_list(Title), $\n,
        "Min.   :", summary_stats(min, Stats), $\n,
        "1st Qr.:", summary_percentile(25, Stats), $\n,
        "Median.:", summary_percentile(50, Stats), $\n,
        "Mean.  :", summary_stats(mean, Stats), $\n,
        "3rd Qr.:", summary_percentile(75, Stats), $\n,
        "Max.   :", summary_stats(max, Stats), $\n
    ],
    io:format("~s", [Out])
  end.

summary_stats(Name, Stats) ->
    case maps:get(Name, Stats) of
        I when is_integer(I) -> integer_to_list(I);
        F when is_float(F) -> float_to_list(F, [{decimals, 6}, compact])
    end.

summary_percentile(N, Stats) ->
    case maps:get({percentile, N}, Stats) of
        I when is_integer(I) -> integer_to_list(I);
        F when is_float(F) -> float_to_list(F, [{decimals, 6}, compact])
    end.

summary_expand(Values) ->
    lists:flatten([lists:duplicate(N, Elem) || {Elem, N} <- Values]).

summary_stats(RLEs) ->
    summary_stats_(lists:sort(RLEs)).

summary_stats_([]) ->
    #{ min => na, max => na, {percentile, 25} => na, {percentile, 50} => na,
      {percentile, 75} => na, mean => na, n => 0 };
summary_stats_([{E, EC} | RLEs] = Values) ->
    {Min, Max, Mean, N} = summary_scan(E, E, EC, E*EC, RLEs),
    #{ min => Min, max => Max, n => N, mean => Mean,
      {percentile, 25} => percentile(Values, N, 25),
      {percentile, 50} => percentile(Values, N, 50),
      {percentile, 75} => percentile(Values, N, 75)
    }.

summary_scan(Min, Max, N, Sum, []) -> {Min, Max, Sum/N, N};
summary_scan(Min, Max, N, Sum, [{E, Count} | RLEs]) ->
   summary_scan(
      min(E, Min),
      max(E, Max),
      N + Count,
      Sum + E*Count,
      RLEs).

percentile(RLE, N, Pct) ->
    percentile_pick(RLE, perc(Pct, N)).
    
percentile_pick([{E, N} | _RLEs], ToSkip) when ToSkip =< N -> E;
percentile_pick([{_E, N} | RLEs], ToSkip) ->
    percentile_pick(RLEs, ToSkip - N).
    
perc(P, Len) ->
    V = round(P * Len / 100),
    erlang:max(1, V).

%% TRACKER PROCESS
%% The tracker process can be used to track a state outside the EQC state
reset(Name) ->
    case whereis(Name) of
        undefined ->
          Pid = spawn_link(fun() -> tracker_loop(undefined) end),
          register(Name, Pid),
          ok;
        P when is_pid(P) ->
          P ! reset,
          ok
    end.

bind(Name, Fun) ->
    Name ! {get_state, self()},
    receive
      {state, S} ->
        case Fun(S) of
          {ok, R, S} -> R; % Optimize the case where there is no change
          {ok, R, N} ->
            Name ! {set_state, N},
            R
        end
    after 5000 ->
        exit(timeout)
    end.

tracker_loop(S) ->
    receive
      reset -> ?MODULE:tracker_loop(undefined);
      stop -> ok;
      {get_state, From} ->
          From ! {state, S},
          ?MODULE:tracker_loop(S);
      {set_state, N} ->
          ?MODULE:tracker_loop(N)
    end.
