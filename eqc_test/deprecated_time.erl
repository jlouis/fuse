-module(deprecated_time).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


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
   default(0, choose(0,999)).
	
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
