% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		schwefel.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Schwefel function

% Initialize
init:-
	dimensions(Dimensions),
	Length is 10 * Dimensions,
	asserta(len(Length)).

% Map interval <0 - 1023> to <-500 - 500>
bound(Value, Val):-
	(Value > 1000 ->
	Val is 500;
	Val is (Value) - 500),
	assertion(Val > -501),
	assertion(Val < 501).

% Calculate inner part of sum
fitness_inner(X, Res):-
	bound(X, Value),
	Abs is abs(Value),
	Sqr is sqrt(Abs),
	Sin is sin(Sqr),
	Res is Sin * Value.

% Calculate fitness value
fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	maplist(fitness_inner, Nums, Inners),
	sum_list(Inners, Sum),
	!,
	Fitness is (418.9829 * D) - Sum.

schtake(0, _, []):-!.
schtake(Pos, List, [Sum|T]):-
	take(Pos, List, Vals),
	sum_list(Vals, Sum),
	Pos1 is Pos - 1,
	schtake(Pos1, List, T).