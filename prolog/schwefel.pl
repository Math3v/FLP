% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		schwefel.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Schwefel function

% Chromosome length
dimensions(5).
len(50).

% Calculate fitness function
% Schwefel function
bound(Value, Val):-
	(Value > 1000 ->
	Val is 500;
	Val is (Value) - 500),
	assertion(Val > -501),
	assertion(Val < 501).

fitness_inner(X, Res):-
	bound(X, Value),
	Abs is abs(Value),
	Sqr is sqrt(Abs),
	Sin is sin(Sqr),
	Res is Sin * Value.

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
