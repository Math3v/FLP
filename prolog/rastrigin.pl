% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		rastrigin.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Rastrigin function

% Initialize
init:-
	dimensions(Dimensions),
	Length is 5 * Dimensions,
	asserta(len(Length)).

% Map interval <0 - 31> to <-2.14 - 2.28>
bound(Value, Val):-
	unded is Value - 15,
	assertion(Bounded > -16),
	assertion(Bounded < 17),
	Val is Bounded / 7.

% Calculate inner part of sum
fitness_inner(X, Res):-
	bound(X, Value),
	Sqr is Value ** 2,
	Bkt is 2 * pi * Value,
	Cos is cos(Bkt),
	Res is Sqr - (10 * Cos) + 10.

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
	Fitness is Sum.