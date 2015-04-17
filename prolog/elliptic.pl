% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		elliptic.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Elliptic function

% Initialize
init:-
	dimensions(Dimensions),
	Length is 10 * Dimensions,
	asserta(len(Length)).

% Map interval <0 - 1023> to <-51.2 - 51.2>
bound(Value, Val):-
	Bounded is Value - 512,
	assertion(Bounded > -513),
	assertion(Bounded < 513),
	Val is Bounded / 10.

% Generate indices
% Need to be reversed
genidx(1, [1]):-!.
genidx(Length, [Length|Res]):-
	Length1 is Length - 1,
	genidx(Length1, Res).

% Calculate inner part of sum
% D - dimensions
% X - actual value
% I - index
fitness_inner(D, X, I, Res):-
	bound(X, Val),
	Sqr is Val ** 2,
	Pow is (I - 1)/(D - 1),
	Res is ((10 ** 6) ** Pow) * Sqr.

% Calculate fitness value
fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	length(Nums, Length),
	genidx(Length, RIndices),
	reverse(RIndices, Indices),
	maplist(fitness_inner(D), Nums, Indices, Inners),
	sum_list(Inners, Sum),
	!,
	Fitness is Sum.