% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		ackley.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Ackley function

% Initialize
init:-
	dimensions(Dimensions),
	Length is 5 * Dimensions,
	asserta(len(Length)).

% Map interval <0 - 31> to <-15 - 16>
bound(Value, Val):-
	Bounded is Value - 15,
	assertion(Bounded > -16),
	assertion(Bounded < 17),
	Val is Bounded.

% Calculate inner part of sum
inner1(Value, Result):-
	bound(Value, Bounded),
	Result is Bounded ** 2.

inner2(Value, Result):-
	bound(Value, Bounded),
	Cx is 2 * pi * Bounded,
	Result is cos(Cx).

% Calculate fitness value
fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	maplist(inner1, Nums, BeforeSum1),
	maplist(inner2, Nums, BeforeSum2),
	sum_list(BeforeSum1, Sum1),
	sum_list(BeforeSum2, Sum2),
	LB is (-0.2) * sqrt((1/D) * Sum1),
	RB is (1/D) * Sum2,
	!,
	Fitness is (-20 * exp(LB)) - exp(RB) + 20 + exp(1).