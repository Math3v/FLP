% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		rosenbrock.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Rosenbrock function

% Chromosome length
dimensions(2).
len(20).

% Map interval <0 - 1023> to <-5.12 - 5.12>
bound(Value, Val):-
	Bounded is Value - 512,
	assertion(Bounded > -513),
	assertion(Bounded < 513),
	Val is Bounded / 100.

% Calculate inner part of sum
split_xy(List, First, Second):-
	nth0(0, List, First),
	nth0(1, List, Second).

% Calculate fitness value
fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	maplist(bound, Nums, BNums),
	split_xy(BNums, X, Y),
	!,
	XP is (1 - X ** 2),
	YP is (Y - X ** 2) ** 2,
	Fitness is XP + 100 * YP.