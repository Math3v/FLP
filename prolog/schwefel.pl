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

% Compute square
square(X, Sqr):-Sqr is X ** 2.

% Compute sum of prefixes
prefixsum(0, _, []):-!.
prefixsum(Pos, List, [Sum|T]):-
	take(Pos, List, Vals),
	sum_list(Vals, Sum),
	Pos1 is Pos - 1,
	prefixsum(Pos1, List, T).

% Calculate fitness value
fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	maplist(bound, Nums, Bounded),
	length(Bounded, Length),
	prefixsum(Length, Bounded, PrefSums),
	maplist(square, PrefSums, Squares),
	sum_list(Squares, Sum),
	!,
	Fitness is Sum.

