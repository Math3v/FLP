% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		rosenbrock.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Rosenbrock function

% Initialize
init:-
	dimensions(Dimensions),
	assertion(Dimensions >= 2),
	Length is 5 * Dimensions,
	asserta(len(Length)).

% Remove head
remhead([_|T], T).

% Map interval <0 - 31> to <-2.14 - 2.28>
bound(Value, Val):-
	unded is Value - 15,
	assertion(Bounded > -16),
	assertion(Bounded < 17),
	Val is Bounded / 7.

% 
inner(X, Y, Z):-
	Y == 100000 ->
	Z is 0;
	LB is ((X ** 2) - Y) ** 2,
	RB is (X - 1) ** 2,
	Z is 100 * LB + RB.

% Calculate fitness value
fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	maplist(bound, Nums, BNums),
	remhead(BNums, Tmp),
	append(Tmp, [100000], Shifted),
	maplist(inner, BNums, Shifted, Bracket),
	sum_list(Bracket, Fitness), !.