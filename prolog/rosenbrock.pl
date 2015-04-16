% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		rosenbrock.pl
% Author:	Matej Minarik (XMINAR29)
% Include this file in order to use Rosenbrock function

% Initialize
init:-
	dimensions(Dimensions),
	assertion(Dimensions >= 2),
	Length is 10 * Dimensions,
	asserta(len(Length)).

% Remove head
remhead([_|T], T).

% Map interval <0 - 1023> to <-5.12 - 5.12>
bound(Value, Val):-
	Bounded is Value - 512,
	assertion(Bounded > -513),
	assertion(Bounded < 513),
	Val is Bounded / 100.

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