% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		ga.pl
% Author:	Matej Minarik (XMINAR29)

% DEBUG: trace. debug.
% Debugging topics:
% chtonum - Chromosome to number

% Dynamic clauses
:- dynamic uid/1.

% Utils
build :- reconsult('ga.pl').
s :- write(' ').
l :- write('\n').

% Chromosome(UID, Ch, Num, Fit, P1, P2, Cpt)
% UID 		Unique IDentifier
% Ch 		Chromosome
% Num 		Chromosome as number
% Fit 		Fitness value
% P1 		Parent 1
% P2 		Parent 2
% CPt 		Crossing point

chromo(0, [0,0,1,0,1,0,1,1,1,0,1], _, _, _, _, _).
chromo(1, [0,1,0,0,0,1,1,1,1,0,0], _, _, _, _, _).

% Generate unique identifier
uid(0).
next_uid(X) :- uid(Y), 
			X is Y + 1,
			retract(uid(Y)),
			asserta(uid(X)).

% Get random crossing point
crossing_point(CPt) :-
	chromo(_, C, _, _, _, _, _),
	length(C, M),
	random(0,M,CPt).

% Chromosome to number
chromo_to_num([], 0).
chromo_to_num([H|T], Number) :-
	length([H|T], Length),
	chromo_to_num([H|T], Length, 0, Number).

chromo_to_num(_, 0, Number, Number).
chromo_to_num([H|T], Length, BNumber, Number) :-
	Power is 2^(Length - 1),
	Val is Power * H,
	Number1 is (BNumber + Val),
	Length1 is Length - 1,
	(	debugging(chtonum)
	-> write('Value '), write(Val), s, 
		write('Number '), write(Number1), s, 
		write('Power '), write(Power), l
	; true
	),
	chromo_to_num(T, Length1, Number1, Number).

% Split chromosome at location loci
% WARNING:	if Loci == length(List) then this will fail
%				BUT: crossing point should not generate this case
%			if Loci == 0 then List is in the second variable
split([H|T], [], [H|T], 0).
split([H|T],[H|Front],Back,Loci):-
	(
	length([H|T], L),
	assertion(Loci \= L)
	),
	Loci1 is Loci - 1,
	split(T,Front,Back,Loci1).

% Crossing
cross(P1, P2, Chld1, Chld2, CPt):-
	split(P1, FP1, BP1, CPt),
	split(P2, FP2, BP2, CPt),
	append(FP1, BP2, Chld1),
	append(FP2, BP1, Chld2).

% Calculate fitness function
% Schwefel function
fitness(Value,Fitness):-
	assertion(Value > -500),
	assertion(Value < 500),
	Abs is abs(Value),
	Sqr is sqrt(Abs),
	Sin is sin(Sqr),
	Mul is Sin * Value,
	Fitness is 418.9829 - Mul.
