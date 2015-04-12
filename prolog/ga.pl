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

%chromo(_, [0,0,1,0], _, _, _, _, _).

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