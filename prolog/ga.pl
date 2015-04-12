% FLP Assignment No. 2
% Genetic Algorithm
% ga.pl
% Matej Minarik (XMINAR29)

% Dynamic clauses
:- dynamic uid/1.

% Unique IDentifier need to be only one
%retractall(uid(_)).

% Simple clause to reconsult current file
build:-reconsult('ga.pl').

% Generate unique identifier
uid(0).
get_uid(X):-uid(Y), 
			X is Y + 1,
			retract(uid(Y)),
			asserta(uid(X)).

% Chromosome to number
chromo_to_num([], 0).
chromo_to_num([H|T], Number):-
	length([H|T], Length),
	chromo_to_num([H|T], Length, 0, Number).

chromo_to_num(_, 0, Number, Number).
chromo_to_num([H|T], Length, BNumber, Number):-
	Power is 2^(Length - 1),
	Val is Power * H,
	Number1 is (BNumber + Val),
	Length1 is Length - 1,
	chromo_to_num(T, Length1, Number1, Number).