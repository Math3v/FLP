% Subject:	FLP Assignment No. 2
% Purpose:	Genetic Algorithm
% File:		ga.pl
% Author:	Matej Minarik (XMINAR29)

% DEBUG: trace. debug.
% Debugging topics: List by list_debug_topics.
% chtonum - Chromosome to number
% evolve - Evolution
% fitness - Fitness function

% Dynamic clauses
:- dynamic uid/1.
:- dynamic chromo/7.

% Utils
b :- reconsult('ga.pl').
s :- write(' ').
l :- write('\n').

% Chromosome length
len(10).

% Chromosome(UID, Ch, Num, Fit, P1, P2, Cpt)
% UID 		Unique IDentifier
% Ch 		Chromosome
% Num 		Chromosome as number
% Fit 		Fitness value
% P1 		Parent 1
% P2 		Parent 2
% CPt 		Crossing point

%chromo(0, [0,0,1,0,1,0,1,1,0,0], _, _, _, _, _).
%chromo(1, [0,1,0,0,0,1,1,1,0,1], _, _, _, _, _).

% Generate unique identifier
uid(0).
next_uid(X) :- uid(Y), 
			X is Y + 1,
			retract(uid(Y)),
			asserta(uid(X)).

% Get random crossing point
crossing_point(CPt) :-
	len(L),
	random(0,L,CPt).

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
cross(P1, P2, P1, P2, 0).
cross(P1, P2, Chld1, Chld2, CPt):-
	split(P1, FP1, BP1, CPt),
	split(P2, FP2, BP2, CPt),
	append(FP1, BP2, Chld1),
	append(FP2, BP1, Chld2).

% Calculate fitness function
% Schwefel function
fitness(Value,Fitness):-
	(Value > 1000 ->
	Val is 500;
	Val is (Value) - 500),
	assertion(Val >= -500),
	assertion(Val =< 500),
	Abs is abs(Val),
	Sqr is sqrt(Abs),
	Sin is sin(Sqr),
	Mul is (Sin) * (Val),
	Fitness is 418.9829 - (Mul),
	(debugging(fitness) ->
		write('Value: '), write(Val), l,
		write('Fitness: '), write(Fitness),l;
		true
	).

% Calculate fitness of population
popfit(PopFitness):-
	findall(F, chromo(_,_,_,F,_,_,_), Fs),
	sum_list(Fs, PopFitness).

divide(Y,X,R):-R is X / Y.

random_list_bounded(Rand, List):-
	max_member(Max, List),
	min_member(Min, List),
	random(Min, Max, Rand).

select(Uid1, Uid2):-
	popfit(PopFitness),
	findall(F, chromo(_,_,_,F,_,_,_), Fs),
	maplist(divide(PopFitness),Fs,Divs),
	random_list_bounded(Rand, Divs),
	(
	write('Divs: '), write(Divs), l,
	write('PopFit: '), write(PopFitness), l,
	write('Rand: '), write(Rand), l),
	include(>=(Rand),Divs,Filtered),
	(write('Include: '), write(Filtered), l),
	sort(Filtered, Sorted),
	length(Sorted, Length),
	%last(Sorted, Last),
	%nth0(0,Filtered,Last),
	nth1(Length, Sorted, Last),
	Length1 is Length - 1,
	nth1(Length1, Sorted, PLast),
	(write('Last: '), write(Last), l),
	(write('PLast: '), write(PLast), l),
	Fitness1 is Last * PopFitness,
	Fitness2 is PLast * PopFitness,
	assertion(member(Fitness1,Fs)),
	assertion(member(Fitness2,Fs)),
	chromo(Uid1,_,_,Fitness1,_,_,_),
	chromo(Uid2,_,_,Fitness2,_,_,_).

select_random(Uid1, Uid2):-
	findall(Uid, chromo(Uid,_,_,_,_,_,_), Uids),
	length(Uids, Length),
	random(1, Length, Rand1),
	random(1, Length, Rand2),
	nth1(Rand1, Uids, Uid1),
	nth1(Rand2, Uids, Uid2).

selection(Uid1, Uid2):-
	select(Uid1, Uid2) ->
	true;
	select_random(Uid1, Uid2).

% Generate chromosome
gen_chromo(Chromo):-
	len(L),
	gen_chromo(Chromo, L).

gen_chromo([], 0).
gen_chromo([H|T], N):-
	(maybe ->
		H is 1 
		;
		H is 0
	),
	N1 is N - 1,
	gen_chromo(T, N1).

% Generate initial population
generate(0).
generate(N):-
	next_uid(Id1),
	next_uid(Id2),
	crossing_point(CPt),
	gen_chromo(Ch1),
	gen_chromo(Ch2),
	chromo_to_num(Ch1, Val1),
	chromo_to_num(Ch2, Val2),
	fitness(Val1, Fit1),
	fitness(Val2, Fit2),
	asserta(chromo(Id1,Ch1,Val1,Fit1,_,_,CPt)),
	asserta(chromo(Id2,Ch2,Val2,Fit2,_,_,CPt)),
	N1 is N - 1,
	generate(N1).

% Testing evolve function
evolve:-
	crossing_point(Cpt),
	chromo(0, Ch1, _, _, _, _, _),
	chromo(1, Ch2, _, _, _, _, _),
	chromo_to_num(Ch1, Val1),
	chromo_to_num(Ch2, Val2),
	fitness(Val1, Fit1),
	fitness(Val2, Fit2),
	cross(Ch1, Ch2, Chld1, Chld2, Cpt),
	(
	debugging(evolve) -> 
	write('Crossing point: '), write(Cpt), l,
	write('Value 1: '), write(Val1), l,
	write('Value 2: '), write(Val2), l,
	write('Fitness 1: '), write(Fit1), l,
	write('Fitness 2: '), write(Fit2), l,
	write('Child 1: '), write(Chld1), l,
	write('Child 2: '), write(Chld2), l;
	true
	).