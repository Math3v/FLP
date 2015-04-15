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
:- dynamic pop/1.
:- dynamic mut/1.
:- dynamic chromo/7.

% Utils
b :- reconsult('ga.pl').
s :- write(' ').
l :- write('\n').

% Chromosome length
dimensions(10).
len(100).

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

% Population counter
pop(0).
inc_pop:-
	pop(Y),
	X is Y + 1,
	retract(pop(Y)),
	asserta(pop(X)).

% Population counter
mut(0).
inc_mut:-
	mut(Y),
	X is Y + 1,
	retract(mut(Y)),
	asserta(mut(X)).

% Float to integer
to_int(Float, Int):- Int is round(Float).

% Little hack
move(X,X).

% Split lsit to list of lists
split(N, List, [Front|Rest]):-
	length(List, Len),
	(Len == N) ->
	(move(List, Front),true);
	(split(List, Front, Back, N),
	split(N, Back, Rest)).

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
fitness__(Value,Fitness):-
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

bound(Value, Val):-
	(Value > 1000 ->
	Val is 500;
	Val is (Value) - 500),
	assertion(Val > -501),
	assertion(Val < 501).

fitness_inner(X, Res):-
	bound(X, Value),
	Abs is abs(Value),
	Sqr is sqrt(Abs),
	Sin is sin(Sqr),
	Res is Sin * Value.

fitness(Chromo, Fitness):-
	dimensions(D),
	len(L),
	N is L / D,
	split(N, Chromo, Chromos),
	maplist(chromo_to_num, Chromos, Nums),
	%write('Nums: '), write(Nums), l,
	maplist(fitness_inner, Nums, Inners),
	%write('Inners: '), write(Inners), l,
	sum_list(Inners, Sum),
	!,
	Fitness is (418.9829 * D) - Sum.

test(Fitness):-
	gen_chromo(Ch),
	write('Chromo: '), write(Ch), l,
	fitness(Ch, Fitness).

% Calculate fitness of population
popfit(PopFitness):-
	findall(F, chromo(_,_,_,F,_,_,_), Fs),
	sum_list(Fs, PopFitness).

% Get id of element in list
index_of(Elem, [Elem|_], 0):- !.
index_of(Elem, [_|T], Index1):-
	index_of(Elem, T, Index),
	Index1 is Index + 1.

% Division as predicate for maplist function
divide(Y,X,R):-R is X / Y.

% Invert probability
invert_probability(Prob, New):- New is 1 - Prob.

% Get random number bounded by list
random_list_bounded(Rand, List):-
	max_member(Max, List),
	min_member(Min, List),
	random(Min, Max, Rand).

% Select chromosomes IDs by roulette
select(Uid1, Uid2):-
	popfit(PopFitness),
	findall(F, chromo(_,_,_,F,_,_,_), Fs),
	maplist(divide(PopFitness),Fs,Divs1),
	maplist(invert_probability,Divs1,Divs),
	random_list_bounded(Rand, Divs),
	include(>=(Rand),Divs,Filtered),
	sort(Filtered, Sorted),
	length(Sorted, Length),
	nth1(Length, Sorted, Last),
	Length1 is Length - 1,
	nth1(Length1, Sorted, PLast),
	index_of(Last, Divs, ILast),
	index_of(PLast, Divs, IPLast),
	nth0(ILast, Fs, Fitness1),
	nth0(IPLast, Fs, Fitness2),
	chromo(Uid1,_,_,Fitness1,_,_,_),
	chromo(Uid2,_,_,Fitness2,_,_,_).

% If roulette fails, select random chromosomes
select_random(Uid1, Uid2):-
	findall(Uid, chromo(Uid,_,_,_,_,_,_), Uids),
	length(Uids, Length),
	random(1, Length, Rand1),
	random(1, Length, Rand2),
	nth1(Rand1, Uids, Uid1),
	nth1(Rand2, Uids, Uid2).

% Selection wrapper
selection(Uid1, Uid2):-
	select(Uid1, Uid2) ->
	true;
	select_random(Uid1, Uid2).

% Mutation
mutate:-
	random(Mut),
	(Mut < 0.10) ->
	(
	findall(Uid, chromo(Uid,_,_,_,_,_,_), Uids),
	random_member(RUid, Uids),
	chromo(RUid, Ch, Val, Fit, _, _, CPt),
	random_permutation(Ch, Ch1),
	retract(chromo(RUid, _, _, _, _, _, _)),
	asserta(chromo(RUid, Ch1, Val, Fit, _, _, CPt)),
	inc_mut
	)
	;
	(true).
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

% Max fitness
max_fit(Fitness):-
	findall(Fit, chromo(_,_,_,Fit,_,_,_), Fits),
	max_member(Fitness, Fits).

% Min fitness
min_fit(Fitness):-
	findall(Fit, chromo(_,_,_,Fit,_,_,_), Fits),
	min_member(Fitness, Fits).

% Find value by fitness
val_by_fit(Fitness, Value):-
	chromo(_,_,Value,Fitness,_,_,_).


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
	fitness(Ch1, Fit1),
	fitness(Ch2, Fit2),
	asserta(chromo(Id1,Ch1,Val1,Fit1,_,_,CPt)),
	asserta(chromo(Id2,Ch2,Val2,Fit2,_,_,CPt)),
	N1 is N - 1,
	generate(N1).

% Evolve
evolve:-
	crossing_point(CPt),
	selection(Uid1, Uid2),
	chromo(Uid1, Ch1, _, Fit1, _, _, _),
	chromo(Uid2, Ch2, _, Fit2, _, _, _),
	cross(Ch1, Ch2, Chld1, Chld2, CPt),
	chromo_to_num(Chld1, ChldVal1),
	chromo_to_num(Chld2, ChldVal2),
	fitness(Chld1, ChldFit1),
	fitness(Chld2, ChldFit2),
	((ChldFit1 < Fit1) ->
		(
		retract(chromo(Uid1, _, _, _, _, _, _)),
		next_uid(Uid3),
		asserta(chromo(Uid3, Chld1, ChldVal1, ChldFit1, _, _, CPt))
		);
		true),
	((ChldFit2 < Fit2) ->
		(
		retract(chromo(Uid2, _, _, _, _, _, _)),
		next_uid(Uid4),
		asserta(chromo(Uid4, Chld2, ChldVal2, ChldFit2, _, _, CPt))
		);
		true),
	min_fit(FMinFit),
	val_by_fit(FMinFit, Value),
	to_int(FMinFit, IMinFit),
	(IMinFit == 0) ->
	(pop(PopNo),
	(write('Population number: '), write(PopNo), l),
	mut(MutNo),
	(write('Mutation number: '), write(MutNo), l),
	(write('Value: '), write(Value), l)),
	fail
	;
	(mutate,
	inc_pop,
	evolve).