% COMPILE WITH:  swipl -O -q -o here -c a.pl

% then run final('testcase.txt', Res).




:- use_module(library(thread)).	%concurrent 


%read_from_file
read_from_file(File, Lines) :-
	 open(File, read, Stream),
	 read_params(Stream, [N]),
	 read_lines(Stream, N, Lines).

%read many integers from line
read_params(Stream , Params) :-
	 read_line_to_codes(Stream, Line),
	 atom_codes(Atom, Line),
	 atomic_list_concat(Atoms, ' ', Atom),
	 concurrent_maplist(atom_number, Atoms, Params).

%read N lines
read_lines(_, 0, []) :- !.
read_lines(Stream, Counter, [X|L]) :-   
	%read
	read_params(Stream, X),
	%recurse
	New_counter is Counter-1,
	read_lines(Stream, New_counter, L).

% END OF IO %




% Res = Base ^ Exp % Mod
fastpow(Base, Exp, Mod, Res) :-
	Base_m is Base mod Mod,
	fastpow_(Base_m, Exp, Mod, 1, Res).

% helping pred
fastpow_(_, 0, _, Acc, Acc) :- !.	%dont look further
fastpow_(Base, Exp, Mod, Acc, Res) :-
	New_b is (Base * Base) mod Mod,
	New_e is Exp >> 1,
	Parity is Exp /\ 1,
	(
		Parity =:= 0 -> 
			New_a is Acc 
		;
			New_a is (Acc * Base) mod Mod
	),
	fastpow_(New_b, New_e, Mod, New_a, Res).


% nCk % p
fermat_binom(N, K, P, Res) :-
	get_num(N, K, P, Num),
	get_den(K, P, Den),
	P_2 is P - 2,
	fastpow(Den, P_2, P, Fast_pow),
	Res is (Num * Fast_pow) mod P.



%product of num and den
get_num(N, K, P, Res) :-
	From is N - K + 1,
	par_range(From, N, P, Res).

get_den(K, P, Res) :-
	par_range(1, K, P, Res).


par_range(From, To, P, Res) :-
	% split product range into 4
	Half is (From + To) >> 1,
	Low  is (From + Half) >> 1,
	High is (Half + To) >> 1,
	Upper is To + 1,

	% Limits of ranges in list for concurrent_maplist to apply onto
	Limits = [[From, Low], [Low, Half], [Half, High], [High, Upper]],

	% compure concurrently 
	concurrent_maplist(comp_range_loop, Limits, [P,P,P,P], [Res1, Res2, Res3, Res4]),

	% merge two lower and two upper areas
	Merge1 is (Res1 * Res2) mod P,
	Merge2 is (Res3 * Res4) mod P,
	%merge together
	Res is (Merge1 * Merge2) mod P.


% like range_loop but I compress (From, To) to list [From, To]
% to use concurrent_maplist/4. cause there is not concurrent_maplist/5.
comp_range_loop([From, To], P, Res) :-
	range_loop(From, To, 1, P, Res).

% product loop
range_loop(To, To, Acc, _, Acc) :- !.
range_loop(From, To, Acc, P, Res) :-
	New_from is From + 1,
	New_acc  is (From * Acc) mod P,
	range_loop(New_from, To, New_acc, P, Res).


%solve one 
solve([N,K,P], Res) :-
	fermat_binom(N, K, P, Res).

% Open file and solve all
final(File, Res) :-
	read_from_file(File, Lines),
	% solve problems concurrently
	concurrent_maplist(solve, Lines, Res).
