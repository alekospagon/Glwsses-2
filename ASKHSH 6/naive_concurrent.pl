%concurrent 
:- use_module(library(thread)).


%read_from_file
read_from_file(File, Lines) :-
	 open(File, read, Stream),
	 read_params(Stream, [N]),
	 read_lines(Stream, N, Lines).

%read_first_line
read_params(Stream , Params) :-
	 read_line_to_codes(Stream, Line),
	 atom_codes(Atom, Line),
	 atomic_list_concat(Atoms, ' ', Atom),
	 concurrent_maplist(atom_number, Atoms, Params).

%read_N_lines_from_Stream
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
	fastpow_(Base, Exp, Mod, 1, Res).

% helping pred
fastpow_(_, 0, _, Acc, Acc) :- !.	%dont look further
fastpow_(Base, Exp, Mod, Acc, Res) :-
	New_b is (Base * Base) rem Mod,
	New_e is Exp >> 1,
	(
		% Parity
		0 is Exp /\ 1 -> 
			New_a is Acc 
		;
			New_a is (Acc * Base) rem Mod
	),
	fastpow_(New_b, New_e, Mod, New_a, Res).



% nCk % p
fermat_binom(N, K, P, Res) :-

	Fermat_Goals = [
			get_num(N, K, P, Num),
			get_den(K, P, Den)
		],

	% spawn threads
	concurrent(2, Fermat_Goals, []),

	P_2 is P - 2,
	fastpow(Den, P_2, P, Fast_pow),
	Res is (Num * Fast_pow) rem P.



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

	Par_goals = [
			range_loop(From, Low  , 1, P, Res1),
			range_loop(Low , Half , 1, P, Res2),
			range_loop(Half, High , 1, P, Res3),
			range_loop(High, Upper, 1, P, Res4)
		],

	concurrent(4, Par_goals, [] ),


	% merge two lower and two upper areas
	Merge1 is (Res1 * Res2) rem P,
	Merge2 is (Res3 * Res4) rem P,
	%merge together
	Res is (Merge1 * Merge2) rem P.


% product loop
range_loop(To, To, Acc, _, Acc) :- !.
range_loop(From, To, Acc, P, Res) :-
	New_from is From + 1,
	New_acc  is (From * Acc) rem P,
	range_loop(New_from, To, New_acc, P, Res).


%solve one 
solve([N,K,P], Res) :-
	fermat_binom(N, K, P, Res).

% Open file and solve all
final(File, Res) :-
	read_from_file(File, Lines),
	% solve problems concurrently
	concurrent_maplist(solve, Lines, Res).
