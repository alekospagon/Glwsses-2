:- use_module(library(thread)).


%read_from_file
read_from_file(File, N, Lines) :-
	 open(File, read, Stream),
	 read_params(Stream, [N]),
	 read_lines(Stream, N, Lines).

%read_first_line
read_params(Stream , Params) :-
	 read_line_to_codes(Stream, Line),
	 atom_codes(Atom, Line),
	 atomic_list_concat(Atoms, ' ', Atom),
	 maplist(atom_number, Atoms, Params).

%read_N_lines_from_Stream
read_lines(_, 0, []).
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
	New_e is Exp div 2,
	Parity is mod(Exp, 2),
	(
		Parity =:= 0 -> 
			New_a is Acc 
		;
			New_a is (Acc * Base) mod Mod
	),
	fastpow_(New_b, New_e, Mod, New_a, Res).



% degree of p in n!  (exponent of p in the factorization of n!)
fact_exp(N, P, Res) :-
	fact_exp_loop(0, P, N, P,Res).
fact_exp_loop(E, U, N, P, Res) :-
	(U > N) -> 
		(Res = E)
	;	
		(
			New_e is E + (N div U),
			New_u is U * P,
			fact_exp_loop(New_e, New_u, N, P, Res)
		)
	.




% fermat_binom. Res = nCk % p
fermat_binom(N, K, P, Res) :-
	% num_degree and den_degree
	fact_exp(N, P, Num1), 
	N_K is N-K, fact_exp(N_K, P, Num2),
	Num_degree is Num1 - Num2,
	fact_exp(K,P,Den_degree),

	% quick check
	( (Num_degree > Den_degree) ; (K>N)	) ->
		(Res = 0)
		;
		(
			get_num(N, K, P, Num),
			get_den(K, P, Den),
			P_2 is P-2,
			fastpow(Den, P_2, P, Fast_pow),
			Res is (Num * Fast_pow) mod P
		)
	.




%product of num and den
get_num(N, K, P, Res) :-
	From is N-K+1,
	To is N + 1,
	range_loop(From, To, 1, P, Res).


get_den(K, P, Res) :-
	To is K + 1,
	range_loop(1, To, 1, P, Res).




% product loop
range_loop(From, To, Acc, P, Res) :-
	(From = To) ->
		(Res = Acc)
	;
		(
			New_from is From + 1,
			New_acc  is (From * Acc) mod P,
			range_loop(New_from, To, New_acc, P, Res)
		)
	.


%solve one 
solve([N,K,P], Res) :-
	fermat_binom(N, K, P, Res).


query(Params, Res) :-
	solve(Params, Res).



final(File, Res) :-
	read_from_file(File, _, Lines),
	%maplist(query, Lines, Res).
	concurrent_maplist(query, Lines, Res).



