/* ___________________IO___________________ 
*/

%read_from_file
read_from_file(File, Params, Lines) :-
	 open(File, read, Stream),
	 read_params(Stream, Params),
	 Params = [N | _],
	 read_lines(Stream, N, Lines).

%read_first_line
read_params(Stream , Params) :-
	 read_line_to_codes(Stream, Line),
	 atom_codes(Atom, Line),
	 atomic_list_concat(Atoms, ' ', Atom),
	 maplist(atom_number, Atoms, Params).


%useful_for_maplist.
code_char(A,B) :- char_code(B,A).

%read_a_string
read_string_(Stream, Res) :-
	read_line_to_codes(Stream, Codes),	%read_line
	Codes \= "\n",				
	maplist(code_char, Codes, Char_list),	%convert 
	string_to_list(Res, Char_list).		


%read_N_lines_from_Stream
read_lines(_, 0, []).
read_lines(Stream, Counter, [X|L]) :-   
	%read
	read_string_(Stream, X),
	%recurse
	New_counter is Counter-1,
	read_lines(Stream, New_counter, L).


%my_handle_is: read_from_file

%__________________IO_END___________________


:- dynamic memo_/1.

memo(Goal) :-
        (   memo_(Goal)
        ->  true
        ;   once(Goal),
            assertz(memo_(Goal))
        ).





%euclidean modulo
em(A, B, Res):-
	M is A rem B,
	(
	(M < 0) ->
		(
			(B < 0) -> 
				Res is M - B
			;
				Res is M + B
		)
	;
		Res = M
	).


wins(L) :-
	L = [1,3,7,15,31,63,127,255,511,1023,2047,4095,
	8191,16383,32767,65535,131071,262143,524287, 1048575].


%calculate ways of n -> iterating winstreaks.
calc(Index, Acc, [Win|Rest], Mod, Res) :-
	Win > Index -> 
		em(Acc, Mod, Res)
	;
	(
		Win =< Index,
		DP_lookback is Index - Win,	
		memo( ways(DP_lookback, Mod, Elem) ),
		New_acc is Acc + Elem, 		
		calc(Index, New_acc, Rest, Mod, Res)
	).

ways(0, _, 1).
ways(1, _, 1).
ways(N, Mod, Res) :-
	wins(Wins),
	calc(N, 0, Wins, Mod, Res).

partial(0, _, 0).
partial(N, Mod, Res) :-
	N > 0,

	memo( ways(N, Mod, Ways_N) ),

	Prev is N-1,

	memo( partial(Prev, Mod, Partial_Prev) ),

	Raw_res is Partial_Prev + Ways_N,
	em(Raw_res, Mod, Res).



solve(0, 0, _, 1).
solve(0, N2, Mod, Res) :-
	N2 > 0,

	memo( partial(N2, Mod, Partial_N2) ),

	Raw_Res is 2 * Partial_N2 - 1,
	em(Raw_Res, Mod, Res).

solve(N1, N2, Mod, Res) :-
	N1 > 0,
	N2 > 0,
	But_one is N1 - 1,
	memo( partial(But_one, Mod, Partial_N1) ),
	memo( partial(N2, Mod, Partial_N2) ),
	Raw_Res is 2 * (Partial_N2 - Partial_N1),
	em(Raw_Res, Mod, Res).



tabling(100008, _).
tabling(N, Mod) :-
	N < 1000008,
	memo( partial(N, Mod, _) ),
	Succ is N + 1,
	tabling(Succ, Mod).

