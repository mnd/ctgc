%% -*- prolog -*-
%%% analyse.pl --- run program checks.

%% Copyright (C) 2011  Merinov Nikolay

%% Author: Merinov Nikolay <kim.roader@gmail.com>

%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.

%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%%% Commentary:

%% This module contain helper functions:

%%% Code:
:- include('program.pl').

% print:
% 1. If program operation create new link to memory then print "plus(cell_number)", if remove -- print "minus(cell_number)". Otherwise ignore operation
% 2. Split probram to blocks
% 3. Write memory reference count for every block
% Usage: check(swap)
check(Name) :-
	user_function(Name, Body),
	phrase(check1(0, _, R), Body), !,
	print(R), nl,
	phrase(blocks(R1), R), !,
	print(R1), nl,
	process_blocks(R1, [], R2), !,
	print(R2).
	

check1(C, C, [], [], []).
check1(C, C2, [plus(A) | Result]) -->
	[alloc_item(A)], { A = C, C1 is C + 1 }, check1(C1, C2, Result).
check1(C, C2, [plus(A) | Result]) -->
	[alloc_array(A)], { A = C, C1 is C + 1 }, check1(C1, C2, Result).
check1(C, C2, [plus(A) | Result]) -->
	[elt(A, _, R)], { R = A, A = C, C1 is C + 1 }, check1(C1, C2, Result).
check1(C, C2, [plus(A) | Result]) -->
	[copy(A, A)], check1(C, C2, Result).
check1(C, C1, [minus(A) | Result]) -->
	[free(A)], check1(C, C1, Result).
check1(C, C1, [minus(A) | Result]) -->
	[unset(A)], check1(C, C1, Result).
check1(C, C3, [if(R1, R2) | Result]) -->
	[if(_, T1, T2)], { phrase(check1(C, C1, R1), T1), phrase(check1(C1, C2, R2), T2) }, check1(C2, C3, Result).
check1(C, C1, Result) -->
	[_], check1(C, C1, Result).


blocks([[]], [], []).
blocks([[], p(R1,R2) | R]) -->
	[if(B1, B2)],
	{ copy_term(B2, B3),
	  phrase(blocks(R1), B1),
	  phrase(blocks(R2), B3)
        },
	blocks(R).
blocks([[A | R] | RS]) -->
	[A],
	blocks([R | RS]).

%% correct([[[A|AL]] | L], [A1 | L1]) :- correct([A|AL], A1), correct(L, L1). 
%% correct([[A|AL] | L], [A1 | L1]) :- correct([A|AL], A1), correct(L, L1). 
%% correct([[]], []).
%% correct(A, C) :- sum_it(A, [], B), msort(B, C).

sum_it([], R, R).
sum_it([plus(A) | L], R, RES) :-
	(   member_unified(R, change(A, I)) ->
	    I1 is I + 1, 
	    change_unified(R, change(A, I), change(A, I1), R1)
	;   R1 = [change(A, 1) | R]
        ),
	sum_it(L, R1, RES).
sum_it([minus(A) | L], R, RES) :-
	(   member_unified(R, change(A, I)) ->
	    I1 is I - 1, 
	    change_unified(R, change(A, I), change(A, I1), R1)
	;   R1 = [change(A, -1) | R]
        ),
	sum_it(L, R1, RES).

%% process_block +blocks +in_data -out_data
process_block([], D, D).
process_block([change(I,C)|CS], IN, OUT) :-
	(   member_unified(IN, count(I, C1)) ->
	    C2 is C1 + C,
	    change_unified(IN, count(I, C1), count(I, C2), IN1)
	;   IN1 = [count(I, C) | IN]
        ),
	process_block(CS, IN1, OUT).
process_block(A, IN, OUT) :- sum_it(A, [], B), process_block(B, IN, OUT).

process_blocks([], _, []).
process_blocks([p(B1, B2) | BS], IN, [b(p(R1, R2), OUT) | R]) :-
	list(B1), list(B2),
	process_blocks(B1, IN, R1),
	process_blocks(B2, IN, R2),
	last(R1, b(_, O1)),
	last(R2, b(_, O2)),
	compose_out(O1,O2, OUT),
	process_blocks(BS, OUT, R).
process_blocks([B | BS], IN, [b(B, OUT) | R1]) :-
	process_block(B, IN, OUT),
	process_blocks(BS, OUT, R1).

compose_out([], R, R).
compose_out([count(I, C) | OS], O2, R) :-
	(   member_unified(O2, count(I, C1)) ->
	    C2 is max(C1, C),
	    change_unified(O2, count(I, C1), count(I, C2), O3)
	;   O3 = [count(I, C) | O2]
        ),
	compose_out(OS, O3, R).
