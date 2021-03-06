%% -*- mode: prolog -*-
%%% operations.pl --- Trivial operations necessary for every language.

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

%% This module contan functions necessary for every programming language. This
%% code work on top of `memory.pl' module.
%%
%% This file provide next predicates:
%%
%%   deref(Ref, Data, Mem, Mem) -- Follow ref(A) reference. Fail on ref(0).
%%   alloc_item(ID, Old_Mem, New_Mem) -- allocate 1 ref(0) cell in heap and
%%       return ID of this cell.
%%   alloc_array(Num, ID, Old_Mem, New_Mem) -- allocate array(Num) heap cell
%%       followed by Num continous ref(0) cells.
%%   free(ID, Old_Mem, New_Mem) -- delete ID cell from memory.
%%   assign(ID, Data, Old_Mem, New_Mem) -- replace data pointed by ID with Data.
%%   elt(A, N, Ref, Mem, Mem) -- get Ref to N element of array A.
%%   sumi(A, B, Result)
%%   sum(A_Ref, B_Ref, Result_Ref, Mem, Mem) -- Result is A + B.
%%   subi(A, B, Result)
%%   sub(A_Ref, B_Ref, Result_Ref, Mem, Mem) -- Result is A - B.
%%   muli(A, B, Result)
%%   mul(A_Ref, B_Ref, Result_Ref, Mem, Mem) -- Result is A * B.
%%   divi(A, B, Result)
%%   div(A_Ref, B_Ref, Result_Ref, Mem, Mem) -- Result is A / B.
%%   if(Condition, Then_List, Else_List, Result_List, Mem, Mem) --
%%       Result_List is result list of commands. Else if Condition -- pointer
%%       to num(0) or ref(0). Else -- true.
%%   :- multifile(user_function/2) -- user_function/2 used for function
%%       definition.
%%   eval1(Head, Body, Old_Mem, New_Mem) -- 1 step evaluation. Directly eval
%%       built in functions, and use user_function/2 for user defined functions.
%%   eval(Command_List, Old_Mem, New_Mem) -- sequencialy eval list of commands.
%%   run(Command_List, Result_Mem) -- evaluate Command_List with empty original memory

%%% Code:

:- include('memory.pl').

%% Dereference
deref(ref(0), _, Mem, Mem) :- !, fail.		% TODO: Может быть использовать 
deref(ref(A), R) --> mem_get(ref(A), R), {!}.

%% Allocation/deallocation
%% TODO: Возможно стоит использовать одно и то же имя для двух alloc_* функций
alloc_item(Ref) --> mem_add(ref(0), Ref).	% 1 cell allocation.
alloc_array(N, Ref) --> mem_add(array(N), Ref).	% allocate array
free(Ref) --> mem_del(Ref). 			% Free this item

%% assignment
assign(ID, Data) --> mem_mod(ID, Data).
%% arrat getter
elt(A, N, R) --> 
	deref(A, R1),
	{ A = ref(N0),
	  R1 = array(N1),
	  N =< N1,
	  NRef is N0 + N,
	  R = ref(NRef) }.

%% Arithmetic
sumi(num(A), num(B), num(R)) :- R is A + B.
sum(A,B,R) --> deref(A, A1), deref(B, B1), { sumi(A1,B1,Data) }, assign(R, Data).

subi(num(A), num(B), num(R)) :- R is A - B.
sub(A,B,R) --> deref(A, A1), deref(B, B1), { subi(A1,B1,Data) }, assign(R, Data).

muli(num(A), num(B), num(R)) :- R is A * B.
mul(A,B,R) --> deref(A, A1), deref(B, B1), { muli(A1,B1,Data) }, assign(R, Data).

divi(num(A), num(B), num(R)) :- R is A / B.
div(A,B,R) --> deref(A, A1), deref(B, B1), { divi(A1,B1,Data) }, assign(R, Data).

if(A, TL, EL, RL) -->
	deref(A, A1),
	{   A1 = num(0) ->
	    RL = EL
	;   A1 = ref(0) ->
	    RL = EL
	;
	    RL = TL
	}.

:- multifile(user_function/2).

eval1(sum(A, B, R), []) --> sum(A, B, R).
eval1(sub(A, B, R), []) --> sub(A, B, R).
eval1(mul(A, B, R), []) --> mul(A, B, R).
eval1(div(A, B, R), []) --> div(A, B, R).
eval1(deref(X, R), []) --> deref(X, R).
eval1(alloc_item(R), []) --> alloc_item(R).
eval1(alloc_array(N, R), []) --> alloc_array(N, R).
eval1(assign_value(Ref, Data), []) --> assign(Ref,Data).
eval1(assign(Ref1, Ref2), []) --> deref(Ref1, Data), assign(Ref2,Data).
eval1(elt(A, N, R), []) --> elt(A, N, R).
eval1(display(X), [], Mem, Mem) :- format("DISPLAY: ~p~n", [X]).
eval1(if(A, TL, EL), RL) --> if(A, TL, EL, RL).
eval1(while(A, IL), RL) --> { append(IL, [while(A, IL)], IL1) }, if(A, IL1, [], RL).
eval1(free(Ref), []) --> free(Ref).
eval1(unset(_), [], Mem, Mem).		      % do nothing. Just flag.
eval1(pass, [], Mem, Mem).		      % do nothing. Just flag.
eval1(copy(A,A), [], Mem, Mem).	% simple create second name for variable.
eval1(Head, Body, Mem, Mem) :- user_function(Head, Body).
%% TODO: initialization facility
eval([O|OL]) --> eval1(O,R), { append(R, OL, RL) }, eval(RL).
eval([], M, M).

run(OL, Result_Mem) :- empty_memory(New_Mem), eval(OL, New_Mem, Result_Mem).
