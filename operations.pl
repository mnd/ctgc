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
%%   sum(A, B, Result)
%%   sum(A, B, Result, Mem, Mem) -- Result is A + B.
%%   sub(A, B, Result)
%%   sub(A, B, Result, Mem, Mem) -- Result is A - B.
%%   mul(A, B, Result)
%%   mul(A, B, Result, Mem, Mem) -- Result is A * B.
%%   div(A, B, Result)
%%   div(A, B, Result, Mem, Mem) -- Result is A / B.
%%   eval1(Head, Body, Old_Mem, New_Mem) -- 1 step evaluation. Directly eval
%%       built in functions, and use user_function/2 for user defined functions.

%%% Code:

:- include('memory.pl').

%% Dereference
deref(ref(0), _, Mem, Mem) :- !, fail.
deref(ref(A), R) --> mem_get(ref(A), R), {!}.

%% Allocation/deallocation
%% TODO: Возможно стоит использовать одно и то же имя для двух alloc_* функций
alloc_item(Ref) --> mem_add(ref(0), Ref).	% 1 cell allocation.
alloc_array(N, Ref) --> mem_add(array(N), Ref).	% allocate array
free(Ref) --> mem_del(Ref). 			% Free this item

%% assignment
assign(ID, Data) --> mem_mod(ID, Data).

%% Arithmetic
sum(num(A), num(B), num(R)) :- R is A + B.
sum(A,B,R,Mem,Mem) :- sum(A,B,R).
sub(num(A), num(B), num(R)) :- R is A - B.
sub(A,B,R,Mem,Mem) :- sub(A,B,R).
mul(num(A), num(B), num(R)) :- R is A * B.
mul(A,B,R,Mem,Mem) :- mul(A,B,R).
div(num(A), num(B), num(R)) :- R is A / B.
div(A,B,R,Mem,Mem) :- div(A,B,R).

%% TODO: function facility, initialization facility
eval1(sum(A, B), R) --> sum(A, B, R).
eval1(sub(A, B), R) --> sub(A, B, R).
eval1(mul(A, B), R) --> mul(A, B, R).
eval1(div(A, B), R) --> div(A, B, R).
eval1(deref(X), R) --> deref(X, R).
eval1(alloc_item, R) --> alloc_item(R).
eval1(alloc_array(N), R) --> alloc_array(N, R).
eval1(Head, Body, Mem, Mem) :- user_function(Head, Body).
