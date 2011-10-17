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
%%   deref(Ref, Data, Mem, Mem) -- Follow sequence of ref(_) cells started from
%%       Ref cell and return Data contained in last cell.
%%   deref_but_last(Ref, Data_Ref, Mem, Mem) -- Follow sequence of ref(_) cells
%%       started from Ref cell and return Ref_Data. Ref_Data is the last ref(_)
%%       cell in sequence.
%%   alloc_item(ID, Old_Mem, New_Mem) -- allocate 1 ref(0) cell in heap and
%%       return ID of this cell.
%%   alloc_array(Num, ID, Old_Mem, New_Mem) -- allocate array(Num) heap cell
%%       followed by Num continous ref(0) cells.
%%   free(ID, Old_Mem, New_Mem) -- delete ID cell from memory.
%%   assign(ID, Data, Old_Mem, New_Mem) -- replace data pointed by ID with Data.
%%   sum(A, B, Result, Mem, Mem) -- Result is A + B. Used deref/4 on A and B.
%%   sub(A, B, Result, Mem, Mem) -- Result is A - B. Used deref/4 on A and B.
%%   mul(A, B, Result, Mem, Mem) -- Result is A * B. Used deref/4 on A and B.
%%   div(A, B, Result, Mem, Mem) -- Result is A / B. Used deref/4 on A and B.

%%% Code:

:- include('memory.pl').

%% Dereference
deref(ref(0), ref(0), Mem, Mem) :- !.
deref(ref(A), R) --> mem_get(ref(A), R1), {!}, deref(R1,R).
deref(Result, Result, Mem, Mem) :- !.

%% TODO: UNUSABLE FUNCTION. Удалить?
deref_but_last(ref(0), _Result, _Old_Memory, _New_Memory) :- !, fail.
deref_but_last(ref(A), R) -->
	mem_get(ref(A), ref(B)),
	deref_but_last(ref(B),R), {!}. % Never backtrace to choices before
				       % deref_but_last call
deref_but_last(ref(A), ref(A), Mem, Mem) :- !.

%% Allocation/deallocation
%% TODO: Возможно стоит использовать одно и то же имя для двух alloc_* функций
alloc_item(ID) --> mem_add(ref(0), ID).		% 1 cell allocation.
alloc_array(N, ID) --> mem_add(array(N), ID).	% allocate array
free(ID) --> mem_del(ID). 			% Free this item

%% assignment
assign(ID, Data) --> mem_mod(ID1, Data).

%% Arithmetic
sum(A, B, num(R)) --> deref(A, num(A1)), deref(B, num(B1)), { R is A1 + B1 }.
sub(A, B, num(R)) --> deref(A, num(A1)), deref(B, num(B1)), { R is A1 - B1 }.
mul(A, B, num(R)) --> deref(A, num(A1)), deref(B, num(B1)), { R is A1 * B1 }.
div(A, B, num(R)) --> deref(A, num(A1)), deref(B, num(B1)), { R is A1 / B1 }.

%% TODO: function facility, initialization facility
