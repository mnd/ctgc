%% -*- mode: prolog -*-
%%% memory.pl --- Simple memory abstraction.

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

%% This module contain code for emulate work with memory. Memory represented by
%% `mem/2' functor. So in data `mem(Next_ID, Node_List)' `Next_ID' is ID of next
%% added memory node, `Node_List' contain list of `node(ID, Data)' functors,
%% where `ID' is ID of current node and `Data' is data stored in this node.
%%
%% Our memory typed and every node can contain one of the four data types.
%% This types:
%%
%%   num(X)    -- X is number
%%   ref(X)    -- X is reference to memory cell
%%   atom(X)   -- X is Prolog atom.
%%   cons(X,Y) -- X is pair of two data's
%%
%% This file provide next predicates:
%%
%%   memory(Mem) -- check if Mem is correct memory data
%%   empty_memory(Mem) -- unified Mem with empty memory. Empty memory contain
%%       only `node(0, ref(0))' node, that contain reference to itself.
%%   mem_add(Data, ID, Old_Memory, New_Memory) -- Add Data in Old_Memory to
%%       provide New_Memory. ID unifies with new node id in the memory.
%%   mem_del(ID, Old_Memory, New_Memory) -- Remove ID node from Old_Memory to
%%       provide New_Memory. Fail with `ID = 0'.
%%   mem_get(ID, Data, Memory)
%%   mem_get(ID, Data, Memory, Memory) -- unify Data with data of ID node in
%%       the Memory.
%%   mem_mod(ID, Data, Old_Memory, New_Memory) -- change data of ID node in
%%       Old_Memory to Data to provide New_Memory.
%%
%% mem_* predicates designed for use in DCG code.

%%% Code:

:- include('helpers.pl').
 
memory(mem(Next_ID, Data_List)) :- number(Next_ID), node_list(Data_List).
 
empty_memory(mem(1, [node(0, ref(0))])) :- memory(mem(1, [node(0, ref(0))])).
 
%% List of memory cells (nodes).
node_list([]).
node_list([node(ID, Data)| Tail]) :- number(ID), data(Data), data_list(Tail).
 
%% Test for memory types.
%% TODO: Тип cons/2 дейстительно нужен?
data(num(X)) :- number(X).
data(ref(X)) :- number(X).
data(atom(X)) :- atom(X).
data(cons(X,Y)) :- data(X), data(Y).
 
%% TODO: проверять, что добавляемая ref ячейка консистентна?
mem_add(Data, ID, mem(ID, DList), mem(Next_ID, [node(ID, Data) | DList])) :-
	memory(mem(ID, DList)),
	Next_ID is ID + 1,
	memory(mem(Next_ID, [node(ID, Data) | DList])).
%% 
%% TODO: проверять остались ли ref ячейки консистентными?
mem_del(0, _, _) :- !, fail.			% NULL удалять нельзя
mem_del(ID, mem(Next_ID, DList), mem(Next_ID, Reduced_DList)) :-
	memory(mem(Next_ID, DList)),
	delete_unified(DList, node(ID,_), Reduced_DList), !, 
	memory(mem(Next_ID, Reduced_DList)).
%% 
mem_get(ID, Data, mem(Next_ID, Data_List)) :-
	memory(mem(Next_ID, Data_List)),
	once(member(node(ID, Data), Data_List)).
mem_get(ID, Data, M, M) :- mem_get(ID, Data, M).
%% 
mem_mod(ID, Data, mem(Next_ID, DList), mem(Next_ID, New_DList)) :-
	memory(mem(Next_ID, DList)),
	change_unified(DList, node(ID, _), node(ID, Data), New_DList), !,
	memory(mem(Next_ID, New_DList)).

