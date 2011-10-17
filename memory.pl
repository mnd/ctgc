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
%%   num(X)   -- X is number.
%%   ref(X)   -- X is reference to memory cell, X have integer type.
%%   atom(X)  -- X is Prolog atom.
%%   array(N) -- N is integer number of continous cells that create with
%%               array(N) cell and must be deleted with this cell.
%%
%% This file provide next predicates:
%%
%%   memory(Mem) -- check if Mem is correct memory data
%%   empty_memory(Mem) -- unified Mem with empty memory. Empty memory contain
%%       only `node(0, ref(0))' node, that contain reference to itself.
%%   mem_add(Data, ID, Old_Memory, New_Memory) -- Add Data in Old_Memory to
%%       provide New_Memory. ID unifies with ref(new_node_id) in the memory.
%%   mem_add_n_ref(Num, Old_Memory, New_Memory) -- Add Num ref(0) cells to
%%       memory.
%%   mem_del(ID, Old_Memory, New_Memory) -- Remove ID node from Old_Memory to
%%       provide New_Memory. Fail with `ID = ref(0)'.
%%   mem_del_n(First_ID, Num, Old_Memory, New_Memory) -- Remove Num memory items
%%       started from First_ID.
%%   mem_get(ID, Data, Memory).
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
node_list([node(ID, Data)| Tail]) :- integer(ID), data(Data), node_list(Tail).
 
%% Test for memory types.
data(num(X)) :- number(X).
data(ref(X)) :- integer(X).
data(atom(X)) :- atom(X).
data(array(N)) :- integer(N).
 
%% TODO: проверять, что добавляемая ref ячейка консистентна?
mem_add(Data, ref(ID), mem(ID, DList), New_Mem) :-
	memory(mem(ID, DList)),
	NID is ID + 1,
	Mem = mem(NID, [node(ID, Data) | DList]),
	memory(Mem),
	(   Data = array(N) ->
	    mem_add_n_ref(N, Mem, New_Mem),
	    New_Mem = mem(Next_ID, _),
	    Next_ID =:= NID + N			% Data were added coninously.
	;   New_Mem = Mem			% Yet done.
        ).
%% 
mem_add_n_ref(0, Mem, Mem) :- !.
mem_add_n_ref(N) -->
	mem_add(ref(0), _), 
	{ !, N1 is N - 1 }, 
	mem_add_n_ref(N1).
%% TODO: проверять остались ли ref ячейки консистентными?
mem_del(ref(0), _, _) :- !, fail.			% NULL удалять нельзя
mem_del(ref(ID), mem(Next_ID, DList), New_Mem) :-
	memory(mem(Next_ID, DList)),
	delete_unified(DList, node(ID,Data), Reduced_DList), !, 
	Mem = mem(Next_ID, Reduced_DList),
	memory(Mem),
	(   Data = array(N) ->
	    ID1 is ID + 1,
	    mem_del_n(ref(ID1), N, Mem, New_Mem)
	;   New_Mem = Mem
        ).
%% 
mem_del_n(_, 0, Mem, Mem) :- !.
mem_del_n(ref(ID), N) -->
	mem_del(ref(ID)),
	{ ID1 is ID + 1, N1 is N - 1 },
	mem_del_n(ref(ID1), N1).
%% 
mem_get(ref(ID), Data, mem(Next_ID, Data_List)) :-
	memory(mem(Next_ID, Data_List)),
	once(member(node(ID, Data), Data_List)).
mem_get(ID, Data, M, M) :- mem_get(ID, Data, M).
%% 
mem_mod(ref(ID), Data, mem(Next_ID, DList), mem(Next_ID, New_DList)) :-
	memory(mem(Next_ID, DList)),
	change_unified(DList, node(ID, _), node(ID, Data), New_DList), !,
	memory(mem(Next_ID, New_DList)).
