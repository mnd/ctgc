%% -*- mode: prolog -*-
%%% program.pl --- Code of the program

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

%%% Code:

:- include('operations.pl').

% You can evaluate it with "empty_memory(M), eval([swap], M, R)" to obtain resulting memory state.
user_function(swap, [
	alloc_item(A),
	alloc_item(B),
	assign_value(A, num(5)),
	assign_value(B, num(6)),

	alloc_item(Cond),
	copy(A, D),
	assign_value(Cond, num(1)),
	if(Cond,
	   [  alloc_item(C), 
	      assign(C, A),
	      assign(A, B),
	      assign(B, C),
	      unset(C)
	   ],
	   [
           ]),
	unset(A), unset(B), unset(Cond), unset(D),
	pass
]).

user_function(alloc, [
	alloc_item(A),
	alloc_item(B),
	assign_value(A, num(5)),
	assign_value(B, num(6)),

	if(ref(0),
	   [  copy(A, D)
	   ],
	   [  copy(B, D)
           ]),
	unset(A)
]).
