%% -*- prolog -*-
%%% helpers.pl --- some helper functions.

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
%% 
%%   delete_unified(List1, Elt, List2) -- removes all occurrences of Elt in
%%       List1 to provide List2. Elt occurrences by mean of unification =/2
%% 
%%   change_unified(List1, Elt1,, Elt2 List2) -- change all occurrences of Elt1
%%       to Elt2 in List1 to provide List2. Elt1 occurrences by mean of
%%       unification =/2

%%% Code:

delete_unified([], _, []).
delete_unified([E | List1], E, List2) :- delete_unified(List1, E, List2).
delete_unified([A | List1], E, [A | List2]) :- delete_unified(List1, E, List2).

change_unified([], _, _, []).
change_unified([E | List1], E, A, [A | List2]) :-
	change_unified(List1, E, A, List2).
change_unified([B | List1], E, A, [B | List2]) :-
	change_unified(List1, E, A, List2).
