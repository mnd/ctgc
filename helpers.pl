%% -*- prolog -*-

%% delete_unified(List1, Elt, List2) -- удаляет из List1 элементы, которые
%% эквивалентны Elt в смысле =/2, для получения List2.
delete_unified([], _, []).
delete_unified([E | List1], E, List2) :- delete_unified(List1, E, List2).
delete_unified([A | List1], E, [A | List2]) :- delete_unified(List1, E, List2).

%% change_unified(List1, Elt1, Elt2, List2) -- заменяет элементы List1, которые
%% эквивалентны Elt1 в смысле =/2, на Elt2 для получения List2.
change_unified([], _, _, []).
change_unified([E | List1], E, A, [A | List2]) :-
	change_unified(List1, E, A, List2).
change_unified([B | List1], E, A, [B | List2]) :-
	change_unified(List1, E, A, List2).
