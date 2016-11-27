member_unified([E | _], E).
member_unified([_ | L1], E) :- member_unified(L1, E).

delete_unified([], _, []).
delete_unified([E | List1], E, List2) :- delete_unified(List1, E, List2).
delete_unified([A | List1], E, [A | List2]) :- delete_unified(List1, E, List2).

change_unified([], _, _, []).
change_unified([E | List1], E, A, [A | List2]) :-
	change_unified(List1, E, A, List2).
change_unified([B | List1], E, A, [B | List2]) :-
	change_unified(List1, E, A, List2).

