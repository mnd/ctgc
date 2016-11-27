%% -*- mode: prolog -*-
%% m(next_queue_name, [queues])
%% q(queue_name, next_var_name, [[vars]])
%% v(var_name, value)
%% value = ref(queue_name, var_name) | number
:- include('helpers.pl').
empty(m(0, [])).
push_queue(N, m(N, Qs), m(N1, [q(N, 0, [[]])|Qs])) :-
    N1 is N + 1, !.
pop_queue(m(N, [_|Qs]), m(N, Qs)) :- !.

enqueue(QName, m(N, Qs), m(N, Qs1)) :-
    enqueue_internal(QName, Qs, Qs1), !.
enqueue_internal(QName, [q(QName, V, Q) | Qs], [q(QName, V, [[]|Q]) | Qs]).
enqueue_internal(QName, [Q | Qs], [Q | Qs1]) :-
    enqueue_internal(QName, Qs, Qs1).

dequeue(QName, m(N, Qs), m(N, Qs1)) :-
    dequeue_internal(QName, Qs, Qs1), !.
dequeue_internal(QName, [q(QName, V, Q1) | Qs], [q(QName, V, Q2) | Qs]) :-
    dequeue_list(Q1, Q2).
dequeue_internal(QName, [Q | Qs], [Q | Qs1]) :-
    dequeue_internal(QName, Qs, Qs1).
dequeue_list([_], []).
dequeue_list([X|Xs], [X|R]) :- dequeue_list(Xs, R).

new_var(QName, ref(QName, VName), Value, m(N, Qs), m(N, Qs1)) :-
    new_var_internal(QName, VName, Value, Qs, Qs1), !.
new_var_internal(QName, V, Value,
		 [q(QName, V,  [Vs               | Vss]) | Qs],
		 [q(QName, V1, [[v(V, Value)|Vs] | Vss]) | Qs]) :-
    V1 is V + 1.
new_var_internal(QName, VName, Value, [Q|Qs], [Q|Qs1]) :-
    new_var_internal(QName, VName, Value, Qs, Qs1).

set_var(ref(QName, VName), Value, m(N, Qs), m(N,Qs1)) :-
    set_var_queue(QName, VName, Value, Qs, Qs1), !.
set_var_queue(QName, VName, Value,
		 [q(QName, V, Vs) |Qs],
		 [q(QName, V, Vs1)|Qs]) :-
    set_var_queue_internal(VName, Value, Vs, Vs1).
set_var_queue(QName, VName, Value, [Q|Qs], [Q|Qs1]) :-
    set_var_queue(QName, VName, Value, Qs, Qs1).
set_var_queue_internal(VName, Value, [V|Vs], [V1|Vs]) :-
    member_unified(V, v(VName, X)),
    change_unified(V, v(VName, X), v(VName, Value), V1).
set_var_queue_internal(VName, Value, [V|Vs], [V|Vs1]) :-
    set_var_queue_internal(VName, Value, Vs, Vs1).

change_var(ref(QName, VName), Value, m(N, Qs), m(N,Qs1)) :-
    change_var_queue(QName, VName, Value, Qs, Qs1), !.
change_var_queue(QName, VName, Value,
		 [q(QName, V, Vs) |Qs],
		 [q(QName, V, Vs1)|Qs]) :-
    change_var_queue_internal(VName, Value, Vs, Vs1).
change_var_queue(QName, VName, Value, [Q|Qs], [Q|Qs1]) :-
    change_var_queue(QName, VName, Value, Qs, Qs1).
change_var_queue_internal(VName, Value,
			  [V                   | Vs],
			  [[v(VName, Value)|V] | Vs]).

get_var(ref(QName, VName), Value, m(N, Qs), m(N, Qs)) :-
    get_var_queue(QName, VName, Value, Qs), !.
get_var(V, V, M, M).
get_var_queue(QName, VName, Value, [q(QName, _, Vs) | _]) :-
    get_var_queue_internal(VName, Value, Vs).
get_var_queue(QName, VName, Value, [_ | Qs]) :-
    get_var_queue(QName, VName, Value, Qs).
get_var_queue_internal(VName, Value, [V|_]) :-
    member_unified(V, v(VName, Value)).
get_var_queue_internal(VName, Value, [_|Vs]) :-
    get_var_queue_internal(VName, Value, Vs).

