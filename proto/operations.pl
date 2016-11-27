%% -*- mode: prolog -*-
:- include('memory.pl').

eval1(enqueue(N)) --> enqueue(N).
eval1(dequeue(N)) --> dequeue(N).
eval1(new(N, X, ref(Q,V))) -->
    get_var(ref(Q,V), Val),
    new_var(N, X, Val).
eval1(new(N, X, Val)) --> new_var(N, X, Val).
eval1(new(N, X)) --> new_var(N, X, 0).
eval1(change(X, ref(Q,V))) -->
    get_var(ref(Q,V), Val),
    change_var(X, Val).
eval1(change(X, Val)) --> change_var(X, Val).
eval1(set(X, ref(Q,V))) -->
    get_var(ref(Q,V), Val),
    set_var(X, Val).
eval1(set(X, Val)) --> set_var(X, Val).
eval1(sum(X, Y, R)) -->
    get_var(X, A), get_var(Y, B),
    {C is A + B},
    set_var(R, C).
eval1(sub(X, Y, R)) -->
    get_var(X, A), get_var(Y, B),
    {C is A - B},
    set_var(R, C).
eval1(mul(X, Y, R)) -->
    get_var(X, A), get_var(Y, B),
    {C is A * B},
    set_var(R, C).
eval1(div(X, Y, R)) -->
    get_var(X, A), get_var(Y, B),
    {C is (A // B)},
    set_var(R, C).
eval1(rem(X, Y, R)) -->
    get_var(X, A), get_var(Y, B),
    {C is A rem B},
    set_var(R, C).
eval1(print(ref(Q,V))) -->
    get_var(ref(Q,V), A),
    {write(ref(Q,V)), write(' = '), write(A), nl}.
eval1(print(X)) -->
    {write(X), nl}.
eval1(pass, M, M).

eval([], M, M) :- !.
eval([block(N, Body) | Cs]) -->
    push_queue(N), eval(Body), pop_queue, eval(Cs).
eval([if(N, Cond, _Then, Else) | Cs]) -->
    get_var(Cond, 0),
    push_queue(N), eval(Else), pop_queue, eval(Cs).
eval([if(N, Cond, Then, _Else) | Cs]) -->
    get_var(Cond, _),
    push_queue(N), eval(Then), pop_queue, eval(Cs).
eval([for(N1, N2, Pres, Cond, Posts, Body) | Cs]) -->
    push_queue(N1), eval(Pres), eval_for_first(N1, N2, Cond, Posts, Body),
    pop_queue,
    eval(Cs).
eval([call(Name, Qs, As) | Cs]) -->
    {function(Name, N, Qs, As, Body),
     copy_term(function(Name, N, Qs, As, Body), function(_, N1, Qs, As, Body1))},
    push_queue(N1),
    eval(Body1),
    pop_queue,
    eval(Cs).
eval([C | Cs]) -->
    eval1(C), eval(Cs).

eval_for_first(_N1, _N2, Cond, _Posts, _Body) -->
    get_var(Cond, 0),
    pop_queue.
eval_for_first(N1, N2, Cond, Posts, Body) -->
    get_var(Cond, _),
    {copy_term(block(N2, Body), B)},
    eval([B]),
    enqueue(N1),
    eval(Posts),
    eval_for(N1, N2, Cond, Posts, Body).
eval_for(N1, _N2, Cond, _Posts, _Body) -->
    get_var(Cond, 0),
    dequeue(N1).
eval_for(N1, N2, Cond, Posts, Body) -->
    get_var(Cond, _),
    dequeue(N1),
    {copy_term(block(N2, Body), B)},
    eval([B]),
    enqueue(N1),
    eval(Posts),
    eval_for(N1, N2, Cond, Posts, Body).

start(Main) :-
    empty(M),
    eval([call(Main, [], [])], M, _M1).

