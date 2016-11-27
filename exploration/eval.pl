%% -*- mode: prolog -*-
%%% eval.pl --- Eval extracted codes in memory models.

%% Copyright (C) 2012  Merinov Nikolay
:- include('post_process.pl').
:- include('memory_models.pl').

eval1(_Type, expr(_), Mem, Mem).
eval1(_Type, create(Var, _)) -->
    create_variable(Var).
eval1(Type, change(Var, _)) -->
    {atom_concat(change_variable_, Type, Change_Variable)},
    call(Change_Variable, Var).
eval1(Type, destroy(Vs)) -->
    destroy_all(Type, Vs).
destroy_all(_Type, [], Mem, Mem).
destroy_all(Type, [V|Vs]) -->
    {atom_concat(delete_variable_, Type, Delete_Variable)},
    call(Delete_Variable, V),
    destroy_all(Type, Vs).

%% Type is one of {queue, stack, region}
%% eval/3
eval(Type, Code, List) :-
    empty_memory(Mem),
    push_memory_block(Mem, Mem1),
    eval(Type, Code, RList, Mem1, Mem),
    reverse(RList, List), !.
%% eval//3
eval(Type, Code, RList) -->
    eval(Type, Code, [], RList).
%% eval//4
eval(_Type, [], List, List) -->
    pop_memory_block.
eval(Type, [block(Code)|Cs], List, List2) -->
    push_memory_block,
    eval(Type, Code, List, List1),
    eval(Type, Cs, List1, List2).
eval(Type, [C|Cs], List, List1) -->
    eval1(Type, C),
    memory_count(N),		% Write after every operation
    eval(Type, Cs, [N|List], List1).

memory_count(N, Mem, Mem) :-
    Mem = m(N, _).


eval_paths([], []) :- !.
eval_paths([P|Ps], [result(R, S, Q) | RSQ]) :-
    eval(region, P, RegionL), !,
    eval(stack, P, StackL), !,
    eval(queue, P, QueueL), !,
    avg(RegionL, R), avg(StackL, S), avg(QueueL, Q),
    eval_paths( Ps, RSQ), !.

best([], sq(0,0)).
best([result(_,S,Q)|Rs], RAs) :-
    best(Rs, sq(N,M)),
    N1 is N+1,
    M1 is M+1,
    (   S @> Q ->
	RAs = sq(N1, M)
    ;   Q @> S ->
	RAs = sq(N,M1)
    ;   RAs = sq(N,M)
    ).


evaluate_best(File, Result) :-
    paths(File, PS),
    length(PS,N),write(N), nl,
    eval_paths(PS, Mem),
    best(Mem, Result).

do_all([]) :- halt.
do_all([F|Fs]) :-
    (   file_property(F, type(regular)) ->
	evaluate_best(F, R1), write(R1),nl
    ;   true
    ), do_all(Fs).

do_all :-
    directory_files('./jruby/', J),
    atom_concat_all('./jruby/', J, J1),
    directory_files('./eclipse/', E),
    atom_concat_all('./eclipse/', E, E1),
    append(J1, E1, Fs),
    do_all(Fs).

:- initialization(do_all).
