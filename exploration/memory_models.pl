%% -*- mode: prolog -*-
%%% memory_models.pl --- N models of memory.

%% Copyright (C) 2012  Merinov Nikolay

:- include('../helpers.pl').

%% All models is stack of somethinh.

%% Used structure is:
%% 
%% m(var_count, [
%%                r(var_count, [
%%                               v(var, is_deleted)
%%                             ])
%%              ])

%% generic methods
empty_memory(m(0, [])).

%% block/1
push_memory_block(m(N, Rs), m(N, [r(0,[]) | Rs])).
pop_memory_block(m(N, [r(M, Vs) | Rs]), m(N1, Rs)) :-
    check_all_deleted(Vs),
    N1 is N - M.
check_all_deleted([]).
check_all_deleted([v(_, deleted) | Vs]) :- check_all_deleted(Vs).

%% create/2
create_variable(Var,		% (var, Old_Mem, New_Mem)
		m(N,  [r(M,                 Vs)  | Rs]),
		m(N1, [r(M1, [v(Var, exist)|Vs]) | Rs])) :-
    N1 is N + 1,
    M1 is M + 1.

%% First is stack of regions.
%% destroy/1
delete_variable_region(Var, m(N, Rs), m(N, Rs1)) :-
    %% actual deleting not happend before pop_region
    delete_variable_from_regions(Var, Rs, Rs1).
delete_variable_from_regions(V, [], _) :-
    atom_concat('variable for deleting not found: ', V, R),
    throw(R).
delete_variable_from_regions(Var, [R|Rs], [R1|Rs]) :-
    R = r(N, Vs),
    member_unified(Vs, v(Var, exist)),
    change_unified(Vs, v(Var, exist), v(Var, deleted), Vs1),
    R1 = r(N, Vs1).
delete_variable_from_regions(Var, [R|Rs], [R|Rs1]) :-
    delete_variable_from_regions(Var, Rs, Rs1).
%% change/2
change_variable_region(Var, m(N, Rs), m(N1, Rs1)) :-
    N1 is N + 1,
    change_variable_on_regions(Var, Rs, Rs1).
change_variable_on_regions(V, [], []) :-
    atom_concat('Variable for change not found: ', V, R),
    throw(R).
change_variable_on_regions(Var, [R|Rs], [R1|Rs]) :-
    R = r(N,Vs),
    member_unified(Vs, v(Var, exist)),
    change_unified(Vs, v(Var, exist), v(Var, deleted), Vs1),
    N1 is N + 1,
    R1 = r(N1, [v(Var, exist) | Vs1]).
change_variable_on_regions(Var, [R|Rs], [R|Rs1]) :-
    change_variable_on_regions(Var, Rs, Rs1).
    
%% Second is stack of stacks
%% destroy/1
delete_variable_stack(Var, m(N, Ss), m(N1, Ss1)) :-
    delete_variable_from_stacks(Var, Ss, Ss1, M),
    N1 is N - M.		% decrease to number of poped vars
delete_variable_from_stacks(_, [], _, _) :-
    throw('variable for deleting not found').
delete_variable_from_stacks(Var, [S | Ss], [S1 | Ss], M) :-
    S = r(N, Vs),
    member_unified(Vs, v(Var, exist)),
    change_unified(Vs, v(Var, exist), v(Var, deleted), Vs1),
    pop_deleted_from_stack(M, Vs1, Vs2),
    N1 is N - M,
    S1 = r(N1, Vs2).
delete_variable_from_stacks(Var, [S | Ss], [S | Ss1], M) :-
    delete_variable_from_stacks(Var, Ss, Ss1, M).
pop_deleted_from_stack(0, [], []).
pop_deleted_from_stack(0, [v(Var, exist) | Vs], [v(Var, exist) | Vs]).
pop_deleted_from_stack(N, [v(_,deleted) | Vs], Vs1) :-
    pop_deleted_from_stack(N1, Vs, Vs1),
    N is N1 + 1.
%% change/2
change_variable_stack(Var, m(N, Rs), m(N1, Rs1)) :-
    change_variable_on_stacks(Var, Rs, Rs1, M),
    N1 is N + M.
change_variable_on_stacks(_, [], _, _) :-
    throw('Variable for change not found').
change_variable_on_stacks(Var, [S|Ss], [S1|Ss], M) :-
    S = r(N,Vs),
    member_unified(Vs, v(Var, exist)),
    change_unified(Vs, v(Var, exist), v(Var, deleted), Vs1),
    pop_deleted_from_stack(M1, Vs1, Vs2),
    M is 1 - M1,
    N1 is N + M,
    S1 = r(N1, [v(Var, exist)|Vs2]).
change_variable_on_stacks(Var, [S|Ss], [S|Ss1], M) :-
    change_variable_on_stacks(Var, Ss, Ss1, M).

%% Third is stack of queues
%% destroy/1
delete_variable_queue(Var, m(N, Ss), m(N1, Ss1)) :-
    delete_variable_from_queue(Var, Ss, Ss1, M),
    N1 is N - M.		% decrease to number of shifted vars
delete_variable_from_queue(_,[],[],_) :-
    throw('variable for deleting not found').
delete_variable_from_queue(Var, [Q|Qs], [Q1|Qs], M) :-
    Q = r(N,Vs),
    member_unified(Vs, v(Var, exist)),
    change_unified(Vs, v(Var, exist), v(Var, deleted), Vs1),
    shift_deleted_from_queue(M, Vs1, Vs2),
    N1 is N - M,
    Q1 = r(N1, Vs2).
delete_variable_from_queue(Var, [Q|Qs], [Q|Qs1], M) :-
    delete_variable_from_queue(Var, Qs, Qs1, M).
shift_deleted_from_queue(M, Vs1, Vs2) :-
    reverse(Vs1, Vsr1),
    shift_deleted_from_queue_reverse(M, Vsr1, Vsr2),
    reverse(Vsr2, Vs2).
shift_deleted_from_queue_reverse(0, [], []).
shift_deleted_from_queue_reverse(0, [v(Var, exist)|Vs], [v(Var, exist)|Vs]).
shift_deleted_from_queue_reverse(M, [v(_, deleted) | Vs], Vs1) :-
    shift_deleted_from_queue_reverse(M1, Vs, Vs1),
    M is M1 + 1.
%% change/2
change_variable_queue(Var, m(N, Qs), m(N1, Qs1)) :-
    change_variable_on_queues(Var, Qs, Qs1, M),
    N1 is N + M.
change_variable_on_queues(_, [], [], _) :-
    throw('Variable for change not found').
change_variable_on_queues(Var, [Q|Qs], [Q1|Qs], M) :-
    Q = r(N,Vs),
    member_unified(Vs, v(Var, exist)),
    change_unified(Vs, v(Var, exist), v(Var, deleted), Vs1),
    shift_deleted_from_queue(M1, Vs1, Vs2),
    M = 1 - M1,
    N1 is N + M,
    Q1 = r(N1, [v(Var, exist)|Vs2]).
change_variable_on_queues(Var, [Q|Qs], [Q|Qs1], M) :-
    change_variable_on_queues(Var, Qs, Qs1, M).