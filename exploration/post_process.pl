%% -*- mode: prolog -*-
%%% post_process.pl --- Extract Functions and add necessary destroy/1 instructions

%% Copyright (C) 2012  Merinov Nikolay

:- include('parse.pl').


%% extract all functions from class tree.
extract_functions([], []) :- !.
extract_functions([func(N,As,Code)|Us], [func(N,Code2)|R]) :-
    simplify_function_code(Code, Code1),
    args_to_creates(As, C1), append(C1, Code1, Code2),
    extract_functions(Us, R).
extract_functions([class(_Name,U1)|Us], R) :-
    extract_functions(U1, R1),
    extract_functions(Us, R2),
    append(R1, R2, R).

args_to_creates([],[]) :- !.
args_to_creates([A|As],[create(A, expr([])) | CAs]) :-
    args_to_creates(As, CAs).

%% replace all cycles with 'cycle(condition, body)'
%% replace all blocks with 'block(body)'
simplify_function_code([], []).
simplify_function_code([expr(E)|C1], [expr(E)|C2]) :-
    simplify_function_code(C1, C2).
simplify_function_code([create(V,E)|C1], [create(V,E)|C2]) :-
    simplify_function_code(C1, C2).
simplify_function_code([change(V,E)|C1], [change(V,E)|C2]) :-
    simplify_function_code(C1, C2).
simplify_function_code([if(E,Then,Else)|C1], [if(E, Then1, Else1)|C2]) :-
    simplify_function_code(Then, Then1),
    simplify_function_code(Else, Else1),
    simplify_function_code(C1, C2).
simplify_function_code([while(E,B)|C1], [cycle(E,B1)|C2]) :-
    simplify_function_code(B,B1),
    simplify_function_code(C1, C2).
simplify_function_code([for(E,B)|C1], [cycle(E,B1)|C2]) :-
    simplify_function_code(B,B1),
    simplify_function_code(C1, C2).
simplify_function_code([synchronized(B)|C1], [block(B1)|C2]) :-
    simplify_function_code(B,B1),
    simplify_function_code(C1, C2).
simplify_function_code([try(B)|C1], [block(B1)|C2]) :-
    simplify_function_code(B,B1),
    simplify_function_code(C1, C2).
simplify_function_code([finally(B)|C1], [block(B1)|C2]) :-
    simplify_function_code(B,B1),
    simplify_function_code(C1, C2).
simplify_function_code([switch(E,B)|C1], [block(B1)|C2]) :-
    append(E,B,B0),
    simplify_function_code(B0,B1),
    simplify_function_code(C1, C2).

add_destroy_instructions([], []).
add_destroy_instructions([func(N,Code)|Fs], [func(N,Code1)|Fs1]) :-
    add_destroy_to_block_top(Code, Code1),
    add_destroy_instructions(Fs, Fs1).

add_destroy_to_block_top(Code, Code1) :-
    add_destroy_to_block_top([], Code, Code1).
add_destroy_to_block_top(V1, Code, Code1) :-
    collect_top_level_create(Code, Vars),
    reverse(Code, RCode),
    append(V1, Vars, Vars1),
    add_destroy_to_block(Vars1, RCode, RCode1),
    reverse(RCode1, Code1).
    
%% collect 'create/2' on top level
collect_top_level_create([], []).
collect_top_level_create([create(N,_)|Cs], [N|Vs]) :-
    collect_top_level_create(Cs, Vs).
collect_top_level_create([_|Cs], Vs) :- 
    collect_top_level_create(Cs, Vs).


add_destroy_to_block([], [], []).
add_destroy_to_block(Vars, [], [destroy(Vars)]).
add_destroy_to_block(Vars, [C|Cs], R) :-
    C = expr(Ls),
    intersection(Vars, Ls, V1),	%vars that used last time
    (   V1 = [] ->
	R = [C | Cs1]
    ;   R = [destroy(V1), C | Cs1]
    ),
    subtraction(Vars, V1, Vars1),
    add_destroy_to_block(Vars1, Cs, Cs1).
add_destroy_to_block(Vars, [C|Cs], R) :-
    C = change(V, expr(Ls)),
    intersection(Vars, [V|Ls], V1),	%vars that used last time
    (   V1 = [] ->
	R = [C | Cs1]
    ;   R = [destroy(V1), C | Cs1]
    ),
    subtraction(Vars, V1, Vars1),
    add_destroy_to_block(Vars1, Cs, Cs1).
add_destroy_to_block(Vars, [C|Cs], R) :-
    C = create(V, expr(Ls)),
    intersection(Vars, [V|Ls], V1),	%vars that used last time
    (   V1 = [] ->
	R = [C | Cs1]
    ;   R = [destroy(V1), C | Cs1]
    ),
    subtraction(Vars, V1, Vars1),
    add_destroy_to_block(Vars1, Cs, Cs1).
add_destroy_to_block(Vars, [block(Bs)|Cs], [block(Bs1) | Cs1]) :-
    collect_used_vars([], Bs, Vs),
    intersection(Vars, Vs, V1),
    add_destroy_to_block_top(V1, Bs, Bs1), %propogate destroy into block
    subtraction(Vars, V1, Vars1),
    add_destroy_to_block(Vars1, Cs, Cs1).
add_destroy_to_block(Vars, [cycle(Hs, Bs)|Cs], R) :-
    ( Hs = [create(N,_)|_] -> PreCreate = [N] ; PreCreate = [] ),
    collect_used_vars(PreCreate, Hs, Vs1),
    collect_used_vars(PreCreate, Bs, Vs2),
    append(Vs1,Vs2,Vs),
    intersection(Vars, Vs, V1),
    add_destroy_to_block_top(Bs, Bs1),
    append(PreCreate, V1, V2),	%destroy pre created variables after cycle.
    (   V2 = [] ->
	R = [cycle(Hs, Bs1, expr([])) | Cs1]
    ;   R = [cycle(Hs, Bs1, destroy(V2)) | Cs1]
    ),
    subtraction(Vars, V1, Vars1),
    add_destroy_to_block(Vars1, Cs, Cs1).
add_destroy_to_block(Vars, [if(expr(IF), Then, Else)|Cs], [if(expr(IF), Then1, Else1)|Cs1]) :-
    collect_used_vars([], Then, TV),
    collect_used_vars([], Else, EV),
    append(IF, TV, ITV), append(ITV, EV, Vs), %all vars
    intersection(Vars, Vs, V1),		      %used in if_then_else
    add_destroy_to_block_top(V1, Then, Then1),    %propagate to Then
    add_destroy_to_block_top(V1, Else, Else1),    %                  and Else blocks
    subtraction(Vars, V1, Vars1),
    add_destroy_to_block(Vars1, Cs, Cs1).

%% Collect variable names used in block.
collect_used_vars(_, [], []).
collect_used_vars(IgnoreVars, [expr(Vs) | Bs], R) :-
    subtraction(Vs, IgnoreVars, Vs1),
    collect_used_vars(IgnoreVars, Bs, R1),
    append(Vs1, R1, R).
collect_used_vars(IgnoreVars, [create(V,expr(Vs)) | Bs], R) :-
    IgnoreVars1 = [V|IgnoreVars],
    subtraction(Vs, IgnoreVars1, Vs1),
    collect_used_vars(IgnoreVars1, Bs, R1),
    append(Vs1, R1, R).
collect_used_vars(IgnoreVars, [change(V,expr(Vs)) | Bs], R) :-
    subtraction([V|Vs], IgnoreVars, Vs1),
    collect_used_vars(IgnoreVars, Bs, R1),
    append(Vs1, R1, R).
collect_used_vars(IgnoreVars, [block(B) | Bs], R) :-
    collect_used_vars(IgnoreVars, B, R1),
    collect_used_vars(IgnoreVars, Bs, R2),
    append(R1, R2, R).
collect_used_vars(IgnoreVars, [cycle(C, B) | Bs], R) :-
    (   C = [create(V,_) | _] ->
	IgnoreVars1 = [V|IgnoreVars]
    ;   IgnoreVars1 = IgnoreVars
    ),
    collect_used_vars(IgnoreVars1, C, R1),
    collect_used_vars(IgnoreVars1, B, R2),
    collect_used_vars(IgnoreVars, Bs, R3),    
    append(R1, R2, R12), append(R12, R3, R).
collect_used_vars(IgnoreVars, [if(expr(Cs), Then, Else) | Bs], R) :-
    subtraction(Cs, IgnoreVars, Cs1),
    collect_used_vars(IgnoreVars, Then, R1),
    collect_used_vars(IgnoreVars, Else, R2),
    collect_used_vars(IgnoreVars, Bs, R3),
    append(Cs1, R1, R01), append(R2, R3, R23), append(R01, R23, R).

%% extract paths. Replace cycle/3 with 10 block/1. Replace if/3 with 2 paths.
extract_all_paths([],[]).
extract_all_paths([func(N,P) | Fs], [func(N, paths(Ps)) | Fs1]) :-
    extract_all_paths_block(P, Ps),
    extract_all_paths(Fs, Fs1).


extract_all_paths_block([], [[]]).
extract_all_paths_block([cycle([PRE,IN], Body, POST) | Bs], R) :-
    iterate(10, [IN|Body], Body1),
    append([PRE|Body1], [POST], Body2),
    extract_all_paths_block(Body2, Body3),
    blockise_all(Body3, Body4),
    extract_all_paths_block(Bs, R1),
    forall_append_to_all(Body4, R1, R).
extract_all_paths_block([if(E, Then, Else) | Bs], R) :-
    extract_all_paths_block([E|Then], Then1),
    extract_all_paths_block([E|Else], Else1),
    extract_all_paths_block(Bs, R1),
    blockise_all(Then1, Then2),
    blockise_all(Else1, Else2),
    forall_append_to_all(Then2, R1, R2),
    forall_append_to_all(Else2, R1, R3),
    append(R2, R3, R).
extract_all_paths_block([block(Body) | Bs], R) :-
    extract_all_paths_block(Body, Body1),
    extract_all_paths_block(Bs, R1),
    blockise_all(Body1, Body2),
    forall_append_to_all(Body2, R1, R).
extract_all_paths_block([B|Bs], R) :-
    extract_all_paths_block(Bs, R1),
    append_to_all(B, R1, R).

only_paths([],[]).
only_paths([func(_,paths(Ps)) | Fs], R) :-
    only_paths(Fs, R1),
    append(Ps, R1, R).

%% test
test :-
    test('test.java').
test(Name) :-
    paths(Name, R),
    format_code(R), !.

paths(Name, Paths) :-
    parse_java_file(Name, R1),
    extract_functions(R1, R2),
    add_destroy_instructions(R2, R3),
    %% filter_func_with_change_instructions,
    extract_all_paths(R3,R4),
    only_paths(R4,Paths).

q1(Name, R3) :-
    parse_java_file(Name, R1),
    extract_functions(R1, R2),
    add_destroy_instructions(R2, R3).