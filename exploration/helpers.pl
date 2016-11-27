%% -*- mode: prolog -*-
%%% helpers.pl --- Define parser helpers functions.

%% Copyright (C) 2012  Merinov Nikolay

atom_concat_all(_, [], []).
atom_concat_all(A, [F|Fs], [R|Rs]) :-
    atom_concat(A, F, R),
    atom_concat_all(A, Fs, Rs).

avg(L, R) :-
    length(L, Len),
    (   Len = 0 ->
	R = 0
    ;   sum(L, Sum),
	R is Sum / Len
    ).

sum([], 0).
sum([X|L], Sum) :-
    sum(L, Sum1),
    Sum is Sum1 + X.

is_space(' ').
is_space('\t').
is_space('\r').
is_space('\n').

is_char(X) :- 
    char_code(X, R),
    char_code('a', AL),
    char_code('z', ZL),
    char_code('A', AU),
    char_code('Z', ZU),
    char_code('_', Under),
    (   R >= AL, R =< ZL
    ;   R >= AU, R =< ZU
    ;   R =:= Under
    ), !.

is_number('0').
is_number('1').
is_number('2').
is_number('3').
is_number('4').
is_number('5').
is_number('6').
is_number('7').
is_number('8').
is_number('9').

is_punct(X) :- \+ is_char(X), \+ is_number(X), \+ is_space(X).

simple_ignore_before(X) --> [X].
simple_ignore_before(X) --> [_], simple_ignore_before(X).

ignore_before(X) --> [X].
ignore_before(X) --> ['\"'],  simple_ignore_before('\"'),  ignore_before(X).
ignore_before(X) --> ['\''], simple_ignore_before('\''), ignore_before(X).
ignore_before(X) --> ['('], ignore_before(')'), ignore_before(X).
ignore_before(X) --> ['{'], ignore_before('}'), ignore_before(X).
ignore_before(X) --> ['['], ignore_before(']'), ignore_before(X).
ignore_before(X) --> [_], ignore_before(X).

simple_word_before(X, W) --> simple_word_before(X, '$noname', W).

simple_word_before(X, W, W) --> [X].
simple_word_before(X, _, W) --> 
    [W1], simple_word_before(X, W1, W).

subtraction([],_,[]).
subtraction([A|As], Bs, Rs) :-
    memberchk(A, Bs), subtraction(As, Bs, Rs).
subtraction([A|As], Bs, [A|Rs]) :-
    subtraction(As, Bs, Rs).

intersection([],_,[]).
intersection([A|As], Bs, [A|Rs]) :-
    memberchk(A, Bs), intersection(As, Bs, Rs).
intersection([_|As], Bs, Rs) :- intersection(As, Bs, Rs).

iterate(0, _, []) :- !.
iterate(N, E, R) :-
    N1 is N-1,
    iterate(N1, E, Es),
    append(E, Es, R).

append_to_all(_,[],[]).
append_to_all(X, [Y|Ys], [[X|Y]|Rs]) :-
    append_to_all(X, Ys, Rs).

blockise_all([],[]).
blockise_all([X|Xs], [block(X) | Ys]) :- blockise_all(Xs, Ys).

forall_append_to_all([], _, []).
forall_append_to_all([X|Xs], Y, R) :-
    append_to_all(X, Y, R1),
    forall_append_to_all(Xs, Y, R2),
    append(R1, R2, R).

format_code(Cs) :- format_code('', Cs).
format_code(P, []) :- write(P), write('[]'), nl.
format_code(P, Cs) :-
    list(Cs),
    write(P), write('['), nl,
    atom_concat(P, '  ', P1),
    format_code_list(P1, Cs),
    write(P), write(']'), nl.
format_code(P, class(Name, Code)) :-
    write(P), write('class('), write(Name), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Code),
    write(P), write(')'), nl, nl.
format_code(P, if(Cond, Then, Else)) :-
    write(P), write('if('), write(Cond), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Then),
    write(P), write('else'), nl,
    format_code(P1, Else),
    write(P), write(')'), nl.
format_code(P, for(Cond, Body)) :-
    write(P), write('for('), write(Cond), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P), write(')'), nl.
format_code(P, try(Body)) :-
    write(P), write('try('), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P), write(')'), nl.
format_code(P, synchronized(Body)) :-
    write(P), write('synchronized(somevar,'), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P), write(')'), nl.
format_code(P, while(Cond, Body)) :-
    write(P), write('while('), write(Cond), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P), write(')'), nl.
format_code(P, func(Name, As, Code)) :-
    write(P), write('func('), write(Name), write(', '), write(As), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Code),
    write(P), write(')'), nl, nl.
%%post process format
format_code(P, func(Name, Code)) :-
    write(P), write('func('), write(Name), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Code),
    write(P), write(')'), nl, nl.
format_code(P, cycle(Cond, Body)) :-
    write(P), write('cycle('), write(Cond), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P), write(')'), nl.
format_code(P, block(Body)) :-
    write(P), write('block('), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P), write(')'), nl.
%% 
format_code(P, paths(Bs)) :-
    write(P), write('paths('), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Bs),
    write(P), write(')'), nl.
format_code(P, cycle(Cond, Body, Post)) :-
    write(P), write('cycle('), write(Cond), write(','), nl,
    atom_concat(P, '  ', P1),    
    format_code(P1, Body),
    write(P1), write(', '), write(Post), nl,
    write(P), write(')'), nl.
%% stoper
format_code(P, C) :- write(P), write(C), nl.

format_code_list(_, []).
format_code_list(P, [C|Cs]) :-
    format_code(P, C),
    format_code_list(P,Cs).