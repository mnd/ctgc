%% -*- mode: prolog -*-
%%% parse.pl --- Parse java functions.

%% Copyright (C) 2012  Merinov Nikolay

:- include('helpers.pl').

read_tokens_file(File_Name, R) :- %read file and get tokens
    open(File_Name, read, Stream, [type(text), eof_action(eof_code)]),
    read_chars(Stream, R1),
    phrase(tokenize_java_file(R), R1),
    close(Stream), !.

read_chars(Stream,Ts) :-
    get_char(Stream, T),
    (   T = end_of_file ->
	Ts = []
    ;   read_chars(Stream, Fs),
	Ts = [T|Fs]
    ).
chars(X, R) --> [X1], {is_char(X1), atom_concat(X, X1, X2)}, chars(X2, R).
chars(X, X, R, R).

numbers(X, R) --> [X1], {is_number(X1), atom_concat(X, X1, X2)}, numbers(X2, R).
numbers(X, X, R, R).

alnums(X, R) --> [X1],
    { ( is_char(X1) ; is_number(X1) ), atom_concat(X, X1, X2) }, alnums(X2, R).
alnums(X, X, R, R).

at_sym_args --> ['('], simple_ignore_before(')').
at_sym_args(L,L).

tokenize_java_file([],[],[]).
tokenize_java_file(Rs) --> ['/','/'], simple_ignore_before('\n'),       tokenize_java_file(Rs).
tokenize_java_file(Rs) --> ['/','*'], simple_ignore_before('*'), ['/'], tokenize_java_file(Rs).
tokenize_java_file(Rs) --> ['\\', _], tokenize_java_file(Rs). %ignore escaped symbol
tokenize_java_file(Rs) --> ['@', X], {is_char(X)}, chars(X,_R), at_sym_args,
    tokenize_java_file(Rs). %ignore @directive
tokenize_java_file(R) --> [X], {is_space(X)}, tokenize_java_file(R).
tokenize_java_file([R|Rs]) --> [X], {is_char(X)}, alnums(X, R), tokenize_java_file(Rs).
tokenize_java_file([R|Rs]) --> [X], {is_number(X)}, numbers(X, R), tokenize_java_file(Rs).
tokenize_java_file([R|Rs]) --> [R], tokenize_java_file(Rs).



parse_java([], [], []) :- !.	    %parse java file
parse_java([]) --> ['}'], {!}.    %end of class
%% Ignore packages, imports, interfaces
parse_java(R) --> [import], {!}, ignore_before(';'), parse_java(R).
parse_java(R) --> [package], {!}, ignore_before(';'), parse_java(R).
parse_java(R) --> [interface], {!}, ignore_before('{'), ignore_before('}'), parse_java(R).
%% Static code block
parse_java(R) --> [static, '{'], {!}, ignore_before('}'), parse_java(R).
%% type modifiers
parse_java(R) --> type_modifier, {!}, parse_java(R).
%% Class
parse_java([class(ClassName,R1) | R2]) -->
    [class, ClassName], {!}, ignore_before('{'),
    parse_java(R1), parse_java(R2).
%% Variable. Ignore it
parse_java(R) --> type_name, parse_name(_), [';'], parse_java(R).
parse_java(R) --> type_name, parse_name(_), ['='], ignore_before(';'), parse_java(R).
%% Function
parse_java([func(Function_Name, As, Code)|Fs]) --> 
    ( type_name ; [] ), [Function_Name, '('],
    parse_definition_args(As),
    parse_body_function(As,Code),
    parse_java(Fs).

parse_body_function(_, [])    --> [';'].
parse_body_function(As, Code) -->
    simple_ignore_before('{'),
    parse_function(As, Code).

type_name --> [_, '<'], simple_ignore_before('>'), !.
type_name --> [_, '['], simple_ignore_before(']'), !.
type_name --> [_, '.'], type_name.
type_name --> [_].

type_modifier --> [public].
type_modifier --> [static].
type_modifier --> [private].
type_modifier --> [volatile].
type_modifier --> [abstract].
type_modifier --> [protected].
type_modifier --> [final].


parse_definition_args([]) --> [')'], {!}.
parse_definition_args(As) --> [','], parse_definition_args(As).
parse_definition_args(As) --> type_modifier, parse_definition_args(As).
parse_definition_args([A|As]) --> type_name, [A], parse_definition_args(As). 

parse_function(_, []) --> ['}'], {!}.	%end of function.
parse_function(Vars, X) --> type_modifier, parse_function(Vars, X).
parse_function(Vars, [expr(R) | X]) -->
    [return], {!}, parse_expression(E), {intersection(E, Vars, R)},
    parse_function(Vars, X).
parse_function(Vars, X) -->
    [continue, ';'],
    parse_function(Vars, X).
parse_function(Vars, X) -->
    [break, ';'],
    parse_function(Vars, X).
parse_function(Vars, [expr(R) | X]) -->
    [throw], {!}, parse_expression(E), {intersection(E, Vars, R)},
    parse_function(Vars, X).
parse_function(Vars, [if(expr(Cond), Then, Else) | X]) -->
    [if, '('], {!}, parse_expression(Cond1), {intersection(Cond1, Vars, Cond)},
    parse_then(Vars, Then), parse_else(Vars, Else),
    parse_function(Vars, X).
parse_function(Vars1, X) -->
    [for, '('], {!}, parse_for_expression(V, Cond1, Cond2),
    {   ( V = [] -> Vars = Vars1 ; Vars = [V|Vars1] ),
        intersection(Cond1, Vars, Cond11),
	intersection(Cond2, Vars, Cond21)
    },
    parse_body(Vars, Body),
    parse_function(Vars1, X1),
    {   V = [] ->
	X = [for([expr(Cond11), expr(Cond21)], Body) | X1]
    ;   X = [for([create(V,expr(Cond11)), expr(Cond21)], Body) | X1]
    }.
parse_function(Vars, [while([expr([]), expr(Cond)], Body) | X]) -->
    [while, '('], {!}, parse_expression(Cond1), {intersection(Cond1, Vars, Cond)},
    parse_body(Vars, Body),
    parse_function(Vars, X).
parse_function(Vars, [synchronized(Body) | X]) -->
    [synchronized, '('], {!},
    ignore_before(')'), ['{'],
    parse_function(Vars, Body),
    parse_function(Vars, X).
parse_function(Vars, [try(Body) | X]) -->
    [try, '{'], {!}, parse_function(Vars, Body),
    parse_function(Vars, X).
parse_function(Vars, X) -->	%ignore catch block
    [catch], {!}, ignore_before('{'),
    parse_function(Vars, _Body),
    parse_function(Vars, X).
parse_function(Vars, [finally(Body) | X]) -->
    [finally, '{'], {!}, parse_function(Vars, Body),
    parse_function(Vars, X).
parse_function(Vars, [switch([expr(Cond)], Body) | X]) -->
    [switch, '('], {!}, parse_expression(Cond1), {intersection(Cond1, Vars, Cond)},
    ['{'], parse_function(Vars, Body),
    parse_function(Vars, X).
parse_function(Vars, X) -->
    [case], {!}, simple_ignore_before(':'),
    parse_function(Vars, X).
parse_function(Vars, X) -->
    [default, ':'], {!},
    parse_function(Vars, X).
parse_function(Vars, X) -->
    [break, ';'], {!},
    parse_function(Vars, X).
%% TODO: Parse 'finaly' ...
parse_function(Vars, [create(V, expr([]))|X]) -->
    type_name, [V, ';'], parse_function([V|Vars], X).
parse_function(Vars, [create(V, expr(Vs))|X]) --> %create new local variable
    type_name, [V, '='], {sub_atom(V, 0, 1, _, V1), is_char(V1)},
    parse_expression(Vs1),
    { intersection(Vs1, [V|Vars], Vs) }, % count only variables from Args
    parse_function([V|Vars], X).
parse_function(Vars, Rs) --> %change local variable
    [V], (['='] ; ['+', '='] ; ['*', '='] ; ['/', '='] ; ['-', '=']),
    parse_expression(Vs1),
    { intersection(Vs1, Vars, Vs) }, % count only variables from Args
    {   memberchk(V, Vars) ->	     % change/2 when V from Vars
	Rs = [change(V, expr(Vs)) | X]
    ;   Rs = [expr(Vs) | X]
    },
    parse_function(Vars, X).
parse_function(Vars, Rs) --> %in place change local variable
    parse_name(V1s), (['='] ; ['+', '='] ; ['*', '='] ; ['/', '='] ; ['-', '=']),
    parse_expression(V2s),
    {   append(V1s,V2s, Vs1),
	intersection(Vs1, Vars, Vs), % count only variables from Args
	Rs = [expr(Vs) | X]
    },
    parse_function(Vars, X).
parse_function(Vars, [expr(Vs) | X]) -->
    parse_expression(Vs1),
    { intersection(Vs1, Vars, Vs) },
    parse_function(Vars, X).
parse_function(_Args, []) --> ignore_before('}').

parse_else(Vars, Else) --> [else, '{'], {!}, parse_function(Vars, Else).
parse_else(Vars, [if(expr(Cond), Then, Else)]) --> [else, if, '('], {!},
    parse_expression(Cond1), {intersection(Cond1, Vars, Cond)},
    parse_then(Vars, Then), parse_else(Vars, Else).
parse_else(Vars, [expr(Else)]) --> [else], {!},
    parse_expression(Else1), {intersection(Else1, Vars, Else)}.
parse_else(_, [], L, L).

parse_then(Vars, Then) -->
    ['{'], parse_function(Vars, Then).
parse_then(Vars, [expr(Then)]) -->
    parse_expression(Then1), {intersection(Then1, Vars, Then)}.

parse_body(Vars, Body) --> parse_then(Vars, Body).


parse_for_expression(V, Cond1, []) --> type_name, [V, ':'], parse_expression(Cond1).
parse_for_expression(V, Cond1, Cond) -->
    type_name, [V, '='], parse_expression(Cond1),
    parse_expression(Cond2), parse_expression(Cond3),
    { append(Cond2, Cond3, Cond) }.
parse_for_expression([], Cond1, Cond) -->
    parse_expression(Cond1),
    parse_expression(Cond2), parse_expression(Cond3),
    { append(Cond2, Cond3, Cond) }.
parse_for_expression([], Cond1, []) --> parse_expression(Cond1).

%% parse expression
parse_expression([]) --> [';'], {!}.	%end of expression
parse_expression([]) --> [')'], {!}.	%end of subexpression
parse_expression([]) --> [']'], {!}.	%end of other subexpression
parse_expression(As) --> ['{'], %function code subexpression
    ignore_before('}'), parse_expression(As).
%% new expression
parse_expression(As) -->
    [new], type_name, ['('], parse_args(As1), parse_expression(As2),
    {append(As1, As2, As)}.
parse_expression([]) -->
    [new], ignore_before(';').
parse_expression(As) -->
    [throw], parse_expression(As).
%% type converting
parse_expression(As) --> ['('], type_name, [')'], parse_expression(As).
%% arithmetic
parse_expression(As) -->
    operator,
    parse_expression(As).
parse_expression(As) -->
    name_or_call_or_expression(As1),
    parse_expression(As2),
    { append(As1, As2, As) }.	%variables in first + variables in last
%% WARNING: May be danger.
parse_expression([]) --> ignore_before(';'). % Some hard to parse expression. Just ignore.

name_or_call_or_expression(As) --> ['('], parse_expression(As).
name_or_call_or_expression(As) --> ['.'], function_call(As).
name_or_call_or_expression(As) --> function_call(As).
name_or_call_or_expression(['$string']) --> ['"'], simple_ignore_before('"').
name_or_call_or_expression(N) --> parse_name(N).

parse_name([N|Ns]) --> [N, '.'], parse_name(Ns).
parse_name([N|Ns]) -->
    [N, '['], parse_expression(Ns1), parse_sub_name(Ns2),
    {append(Ns1, Ns2, Ns)}.
parse_name([N|Ns]) --> [N, '['], parse_expression(Ns).
parse_name([N]) --> [N].

parse_sub_name(Ns) --> ['.'], parse_name(Ns).
parse_sub_name([],L,L).

function_call(As) -->
    [A], function_names,
    ['('], parse_args(As1), function_subcall(As2), {append([A|As1], As2, As)}.
function_call(As) -->
    [_Function_Name],		%not on object. Regular function.
    ['('], parse_args(As1), function_subcall(As2), {append(As1, As2, As)}.

function_subcall(As) -->	%func1().func2().func3()
    function_names, ['('], parse_args(As1),
    function_subcall(As2), {append(As1, As2, As)}.
function_subcall([], L, L).

function_names --> ['.', _Name], function_names. %% .subobj.subsubobj.func
function_names --> ['.', _Name]. %% .funcName

parse_args([]) --> [')'], {!}.
parse_args(As) --> [','], parse_args(As).
parse_args(As) --> [new], parse_args(As).
parse_args(As) --> name_or_call_or_expression(As1), {!}, parse_args(As2), {append(As1, As2, As)}.

operator --> ['+'].
operator --> ['-'].
operator --> ['*'].
operator --> ['/'].
operator --> ['='].
operator --> ['!'].
operator --> ['|'].
operator --> ['^'].
operator --> ['~'].
operator --> ['&'].
operator --> ['>', '>', '>'].
operator --> ['>', '>'].
operator --> ['<', '<'].
operator --> ['>'].
operator --> ['<'].
operator --> ['?'].
operator --> [':'].
operator --> [instanceof].

parse_java_file(Name, R) :-
    read_tokens_file(Name, R1), phrase(parse_java(R), R1).
