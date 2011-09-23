%% -*- mode: prolog -*-

:- include('helpers.pl').

%% Память будет представляться в виде функтора mem(Next_ID, [node(ID, Data)| ...])
memory(mem(Next_ID, Data_List)) :- number(Next_ID), node_list(Data_List).

%% Пустая память -- это память из одной ячейки NULL содержащей ссылку на себя.
empty_memory(mem(1, [node(0, ref(0))])) :- memory(mem(1, [node(0, ref(0))])).

%% Список ячеек памяти
node_list([]).
node_list([node(ID, Data)| Tail]) :- number(ID), data(Data), data_list(Tail).

%% Память типизирована и речь о том корректны ли данные внутри ячейки
data(num(X)) :- number(X).
data(ref(X)) :- number(X).
data(atom(X)) :- atom(X).
data(cons(X,Y)) :- data(X), data(Y).

%% Добавление данных в память. Порядок аргументов подобран для использования с DCG
%% TODO: проверять, что добавляемая ref ячейка консистентна?
mem_add(Data, ID, mem(ID, DList), mem(Next_ID, [node(ID, Data) | DList])) :-
	memory(mem(ID, DList)),
	Next_ID is ID + 1,
	memory(mem(Next_ID, [node(ID, Data) | DList])).

%% Удаление данных из памяти.
%% TODO: проверять остались ли ref ячейки консистентными?
mem_del(0, _, _) :- !, fail.			% NULL удалять нельзя
mem_del(ID, mem(Next_ID, DList), mem(Next_ID, Reduced_DList)) :-
	memory(mem(Next_ID, DList)),
	delete_unified(DList, node(ID,_), Reduced_DList), !, 
	memory(mem(Next_ID, Reduced_DList)).

%% Получение данных из памяти
mem_get(ID, Data, mem(Next_ID, Data_List)) :-
	memory(mem(Next_ID, Data_List)),
	once(member(node(ID, Data), Data_List)).

%% TODO: Если я не буду использовать DCG для обработки памяти -- удалить mem_get/4
mem_get(ID, Data, M, M) :- mem_get(ID, Data, M).

%% Изменение значения ячейки памяти.
mem_mod(ID, Data, mem(Next_ID, DList), mem(Next_ID, New_DList)) :-
	memory(mem(Next_ID, DList)),
	change_unified(DList, node(ID, _), node(ID, Data), New_DList), !,
	memory(mem(Next_ID, New_DList)).

