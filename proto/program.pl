%% -*- mode: prolog -*-
:- include('operations.pl').

% Usage: start(main)
function(fact, Q, [RQ], [V, Result],
[ if(Q1, V,
     [ new(Q1, V1),
       sub(V, 1, V1),
       enqueue(Q1),
       call(fact, [Q1], [V1, R1]),
       dequeue(Q1), % free V1
       new(RQ, Result),
       mul(R1, V, Result)
     ],
     [ new(RQ, Result, 1)
     ])
]).

function(main, Q, [], [],
[ new(Q, N, 10),
  enqueue(Q),
  print(N),
  call(fact, [Q], [N, Result]),
  dequeue(Q), % free N
  print(Result)
]).

