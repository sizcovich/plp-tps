% Definiciones de operadores.
:- op(900,xfy, [ + ]).
:- op(800,xfy, [ * ]).

% Implementación de los predicados.

%acciones(+Proceso,-Acciones)
acciones(0,[]).
acciones(tau*P,L) :- acciones(P,L).
acciones(Mu*P,[Mu|L]) :- Mu \= tau, acciones(P,L).
acciones(P+Q,L) :- acciones(P,L1), acciones(Q,L2), append(L1,L2,L).

%reduce(+Proceso1,?Accion,?Proceso2)
reduce(Mu*P,Mu,P).
reduce(P+_,Mu,P1) :- acciones(P,Acciones),append([Mu],_,Acciones),reduce(P,Mu,P1).
reduce(_+Q,Mu,Q1) :- acciones(Q,Acciones),append([Mu],_,Acciones),reduce(Q,Mu,Q1).

%reduceLista(+Proceso1,?Cadena,?Proceso2)
reduceLista(P,[],P).
reduceLista(P1,L,P2) :- reduce(P1,X,T), X == tau, reduceLista(T,L,P2).
reduceLista(P1,[X|L],P2) :- reduce(P1,X,T), X \= tau, reduceLista(T,L,P2).

%prefijos(+L,?P)
prefijos([],[[]]).
prefijos(L,P) :- append(P,_,L).

%calculoDeTrazas(+Proceso, -Cadenas)
calculoDetrazas(tau,[[]]).
calculoDetrazas(Proc,L) :- reduceLista(Proc,L,0).
calculoDetrazas(P+Q,L):- calculoDetrazas(P,L1), calculoDetrazas(Q,L2), append([L1],[L2],L).

%trazas(+Proceso, -Cadenas)
trazas(Proc,L) :- setof(K,calculoDetrazas(Proc,K),L).

% Tests (van un par de ejemplos, agreguen los suyos).

test(0) :- not((acciones(0, L), member(_,L))).

test(1) :- reduceLista(0,[],0).

test(2) :- not(puedeReemplazarA(moneda * (te * 0 + cafe * 0), moneda * te * 0 + moneda * cafe * 0)).
test(3) :- puedeReemplazarA(tau*a*0, a*0).

test(4) :- equivalentes(a*b*(c*0+d*0), a*b*(d*tau*0+c*0)).
test(5) :- not(equivalentes(a*b*0, b*a*0)).
test(6) :- not(acciones(c*((a*0) + (b*tau*0)),[])).

tests :- forall(between(0, 5, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.