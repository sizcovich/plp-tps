% Definiciones de operadores.
:- op(900,xfy, [ + ]).
:- op(800,xfy, [ * ]).

% Implementación de los predicados.

%acciones(+Proceso,-Acciones)
acciones(0,[]).
acciones(tau*P,L) :- acciones(P,L).
acciones(Mu*P,[Mu|L]) :- Mu \= tau, acciones(P,L).
acciones(P+Q,L) :- acciones(P,L1), acciones(Q,L2), append(L1,L2,L).


%me parece que no hace falta acciones y el append en este metodo.
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
%este caso base no va.
calculoDetrazas(Proc,L) :- reduceLista(Proc,M,0), prefijos(M,L).
calculoDetrazas(P+_,L1):- calculoDetrazas(P,L1).
calculoDetrazas(_+Q,L2):- calculoDetrazas(Q,L2).

%trazas(+Proceso, -Cadenas)
trazas(Proc,M) :- setof(K,calculoDetrazas(Proc,K),M).


%residuo(+X,+Cadena,-Qs)
residuo(Proceso, Cadena, Ps2) :- setof(X, reduceLista(Proceso, Cadena, X), Ps2).
residuo(Lss, Cadena, Residuos) :- consumirCadenaEnProcesos(Lss, Cadena, Residuos).
residuo(Proceso, Cadena, []) :- not(reduceLista(Proceso,Cadena,_)).

consumirCadenaEnProcesos([Ps], Cadena, Ps2) :- residuo(Ps, Cadena, Ps2).
consumirCadenaEnProcesos([Ps | Pss], Cadena, Residuos) :- residuo(Ps, Cadena, Ps2), consumirCadenaEnProcesos(Pss, Cadena, ResiduoPs), union(Ps2, ResiduoPs, Residuos).

isList([_|_]).
isList([]).

must(P, Ls) :- isList(P), mustList(P,Ls).
must(P, Ls) :- not(isList(P)), mustOneProcess(P,Ls).

mustList([], _).
mustList([Proceso | Pss], Ls) :- mustOneProcess(Proceso, Ls), mustList(Pss, Ls).

mustOneProcess(0, _).
mustOneProcess(Proceso, [X | Ls]) :-  residuo(Proceso, [X], ProcesoResiduo), ProcesoResiduo \= [] ; mustOneProcess(Proceso, Ls).

puedeReemplazarA(P,Q) :- getTrazasPQ(P, Q, TrazasPyQ), getAccionesPQ(P, Q, AccionesPyQ),  append(L, _, AccionesPyQ), not(probarTrazas(P, Q, TrazasPyQ, L)). 
getTrazasPQ(P, Q, TrazasPyQ) :- trazas(P, TrazasP), trazas(Q, TrazasQ), union(TrazasP, TrazasQ, TrazasPyQ).
getAccionesPQ(P, Q, AccionesPyQ) :- acciones(P, AccionesP), acciones(Q, AccionesQ), union(AccionesP,AccionesQ, AccionesPyQ).


probarTrazas(_, _, [] , _).

probarTrazas(P, Q, [Traza | TrazasPyQ] , AccionesPyQ) :- residuo(P, Traza, PResiduos) , residuo(Q, Traza, QResiduos), must(PResiduos, AccionesPyQ), not(must(QResiduos, AccionesPyQ)) ; probarTrazas(P, Q, TrazasPyQ , AccionesPyQ).


equivalentes(P, Q) :- puedeReemplazarA(P,Q), puedeReemplazarA(Q,P).

% Tests (van un par de ejemplos, agreguen los suyos).
test(0) :- not((acciones( 0 , L ), member( _ , L ))).

test(1) :- reduceLista(0,[],0).

test(2) :- not(puedeReemplazarA(moneda * (te * 0 + cafe * 0), moneda * te * 0 + moneda * cafe * 0)).
test(3) :- puedeReemplazarA(tau*a*0, a*0).

test(4) :- equivalentes(a*b*(c*0+d*0), a*b*(d*tau*0+c*0)).
test(5) :- not(equivalentes(a*b*0, b*a*0)).
test(6) :- not(acciones(c*((a*0) + (b*tau*0)),[])).

tests :- forall(between(0, 5, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.