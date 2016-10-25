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
reduce(P+_,Mu,P1) :- reduce(P,Mu,P1).
reduce(_+Q,Mu,Q1) :- reduce(Q,Mu,Q1).

%reduceLista(+Proceso1,?Cadena,?Proceso2)
reduceLista(P,[],P).
reduceLista(P1,L,P2) :- reduce(P1,X,T), X == tau, reduceLista(T,L,P2).
reduceLista(P1,[X|L],P2) :- reduce(P1,X,T), X \= tau, reduceLista(T,L,P2).

%prefijos(+L,?P)
prefijos([],[[]]).
prefijos(L,P) :- append(P,_,L).

%calculoDeTrazas(+Proceso, -Cadenas)
calculoDetrazas(Proc,L) :- reduceLista(Proc,M,0), prefijos(M,L).
calculoDetrazas(P+_,L1):- calculoDetrazas(P,L1).
calculoDetrazas(_+Q,L2):- calculoDetrazas(Q,L2).

%trazas(+Proceso, -Cadenas)
trazas(Proc,M) :- setof(K,calculoDetrazas(Proc,K),M).

%residuo(+X,+Cadena,-Qs)
residuo(Proceso, Cadena, Ps2) :- setof(X, reduceLista(Proceso, Cadena, X), Ps2).
residuo(Lss, Cadena, Residuos) :- consumirCadenaEnProcesos(Lss, Cadena, Residuos).
residuo(Proceso, Cadena, []) :- not(reduceLista(Proceso,Cadena,_)).

%consumirCadenaEnProcesos(+Pss, +Cadena, ?Residuos)
consumirCadenaEnProcesos([Ps], Cadena, Residuos) :- residuo(Ps, Cadena, Residuos).
consumirCadenaEnProcesos([Ps | Pss], Cadena, Residuos) :- residuo(Ps, Cadena, ResiduosPs), consumirCadenaEnProcesos(Pss, Cadena, ResiduoPss), union(ResiduosPs, ResiduoPss, Residuos).

%must(+P,+L).
must(P, Ls) :- is_list(P), mustList(P,Ls).
must(P, Ls) :- not(is_list(P)), mustOneProcess(P,Ls).

%mustList(+Pss, +Ls)
mustList([], _).
mustList([Proceso | Pss], Ls) :- mustOneProcess(Proceso, Ls), mustList(Pss, Ls).

%mustOneProcess(+P, +Ls)
mustOneProcess(0, _).
mustOneProcess(tau*P, Ls) :- mustOneProcess(P, Ls).
mustOneProcess(Proceso, [X | Ls]) :-  residuo(Proceso, [X], ProcesoResiduo), ProcesoResiduo \= [] ; mustOneProcess(Proceso, Ls).

%La idea de esta funcion es la de primero generar todas las trazas, luego todas las acciones,y luego 
% como todo esta instanciado a la hora de llamar a algunaTrazaYSubDeAccRompen el comportamiento del not sera
% analogo al NOT logico. algunaTrazaYSubDeAccRompen lo que hace es tratar de encontrar una traza y un conjunto de acciones tal que al aplicarle la traza a ambos procesos P y Q
% dicho conjunto de acciones (generados todos en generarYProbarSubsetsDeAcciones) rompa la negacion del predicado que en realidad debe cumplirse
% con encontrar una combinacion de Traza + Acciones que rompa el predicado este dara algunaTrazaYSubDeAccRompen dara TRUE
% y el not hara que de FALSE puedeReemplazarA, implicando justamente que no puede ser reemplazado P por Q.
% Ahora bien, si da false el predicado algunaTrazaYSubDeAccRompen, significa que no pudo cumplirse la negacion del predicado a cumplir del enunciado,
% lo cual significa que para todo conjunto de acciones y trazas posibles se cumple la definicion de puedeReemplazarA sin negar, haciendo que finalmente el not que encierra a algunaTrazaYSubDeAccRompen
% de TRUE, implicando en el TRUE de puedeReemplazarA.
%puedeReemplazarA(+P, +Q)
puedeReemplazarA(P,Q) :- getTrazasPQ(P, Q, TrazasPyQ), getAccionesPQ(P, Q, AccionesPyQ),  not(algunaTrazaYSubDeAccRompen(P, Q, TrazasPyQ, AccionesPyQ)). 
	
%algunaTrazaYSubDeAccRompen(+P, +Q, +Ts, +Accs)
algunaTrazaYSubDeAccRompen(P, Q, [Traza | TrazasPyQ] , AccionesPyQ) :- residuo(P, Traza, PResiduos) , residuo(Q, Traza, QResiduos), generarYProbarSubsetsDeAcciones(PResiduos, QResiduos, AccionesPyQ); algunaTrazaYSubDeAccRompen(P, Q, TrazasPyQ , AccionesPyQ).

%generarYProbarSubsetsDeAcciones(+PResiduos, +QResiduos, +AccionesPyQ)
generarYProbarSubsetsDeAcciones(PResiduos, QResiduos, AccionesPyQ) :- setof(X, subseq0(AccionesPyQ, X), SubSets), anySubsetMatches(PResiduos, QResiduos, SubSets).

%anySubsetMatches(+PResiduos, +QResiduos, +Ssets)
anySubsetMatches(PResiduos,QResiduos,[Sset | Ssets]) :- must(PResiduos, Sset), not(must(QResiduos, Sset)); anySubsetMatches(PResiduos,QResiduos,Ssets).

%subseq0(+List, ?List)
subseq0(List, List).
subseq0(List, Rest) :- subseq1(List, Rest).

%subseq0(+List, ?Rest)
subseq1([_|Tail], Rest) :- subseq0(Tail, Rest).
subseq1([Head|Tail], [Head|Rest]) :- subseq1(Tail, Rest).

%puedeReemplazarA(+P, +Q, ?TrazasPyQ)
getTrazasPQ(P, Q, TrazasPyQ) :- trazas(P, TrazasP), trazas(Q, TrazasQ), union(TrazasP, TrazasQ, TrazasPyQ).

%getAccionesPQ(+P, +Q, ?AccionesPyQ)
getAccionesPQ(P, Q, AccionesPyQ) :- acciones(P, AccionesP), acciones(Q, AccionesQ), union(AccionesP,AccionesQ, AccionesPyQ).


%equivalentes(+P, +Q)
equivalentes(P, Q) :- puedeReemplazarA(P,Q), puedeReemplazarA(Q,P).


% Tests (van un par de ejemplos, agreguen los suyos).
test(0) :- not((acciones( 0 , L ), member( _ , L ))).

test(1) :- reduceLista(0,[],0).

test(2) :- not(puedeReemplazarA(moneda * (te * 0 + cafe * 0), moneda * te * 0 + moneda * cafe * 0)).
test(3) :- puedeReemplazarA(tau*a*0, a*0).

test(4) :- equivalentes(a*b*(c*0+d*0), a*b*(d*tau*0+c*0)).
test(5) :- not(equivalentes(a*b*0, b*a*0)).
test(6) :- not(acciones(c*((a*0) + (b*tau*0)),[])).


test(7) :- acciones((b*tau*0),[b]).
test(8) :- acciones((b*0),[b]).
test(9) :- acciones(c*((a*0) + (b*tau*0)),[c,a,b]).

test(10) :- reduce((a*b*c*0),a, (b*c*0)).
test(11) :- reduce(((a*b*0)+ (b*tau*0)),a,(b*0)).
test(12) :- reduce(((a*b*0)+ (b*tau*0)),b,(tau*0)).

test(13) :- not(reduceLista(c *((a*0) + (b * 0)), b,_)).

test(14) :- trazas( ((a*0) + (b * tau * 0)), [[], [a], [b]]).

test(15) :- residuo((a*0+b*tau*0+b*c*0),[c],[]).

test(16) :- not(must([d*0,(b*d*0)],[c])).

test(17) :- puedeReemplazarA(a*tau*0,a*0).

test(18) :- equivalentes(a*b*c*0,a*tau*b*c*tau*0).


tests :- forall(between(0, 18, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.