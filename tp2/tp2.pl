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

%utilizamos setof para devolver todas las trazas posibles en una lista
%trazas(+Proceso, -Cadenas)
trazas(Proc,M) :- setof(K,calculoDetrazas(Proc,K),M).

%se tiene la definciion de residuo para una lista de procesos y para un proceso unicamente.
%si viene una lista se aplicara a cada uno de ellos la definicion de residuo para un proceso.
%si es un proceso solo entonces directamente aplicaremos la definicion correspondiente de residuo.
%En caso de que la cadena provista no pueda ser consumida por el proceso entonces se devolvera el residuo Vacio (tercera definicion de residuo)
%residuo(+X,+Cadena,-Qs)
residuo(Proceso, Cadena, Ps2) :- setof(X, reduceLista(Proceso, Cadena, X), Ps2).
residuo(Lss, Cadena, Residuos) :- consumirCadenaEnProcesos(Lss, Cadena, Residuos).
residuo(Proceso, Cadena, []) :- not(reduceLista(Proceso,Cadena,_)).

%utilizamos union para devolver una lista de resultados sin repetidos
%aplicamos la cadena a cada proceso ( s ) y nos quedamos con todos los residuos generados
%consumirCadenaEnProcesos(+Pss, +Cadena, ?Residuos)
consumirCadenaEnProcesos([Ps], Cadena, Residuos) :- residuo(Ps, Cadena, Residuos).
consumirCadenaEnProcesos([Ps | Pss], Cadena, Residuos) :- residuo(Ps, Cadena, ResiduosPs), consumirCadenaEnProcesos(Pss, Cadena, ResiduoPss), union(ResiduosPs, ResiduoPss, Residuos).

%Se hace distincion de cuando se tiene un proceso unicamente, o bien una lista de procesos.
%must(+P,+L).
must(P, Ls) :- is_list(P), mustList(P,Ls).
must(P, Ls) :- not(is_list(P)), mustOneProcess(P,Ls).

%recursion, para cada uno de los procesos de la lista de procesos debe verificar mustOneProcess
%mustList(+Pss, +Ls)
mustList([], _).
mustList([Proceso | Pss], Ls) :- mustOneProcess(Proceso, Ls), mustList(Pss, Ls).

%Si el proceso es vacio vale para toda lista el must, si viene un tau obviamos el mismo para alcanzar el primer simbolo terminal consumible en el proceso.
% El ultimo caso verifica si existe al menos un simbolo terminal que tenga un residuo distinto de vacio lo cual implica que pudo ser consumido por el proceso.
% el proceso residuo debe ser al menos el proceso 0. 
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
puedeReemplazarA(P,Q) :- getTrazasPyQ(P, Q, TrazasPyQ), getAccionesPyQ(P, Q, AccionesPyQ),  not(algunaTrazaYSubDeAccRompen(P, Q, TrazasPyQ, AccionesPyQ)). 

%aplicaremos cada traza a ambos procesos (P y Q), y luego probamos con generarYProbarSubsetsDeAcciones todos los posibles subconjuntos de acciones posibles. 	
%algunaTrazaYSubDeAccRompen(+P, +Q, +Ts, +Accs)
algunaTrazaYSubDeAccRompen(P, Q, [Traza | TrazasPyQ] , AccionesPyQ) :- residuo(P, Traza, PResiduos) , residuo(Q, Traza, QResiduos), generarYProbarSubsetsDeAcciones(PResiduos, QResiduos, AccionesPyQ); algunaTrazaYSubDeAccRompen(P, Q, TrazasPyQ , AccionesPyQ).

%generacion de todos los conjuntos de acciones posibles a partir de las acciones de P y Q, luego se intenta buscar, entre alguno de ellos, uno que rompa el predicado que define puedeReemplazarA 
%generarYProbarSubsetsDeAcciones(+PResiduos, +QResiduos, +AccionesPyQ)
generarYProbarSubsetsDeAcciones(PResiduos, QResiduos, AccionesPyQ) :- setof(X, subsecuencia(AccionesPyQ, X), SubSetsAcciones), anySubsetMatches(PResiduos, QResiduos, SubSetsAcciones).

%dado un conjunto de sets de acciones, se verifica si almenos uno de ellos verifica la negacion del predicado que define puedeReemplazarA (dada en el enunciado)
%anySubsetMatches(+PResiduos, +QResiduos, +SsetsAcciones)
anySubsetMatches(PResiduos,QResiduos,[Sset | SsetsAcciones]) :- must(PResiduos, Sset), not(must(QResiduos, Sset)); anySubsetMatches(PResiduos,QResiduos,SsetsAcciones).

%subsecuencias de una lista
%subsecuencia(+List, ?List)
subsecuencia(List, List).
subsecuencia(List, Rest) :- subseq(List, Rest).

%subseq(+List, ?Rest)
subseq([_|Tail], Rest) :- subsecuencia(Tail, Rest).
subseq([Head|Tail], [Head|Rest]) :- subseq(Tail, Rest).

%trazas(P) U trazas(Q)
%getTrazasPyQ(+P, +Q, ?TrazasPyQ)
getTrazasPyQ(P, Q, TrazasPyQ) :- trazas(P, TrazasP), trazas(Q, TrazasQ), union(TrazasP, TrazasQ, TrazasPyQ).

%acciones(P) U acciones(Q)
%getAccionesPyQ(+P, +Q, ?AccionesPyQ)
getAccionesPyQ(P, Q, AccionesPyQ) :- acciones(P, AccionesP), acciones(Q, AccionesQ), union(AccionesP,AccionesQ, AccionesPyQ).

%si P reemplaza a Q y Q a P entonces son equivalentes.
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
test(14) :- reduceLista(0,[],0).
test(15) :- reduceLista(c * ((a*0) + (b * tau * 0)), [], (c* (a*0+b*tau*0))).
test(16) :- reduceLista(c * ((a*0) + (b * tau * 0)), [c, b], (tau*0)).
test(17) :- reduceLista(c * ((a*0) + (b * tau * 0)), [c, b], 0).

test(18) :- trazas( ((a*0) + (b * tau * 0)), [[], [a], [b]]).
test(19) :- trazas((a*0) + (b * 0), [[], [a], [b]]).
test(20) :- trazas(c * ((a*0) + (b * tau * 0)), [[], [c], [c, a], [c, b]]).

test(21) :- residuo((a*0+b*tau*0+b*c*0),[c],[]).
test(22) :- residuo((a*0+b*tau*0+b*c*0),[b],[0, c*0, tau*0]).
test(23) :- residuo([(a*0+b*tau*0+b*c*0),(b*a*c*0)],[b],[0, c*0, tau*0, a*c*0]).
test(24) :- residuo([(a*0+b*tau*0+b*c*0),(b*a*c*0+b*c*0)],[b],[0, tau*0, a*c*0, c*0]).

test(25) :- not(must([d*0,(b*d*0)],[c])).
test(26) :- must((a*0+b*0+b*c*0),[a]).
test(27) :- must((tau*b*0+b*c*0),[b]).
test(28) :- must((a*0+b*c*0),[a,b]).
test(29) :- not(must((a*0+b*c*0),[c])).
test(30) :- not(must([c*0,(b*c*0)],[c])).
test(31) :- must([b*0,(a*0+b*c*0)],[b]).

test(32) :- puedeReemplazarA(a*tau*0,a*0).
test(33) :- puedeReemplazarA(tau*tau*a*tau*0,a*0).
test(34) :- puedeReemplazarA(a*0,a*0).

test(35) :- equivalentes(a*b*c*0,a*tau*b*c*tau*0).
test(36) :- equivalentes(a*b*c*0,a*b*c*0).
test(37) :- not(equivalentes(a*b*c*0,a*b*0)).
test(38) :- not(equivalentes(a*b*c,a*b*c)).

tests :- forall(between(0, 38, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.