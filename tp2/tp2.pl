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

%utilizamos setof para unificar todas las trazas posibles con una lista
%trazas(+Proceso, -Cadenas)
trazas(Proc,M) :- setof(K,calculoDetrazas(Proc,K),M).

%Se tiene una definicion de residuo para una lista de procesos y otra para el caso en el cual Proceso unifica con un proceso unicamente.
%En el caso de una lista se verificará si cada elemento cumple con la definicion de residuo para un proceso.
%En caso de que la cadena provista no pueda ser consumida por el proceso entonces solo unifica el residuo Vacio (tercera definicion de residuo)
%residuo(+X,+Cadena,-Qs)
residuo(Proceso, Cadena, Ps2) :- setof(X, reduceLista(Proceso, Cadena, X), Ps2).
residuo(Lss, Cadena, Residuos) :- consumirCadenaEnProcesos(Lss, Cadena, Residuos).
residuo(Proceso, Cadena, []) :- not(reduceLista(Proceso,Cadena,_)).

%Utilizamos union para que lo unico que unifique con Residuos sea una lista de resultados sin repetidos
%Hacemos una verificacion de la cadena con cada proceso y solo unifican los residuos generados
%consumirCadenaEnProcesos(+Pss, +Cadena, ?Residuos)
consumirCadenaEnProcesos([Ps], Cadena, Residuos) :- residuo(Ps, Cadena, Residuos).
consumirCadenaEnProcesos([Ps | Pss], Cadena, Residuos) :- residuo(Ps, Cadena, ResiduosPs), consumirCadenaEnProcesos(Pss, Cadena, ResiduoPss), union(ResiduosPs, ResiduoPss, Residuos).

%Se hace la distincion de cuando se tiene un proceso o una lista de procesos.
%must(+P,+L).
must(P, Ls) :- is_list(P), mustList(P,Ls).
must(P, Ls) :- not(is_list(P)), mustOneProcess(P,Ls).

%Cada uno de los procesos de la lista de procesos debe verificar mustOneProcess
%mustList(+Pss, +Ls)
mustList([], _).
mustList([Proceso | Pss], Ls) :- mustOneProcess(Proceso, Ls), mustList(Pss, Ls).

%Si el proceso es vacio vale para toda lista el must, si viene un tau obviamos el mismo para alcanzar el primer simbolo terminal consumible en el proceso.
%El ultimo caso verifica si existe al menos un simbolo terminal que tenga un residuo distinto de vacio lo cual implica que pudo ser consumido por el proceso.
%El proceso residuo debe ser al menos el proceso 0. 
%mustOneProcess(+P, +Ls)
mustOneProcess(0, _).
mustOneProcess(tau*P, Ls) :- mustOneProcess(P, Ls).
mustOneProcess(Proceso, [X | Ls]) :-  residuo(Proceso, [X], ProcesoResiduo), ProcesoResiduo \= [] ; mustOneProcess(Proceso, Ls).

%La idea de esta funcion es la de primero generar todas las trazas y acciones de P y de Q, y luego realizar un testeo de cada traza con cada subconjunto de acciones, aquí debe notarse el uso de la técnica Generate & Test.
%Como todo esta instanciado a la hora de llamar a algunaTrazaYSubDeAccRompen el comportamiento del not que lo encierra sera
%analogo al del NOT logico. algunaTrazaYSubDeAccRompen lo que hace es tratar de encontrar una traza tal que al unificar ambos procesos P y Q con sus correspondientes residuos
%se tome luego algun subconjunto de acciones (generados todos en generarYProbarSubsetsDeAcciones) tal que rompa la negacion del predicado que en realidad debe cumplirse para verificar puedeReemplazarA.
%Con encontrar una combinacion de Traza + subconjunto_De_Acciones que verifique el predicado del enunciado, algunaTrazaYSubDeAccRompen unificará a TRUE
%y el not que lo encierra unificará a FALSE, entonces puedeReemplazarA unificará a FALSE también, implicando justamente que no pueda ser reemplazado P por Q.
%Ahora bien, si unifica a false el predicado algunaTrazaYSubDeAccRompen, significa que no pudo cumplirse la negacion del predicado a cumplir del enunciado 
%lo cual significa que para todo conjunto de acciones y trazas posibles se cumple la definicion de puedeReemplazarA sin negar, haciendo que finalmente el not que encierra a algunaTrazaYSubDeAccRompen
%unifique TRUE, implicando en la unificacion a TRUE de puedeReemplazarA que determina que P puede ser reemplazado por Q.
%puedeReemplazarA(+P, +Q)
puedeReemplazarA(P,Q) :- getTrazasPyQ(P, Q, TrazasPyQ), getAccionesPyQ(P, Q, AccionesPyQ),  not(algunaTrazaYSubDeAccRompen(P, Q, TrazasPyQ, AccionesPyQ)). 

%unificaremos los residuos de cada proceso utilizando cada una de las trazas, y luego probamos con generarYProbarSubsetsDeAcciones todos los posibles subconjuntos de acciones posibles que se generarán. Usa Generate & Test. 	
%algunaTrazaYSubDeAccRompen(+P, +Q, +Ts, +Accs)
algunaTrazaYSubDeAccRompen(P, Q, [Traza | TrazasPyQ] , AccionesPyQ) :- residuo(P, Traza, PResiduos) , residuo(Q, Traza, QResiduos), generarYProbarSubsetsDeAcciones(PResiduos, QResiduos, AccionesPyQ); algunaTrazaYSubDeAccRompen(P, Q, TrazasPyQ , AccionesPyQ).

%generacion de todos los conjuntos de acciones posibles a partir de las acciones de P y Q, luego se intenta buscar, entre alguno de ellos, uno que no verifique el predicado que define puedeReemplazarA. Usa Generate & test.
%generarYProbarSubsetsDeAcciones(+PResiduos, +QResiduos, +AccionesPyQ)
generarYProbarSubsetsDeAcciones(PResiduos, QResiduos, AccionesPyQ) :- setof(X, subsecuencia(AccionesPyQ, X), SubSetsAcciones), anySubsetMatches(PResiduos, QResiduos, SubSetsAcciones).

%dado un conjunto de sets de acciones, se verifica si al menos uno de ellos verifica la negacion del predicado que define puedeReemplazarA (dado en el enunciado)
%anySubsetMatches(+PResiduos, +QResiduos, +SsetsAcciones)
anySubsetMatches(PResiduos,QResiduos,[Sset | SsetsAcciones]) :- must(PResiduos, Sset), not(must(QResiduos, Sset)); anySubsetMatches(PResiduos,QResiduos,SsetsAcciones).

%subsecuencias de una lista
%subsecuencia(+List, ?List)
subsecuencia(List, List).
subsecuencia(List, Resto) :- subsecuenciaCasoListaNoVacia(List, Resto).

%subsecuenciaCasoListaNoVacia(+List, ?Resto)
subsecuenciaCasoListaNoVacia([_|Cola], Resto) :- subsecuencia(Cola, Resto).
subsecuenciaCasoListaNoVacia([X|Cola], [X|Resto]) :- subsecuenciaCasoListaNoVacia(Cola, Resto).

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
test(39) :- equivalentes(a*tau*b*c*tau*0,a*b*c*0).

tests :- forall(between(0, 39, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.