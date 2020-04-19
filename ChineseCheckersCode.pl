%.................TABLERO INICIAL.............................
%
% Cada posicion se compone de dos coordenadas (X,Y).
% X pertenece de (1,9)
% Y pertenece de (1,17)
% Estos componentes son dinamicos ya que se alteran a lo largo del
% juego.
%

:- dynamic computadoraR/ 2.
computadoraR( 1 , 17 ).
computadoraR( 1 , 16 ).
computadoraR( 2 , 16 ).
computadoraR( 1 , 15 ).
computadoraR( 2 , 15 ).
computadoraR( 3 , 15 ).
computadoraR( 1 , 14 ).
computadoraR( 2 , 14 ).
computadoraR( 3 , 14 ).
computadoraR( 4 , 14 ).

:- dynamic jugadorA/ 2.
jugadorA( -4 , 13 ).
jugadorA( -3 , 13 ).
jugadorA( -2 , 13 ).
jugadorA( -1 , 13 ).
jugadorA( -3 , 12 ).
jugadorA( -2 , 12 ).
jugadorA( -1 , 12 ).
jugadorA( -2 , 11 ).
jugadorA( -1 , 11 ).
jugadorA( -1 , 10 ).

:- dynamic jugadorB/ 2.
jugadorB( -1 , 8).
jugadorB( -2 , 7 ).
jugadorB( -1 , 7 ).
jugadorB( -1 , 6 ).
jugadorB( -2 , 6 ).
jugadorB( -3 , 6 ).
jugadorB( -1 , 5 ).
jugadorB( -2 , 5 ).
jugadorB( -3 , 5 ).
jugadorB( -4 , 5 ).

:- dynamic jugadorV/ 2.
jugadorV( 1 , 1 ).
jugadorV( 1 , 2 ).
jugadorV( 2 , 2 ).
jugadorV( 1 , 3 ).
jugadorV( 2 , 3 ).
jugadorV( 3 , 3 ).
jugadorV( 1 , 4 ).
jugadorV( 2 , 4 ).
jugadorV( 3 , 4 ).
jugadorV( 4 , 4 ).

:- dynamic jugadorZ/ 2.
jugadorZ( 6 , 5 ).
jugadorZ( 7 , 5 ).
jugadorZ( 8 , 5 ).
jugadorZ( 9 , 5 ).
jugadorZ( 7 , 6 ).
jugadorZ( 8 , 6 ).
jugadorZ( 9 , 6 ).
jugadorZ( 8 , 7 ).
jugadorZ( 9 , 7 ).
jugadorZ( 9 , 8 ).

:- dynamic jugadorN/ 2.
jugadorN( 6 , 13 ).
jugadorN( 7 , 13 ).
jugadorN( 8 , 13 ).
jugadorN( 9 , 13 ).
jugadorN( 7 , 12 ).
jugadorN( 8 , 12 ).
jugadorN( 9 , 12 ).
jugadorN( 8 , 11 ).
jugadorN( 9 , 11 ).
jugadorN( 9 , 10 ).





%.................CONDICIONES DEL TABLERO......................
%

% -- Checar si una casilla se encuentra dentro del tablero
%
casilla(X , Y):-
    G is abs( Y - 9 ),
    Y > 0,
    Y =< 17,
    X > 0,
    X =< ( 9 - G ).

casilla2(X , Y):-
    G is abs( Y - 9 ),
    Y > 4,
    Y =< 13,
    (X < 0, X >= (0-G);
    X >(9-G), X =< 9).


% -- Checar si dos casillas son diferentes
%
diferentes(X1 , Y1 , X2 , Y2):-
    X1 \== X2;
    Y1 \== Y2.

% -- Checar si un dos casillas son adjuntas
%
% El primero siempre falla si algun miembro no pertenece a las casillas
% validas del tablero y luego checamos los casos que si son adjuntos.
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (not(casilla( X1 , Y1 ));
    not(casilla( X2 , Y2 ))),
    !,
    fail.

% Adjuntos horizontales
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 == Y2,
    G is abs( X1 - X2 ),
    G  ==  1.

% Adjuntos verticales
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    X1 == X2,
    G is abs( Y1 - Y2 ),
    G  ==  1 .

% Adjuntos diagonales en la mitad inferior del tablero (Checamos
% movimientos para arriba y abajo)
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1  <  9,
    W is Y2 - Y1,
    W  ==  1,
    Z is X2 - X1,
    Z  ==  1.
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1  =<  9,
    W is Y2 - Y1,
    W  ==  - 1,
    Z is X2 - X1,
    Z  ==  - 1.

% Adjuntos diagonales en la mitad superior del tablero
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1  >= 9,
    W is Y2 - Y1,
    W  ==  1,
    Z is X1 - X2,
    Z  ==  1 .
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1  >  9,
    W is Y2 - Y1,
    W  ==  - 1,
    Z is X1 - X2,
    Z  ==  - 1.

%.................CONDICIONES DE LA CASILLA......................
%
% Una casilla puede estar libre u ocupada
%
libre( X , Y ):-
    casilla( X , Y ),
    not(jugadorV( X , Y )),
    not(computadoraR( X , Y )).

ocupada( X , Y ):-
    casilla( X , Y ),
    (jugadorV( X , Y );
    computadoraR( X , Y )).


%.................SALTOS A CASILLAS..............................
%
% A continuacion mostrados todos los posibles movimientos/saltos
% factibles de las distintas piezas.
%
% -- Saltos sobre la misma linea
% Izquierda
%
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Diff is X1 - X2,
    Diff == 2,
    Xi is X1 - 1,
    Y1 == Y2,
    ocupada( Xi , Y1 ),
    !.

% Derecha
%
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Diff is X2 - X1 ,
    Diff == 2,
    Xi is X1 + 1,
    Y1 == Y2,
    ocupada( Xi , Y1 ),
    !.

% -- Saltos hacia adelante o atras diagonales
% -- Mitad inferior
%
% Subiendo
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 < 9 , Y2 =< 9,
    DiffX is X2 - X1,
    DiffY is Y2 - Y1,
    ( DiffX == 2 ; DiffX == 0 ),
    DiffY ==2,
    Xi is X1 + ( DiffX / 2),
    Yi is Y1 + 1,
    ocupada( Xi , Yi ),
    !.

% Bajando
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 < 9,
    Y2 =< 9,
    DiffX is X1 - X2,
    DiffY is Y1 - Y2,
    ( DiffX == 2 ; DiffX == 0 ),
    DiffY ==2,
    Xi is X1 - ( DiffX / 2),
    Yi is Y1 - 1,
    ocupada( Xi , Yi ),
    !.

% -- Mitad superior
%
% Subiendo
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 > 9,
    Y2 >= 9,
    DiffX is X1 - X2,
    DiffY is Y2 - Y1,
    ( DiffX == 2 ; DiffX == 0 ),
    DiffY ==2,
    Xi is X1 - ( DiffX / 2),
    Yi is Y1 + 1,
    ocupada( Xi , Yi ),
    !.

% Bajando
saltoFact(X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 > 9,
    Y2 >= 9,
    DiffX is X2 - X1,
    DiffY is Y1 - Y2,
    ( DiffX == 2 ; DiffX == 0 ),
    DiffY ==2,
    Xi is X1 + ( DiffX / 2),
    Yi is Y1 - 1,
    ocupada( Xi , Yi ),
    !.

% -- Linea Central (caso especial)
%
% Subiendo1
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 8,
    Y2 == 10 ,
    DiffX is X2 - X1 ,
    DiffX == 1,
    Yi is Y1 + 1 ,
    ocupada( X2 , Yi ),
    !.

% Subiendo2
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 8 ,
    Y2 == 10 ,
    DiffX is X1 - X2 ,
    DiffX == 1,
    Yi is Y1 + 1 ,
    ocupada( X1 , Yi ),
    !.

% Bajando1
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 10 ,
    Y2 == 8 ,
    DiffX is X2 - X1 ,
    DiffX == 1,
    Yi is Y1 - 1 ,
    ocupada( X2 , Yi ),
    !.

% Bajando2
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 10 ,
    Y2 == 8 ,
    DiffX is X1 - X2 ,
    DiffX == 1,
    Yi is Y1 - 1 ,
    ocupada( X1 , Yi ),
    !.


%.................PASOS A CASILLAS..............................
%
pasoFact( X1 , Y1 , X2 , Y2 ):-
    adjuntos( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ) .


%.................MOVIMIENTOS VALIDOS...........................
%
% Un movimiento es valido si existe un paso factible o un salto factible
% a el.
%
movVal( X1 , Y1 , X2 , Y2 ):-
    pasoFact( X1 , Y1 , X2 , Y2 ),
    !.
movVal( X1 , Y1 , X2 , Y2 ):-
    saltoFact( X1 , Y1 , X2 , Y2 ),
    ! .



% .................FIN DEL JUEGO.............................
%
ganoJugadorV:-
    jugadorV( 1 , 17 ),
    jugadorV( 1 , 16 ),
    jugadorV( 2 , 16 ),
    jugadorV( 1 , 15 ),
    jugadorV( 2 , 15 ),
    jugadorV( 3 , 15 ),
    jugadorV( 1 , 14 ),
    jugadorV( 2 , 14 ),
    jugadorV( 3 , 14 ),
    jugadorV( 4 , 14 ).

ganoComputadoraR:-
    computadoraR( 1 , 1 ),
    computadoraR( 1 , 2 ),
    computadoraR( 2 , 2 ),
    computadoraR( 1 , 3 ),
    computadoraR( 2 , 3 ),
    computadoraR( 3 , 3 ),
    computadoraR( 1 , 4 ),
    computadoraR( 2 , 4 ),
    computadoraR( 3 , 4 ),
    computadoraR( 4 , 4 ).



% ................EVALUACIONES PARA LA HEURISTICA............
%
% -- Tama�o de una lista
tama�oLista( [] , 0 ).
tama�oLista([ _ | R ], T ):-
    tama�oLista( R , T2 ),
    T is T2 + 1.

% -- Encontrar las posiciones de la computadoraR y jugadorV y guardarlas
% en una lista.
%
computadoraRPos(Pos):-
    findall( [ X , Y ], computadoraR(X , Y), Pos ).

jugadorVPos(Pos):-
    findall( [ X , Y ], jugadorV(X , Y), Pos ).

tableroPos(Pos):-
    computadoraRPos(P1),
    jugadorVPos(P2),
    append([P1], [P2], Pos ).


% -- Valor de las posiciones
%
% Primero evaluamos cuantos movimientos posibles tiene una pieza
% unitarios
%
miembro( X , [ X | _ ]).
miembro( X , [ _ | L ]):-
    miembro(X , L).

movilidad( X , Y , Mov):-
    findall( X2 ,( Z is X - 1 , W is X + 1 , numlist(Z , W , DX), Z2 is Y - 1 , W2 is Y + 1 , numlist(Z2 , W2 , DY), miembro(X2 , DX), miembro( Y2 ,DY ), pasoFact( X , Y , X2 , Y2 )), M),
    tama�oLista( M , Mov).

% Evaluamos la distancia al centro
%
distCentro(X , Y , Dist):-
    Y > 9,
    Aux is 18 - Y,
    Centro is Aux / 2,
    Dist is abs(Centro - X).

distCentro(X , Y , Dist):-
    Y =< 9 ,
    Centro is Y / 2,
    Dist is abs(Centro - X).


% -- Ponderamos el valor que tiene la posicion de una ficha
%

% valorComputadoraR(X, Y, Val):- movilidad(X, Y, M), Dist is
% 17-Y-1, Val is (Dist * 3) -M.

valorComputadoraR(X , Y , Val):-
    distCentro(X , Y , Dist),
    Val is (( 18 - Y ) * 4 - Dist * 2).

% Recibe lista con todas las posiciones de sus piezas en formato de
% lista cada una (cada elemento tiene "x" y "y")
valorTableroComputadoraR( [] , 0 ).
valorTableroComputadoraR([ T | Q ], Val):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    valorComputadoraR( X , Y , Val2 ),
    valorTableroComputadoraR( Q , Val1 ),
    Val is Val1 + Val2.

valorFinalComputadoraR(Val):-
    computadoraRPos(Pos),
    valorTableroComputadoraR(Pos , Val).

% valorJuagadorV(X, Y, Val):- movilidad(X, Y, M), Dist is Y + 1,
% Val is (Dist * 3) -M.
%

valorJugadorV(X , Y , Val):-
    distCentro(X , Y , Dist),
    Val is (( Y ) * 4 - Dist * 2 ).

valorTableroJugadorV([] , 0).
valorTableroJugadorV([ T | Q ], Val ):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    valorJugadorV( X , Y , Val2 ),
    valorTableroJugadorV( Q , Val1 ),
    Val is Val1 + Val2.

valorFinalJugadorV(Val):-
    jugadorVPos(Pos),
    valorTableroJugadorV( Pos , Val ).


%.....................HEURISTICA.............................
%

% En pos le damos tablero pos que tiene todas las posiciones del
% tablero, primero la de la computadora y luego la del jugadorV
%

heur(Pos, _ , Val, IA):-
	IA == 1 ,
	nth1(1 , Pos , Piezascompu), nth1(2 , Pos , PiezasjugadorV),
	heurComputadoraRAvanza(Piezascompu , Valcompu),
	heurJugadorVAvanza(PiezasjugadorV , ValjugadorV),
	Val is Valcompu - ValjugadorV.

heur(Pos, _ , Val, IA):-
	IA == 2,
	nth1( 1 , Pos , Piezascompu ), nth1( 2 , Pos , PiezasjugadorV ),
	heurComputadoraRAvanza2( Piezascompu , Valcompu ),
	heurJugadorVAvanza2( PiezasjugadorV , ValjugadorV ),
	Val is Valcompu - ValjugadorV.

% Es igual a valorComputadoraR
heurAPiezaComputadoraR( X , Y , Val ):-
    distCentro( X , Y , Dist ),
    Val is (( 18 - Y ) * 4 - Dist * 2 ).

% Es igual a valorTableroComputadora
heurComputadoraRAvanza( [] , 0 ).
heurComputadoraRAvanza([ T | Q ], Val ):-
	nth1( 1 , T , X ),
        nth1( 2 , T , Y ),
        heurAPiezaComputadoraR( X , Y , Val2 ),
	heurComputadoraRAvanza( Q , Val1 ),
        Val is Val1 + Val2.

% Es igual a valorJugadorV
heurAPiezaJugadorV( _ , Y , Val ):-
    Val is ( Y * 4 ).

% Es igual a valorTableroJugador
heurJugadorVAvanza( [] , 0 ).
heurJugadorVAvanza([ T | Q ], Val ):-
	nth1( 1 , T , X ),
        nth1( 2 , T , Y ),
        heurAPiezaJugadorV( X , Y , Val2 ),
	heurJugadorVAvanza( Q , Val1 ),
        Val is Val1 + Val2.


% Para enfrentar 2 AI
%

heurAPiezaComputadoraR2( X , Y , Val ):-
    distCentro( X , Y , _ ),
    Val is (( 18 - Y ) * 4 ).

heurComputadoraRAvanza2( [] , 0 ).
heurComputadoraRAvanza2([ T | Q ], Val ):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaComputadoraR2( X , Y , Val2 ),
    heurComputadoraRAvanza2( Q , Val1 ),
    Val is Val1 + Val2.

heurAPiezaJugadorV2( X , Y , Val ):-
    distCentro( X , Y , Dist ), Val is ( Y * 4 ) - Dist * 2.
heurJugadorVAvanza2( [] , 0 ).
heurJugadorVAvanza2([ T | Q ], Val ):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorV2( X , Y , Val2 ),
    heurJugadorVAvanza2( Q , Val1 ),
    Val is Val1 + Val2.



%...................POS. ACCESIBLES............................
%


% Devuelve la lista de espacios disponibles

espaciosDisponiblesComputadoraR( X , Y , M ):-
    findall([ X2 , Y2 ],
            (numlist( 1 , 9 , DX ),numlist( 1 , Y , DY ), miembro( X2 , DX ), miembro( Y2 , DY ), movVal( X , Y , X2 , Y2 )),
            M).

espaciosDisponiblesJugadorV( X , Y , M ):-
    findall([ X2 , Y2 ],
            (numlist( 1 , 9 , DX ), numlist( Y , 17 , DY ), miembro( X2 , DX ), miembro( Y2 , DY ) , movVal( X , Y , X2 , Y2 )),
            M) .

% Turno 1 juega la compu, turno 2 juega el jugadorV
%

espaciosDisponibles(X , Y , M , Turno , IA):-
    IA == 1 ,
    ((Turno == 1, espaciosDisponiblesComputadoraR( X , Y , M ),! ); (Turno == 2, espaciosDisponiblesJugadorV( X , Y , M ) ,! )).

espaciosDisponibles(X , Y , M , Turno , IA):-
    IA == 2 ,
    ((Turno == 2, espaciosDisponiblesComputadoraR( X , Y , M ), ! ); (Turno == 1, espaciosDisponiblesJugadorV( X , Y , M ) ,!)).



% ------- Juega la ficha
%
% Tomamos la Pos con toda las posiciones y simula el tiro quitando la
% lista de las posiciones y agregando la nueva posicion.
%

simularTiroComputadoraR( Pos , X1 , Y1 , X2 , Y2 , Out ):-
    subtract( Pos , [[ X1 , Y1 ]], G ),
    append([[ X2 , Y2 ]], G , Out ).

% Combina elem con todos los elementos de la lista y lo guarda en Lista.
% Jugador mete el elemento despues de cada nodo y computadora antes
distribucionJugadorV( [] , _ , [] ).
distribucionJugadorV([ T | Q ], Elem , Lista ):-
    distribucionJugadorV( Q , Elem , L2 ),
    append([ T ], [ Elem ], L1 ),
    append([ L1 ], L2 , Lista ).

distribucionComputadoraR( [] , _ , [] ) .
distribucionComputadoraR([ T | Q ], Elem , Lista ):-
    distribucionComputadoraR( Q , Elem , L2 ),
    append([ Elem ], [ T ], L1 ),
    append([ L1 ], L2 , Lista ).


% -- Sucesores de la posici�n actual
% Todos los pr�ximos movimientos posibles y como queda la configuraci�n
% del tablero.
%

succL([] , _ , []).
succL([ Elem | QL ], G , Res):-
	succL(QL , G , Res1),
	append([ Elem ], G , Res2),
	append(Res1 , [ Res2 ], Res).

succCalc([] , _ , [] , _ , _ ).
succCalc([ T | Q ], Pos , PosList , Turno , AI ):-
	nth1( 1 , T , Xi ),
        nth1( 2 , T , Yi ),
	espaciosDisponibles( Xi , Yi , L , Turno , AI ),
	subtract( Pos , [[ Xi , Yi ]], G ),
	succL( L , G , PosList1 ),
	succCalc( Q , Pos , PosList2 , Turno , AI ),
	append( PosList1 , PosList2 , PosList ).


% SUCC se apoya de las dos funciones anteriores y lo que hace es hacer
% un movimineto, meterlo a una lista y meter como quedan todas las demas
% posiciones

succ([ Compu , Player ], Turno , PosList , IA ):-
	IA == 1 ,
        Turno == 1 ,
	succCalc( Compu , Compu , Temp , Turno , IA ),
        distribucionJugadorV(Temp , Player , PosList ). %, writeln (PosList).

succ([ Compu , _ ], Turno , PosList , IA ):-
	IA == 1 ,
        Turno == 2 ,
	succCalc( Compu , Compu , Temp , Turno , IA ),
	distribucionComputadoraR( Temp , Compu , PosList ). %, writeln (PosList).

succ([ Compu , Player ], Turno , PosList , IA ):-
	IA == 2 ,
        Turno == 1 ,
	succCalc( Player , Player , Temp , Turno , IA ),
	distribucionComputadoraR( Temp , Compu , PosList ). %, writeln (PosList).

succ([ _ , Player ], Turno , PosList , IA ):-
	IA == 2 ,
        Turno == 2 ,
	succCalc( Player , Player , Temp , Turno , IA ),
        distribucionJugadorV( Temp , Player , PosList ). %, writeln (PosList).





% ...................MINIMAX CON PODA ALPHA BETA........................

% MAX = 1, MIN = 2
siguienteJugador( 1 , 2 ).
siguienteJugador( 2 , 1 ).

copia(Posicion , Valor , Posicion , Valor).

% El siguiente metodo guarda el mejor movimiento posible de dos
% que compara y servir� para el metodo minimax
%

recordMax( X , X , MejorMovX , _ , MejorMovX , X ).
recordMax( X , Y , MejorMovX , _ , MejorMovX , X ):-
    X >= Y.
recordMax( X , Y , _ , MejorMovY , MejorMovY , Y ):-
    X =< Y.


recordMin( X , X , MejorMovX , _ , MejorMovX , X ).
recordMin( X , Y , MejorMovX , _ , MejorMovX , X ):-
    X =< Y.
recordMin( X , Y , _ , MejorMovY , MejorMovY , Y ):-
    X >= Y.

%Herramientas
%Decremento
decr( P , P2 ):-
    P2 is P - 1.
% Lista vac�a
vacio([]).

testDepth( P , _ , _ ):-
    P  ==  0 ;
    ganoJugadorV;
    ganoComputadoraR.

max( A , A , A ).
max( A , B , A ):-
    A > B .
max( A , B , B ):-
    A < B.

min( A , A , A ).
min( A , B , A ):-
    A < B.
min( A , B , B ):-
    A > B.



% Minimax alpha beta: llamada
minimaxab( Posicion , Turno , Profundidad , MejorMov , Valor , IA ):-
	succ( Posicion , Turno , X , IA ),% Simulaci�n del movimiento, X = lista de posiciones dispu�s de jugar
	siguienteJugador( Turno , OtroJugador ), % Cambio de jugador
	decr( Profundidad , P2 ), % Profundidad de disminuci�n
	alphabeta( X , OtroJugador , P2 , -999 , 999 , MejorMov , Valor , IA ).  % Alpha y Beta se inicializan a -999 y 999


alphabeta( [] , _ , _ , _ , _ , _ , _ , _ ).

alphabeta([ E | L ], 1 , P , A , B , MejorMov , Valor , IA ):-
        testDepth( P , E , 1 ), % hoja final alcanzada o final del juego
        heur( E , 1 , ValE , IA ), % Evaluaci�n de la posici�n en ValE
        (( A =< ValE ,
           alphabeta( L , 1 , P , A , B , MejorMovL , ValL , IA ),
           recordMin( ValE , ValL , E , MejorMovL , MejorMov , Valor ));
        copia( E , ValE , MejorMov , Valor )).  % Memorizaci�n de los mejores, o es el y anterior o copiamos lo que teniamos

alphabeta([ E | L ], 2 , P , A , B , MejorMov , V , IA ):-
        testDepth( P , E , 2 ),
        heur( E , 2 , ValE , IA ),
        (( B >= ValE ,
           alphabeta( L , 2 , P , A , B , MejorMovL , ValL , IA ),
           recordMax( ValE , ValL , E , MejorMovL , MejorMov , V ));
         copia( E , ValE , MejorMov , V )).

alphabeta([ E | L ], 1 , P , A , B , MejorMov , V , IA ):-
        succ( E , 1 , X , IA ),
        not(vacio( X )),
        P \= 0 ,
        decr( P , P2 ),
        alphabeta( X , 2 , P2 , A , B , MejorMovX , ValX , IA ),
        (( ValX >= A ,
           min( ValX , B , Bbis ),
           alphabeta( L , 1 , P , A , Bbis , MejorMovL , ValL , IA ),
           recordMin( ValX , ValL , E , MejorMovL , MejorMov , V ));
         copia( MejorMovX , ValX , MejorMov , V )).


alphabeta([ E | L ], 2 , P , A , B , MejorMov , V , IA ):-
        succ( E , 2 , X , IA ),
        not(vacio( X )),
        P \= 0 ,
        decr( P , P2 ),
        alphabeta( X , 1 , P2 , A , B , MejorMovX , ValX , IA ),
        (( ValX =< B ,
           max( ValX , A , Abis ),
           alphabeta( L , 2 , P , Abis , B , MejorMovL , ValL , IA ),
           recordMax( ValX , ValL , E , MejorMovL , MejorMov , V ));
         copia( MejorMovX , ValX , MejorMov , V )).






% .........................Dibujar..............................
%

% Draw o si el cuadro existe, 1 para la CPU, 2 para el jugador
draw( A , B ):-
    computadoraR( A , B ),
    write('R'),
    !.
draw( A , B ):-
    jugadorA( A , B ),
    write('A'),
    !.
draw( A , B ):-
    jugadorB( A , B ),
    write('B'),
    !.
draw( A , B ):-
    jugadorV( A , B ),
    write('V'),
    !.
draw( A , B ):-
    jugadorZ( A , B ),
    write('Z'),
    !.
draw( A , B ):-
    jugadorN( A , B ),
    write('N'),
    !.
draw( A , B ):-
    casilla( A , B ),
    write(.),
    !.
draw( A , B ):-
    casilla2( A , B ),
    write(.),
    !.
draw( _ , _ ):-
    write(-).

drawTablero:-
    drawLinea17,
    drawLinea16,
    drawLinea15,
    drawLinea14,
    drawLinea13,
    drawLinea12,
    drawLinea11,
    drawLinea10,
    drawLinea9,
    drawLinea8,
    drawLinea7,
    drawLinea6,
    drawLinea5,
    drawLinea4,
    drawLinea3,
    drawLinea2,
    drawLinea1.


drawLinea17:-
writeln(''),
write('                '),draw(1,17),write('        ').
drawLinea16:-
writeln(''),
write('               '),draw(1,16),write(' '),draw(2,16),write('       ').
drawLinea15:-
writeln(''),
write('              '),draw(1,15),write(' '),draw(2,15),write(' '),draw(3,15),write('      ').
drawLinea14:-
writeln(''),
write('             '),draw(1,14),write(' '),draw(2,14),write(' '),draw(3,14),write(' '),draw(4,14),write('     ').
drawLinea13:-
writeln(''),
write('    '),draw(-4,13),write(' '), draw(-3,13),write(' '), draw(-2,13),write(' '), draw(-1,13),write(' '), draw(1,13),write(' '), draw(2,13),write(' '), draw(3,13),write(' '), draw(4,13),write(' '),draw(5,13), write(' '), draw(6,13), write(' '), draw(7,13), write(' '), draw(8,13), write(' '), draw(9,13),write('    ').
drawLinea12:-
writeln(''),
write('     '), draw(-3,12), write( ' ' ), draw(-2,12), write( ' ' ), draw(-1,12), write( ' ' ), draw(1,12), write( ' ' ), draw(2,12), write( ' ' ), draw(3,12), write( ' ' ), draw(4,12), write( ' ' ), draw(5,12), write( ' ' ), draw(6,12), write(' '), draw(7,12), write(' '), draw(8,12), write(' '), draw(9,12),write( '   ' ).
drawLinea11:-
writeln(''),
write('      '),draw(-2,11),write(' '),draw(-1,11),write(' '),draw(1,11),write(' '),draw(2,11),write(' '),draw(3,11),write(' '),draw(4,11),write(' '),draw(5,11),write(' '),draw(6,11),write(' '),draw(7,11), write(' '), draw(8,11), write(' '), draw(9,11),write('  ').
drawLinea10:-
writeln(''),
write('       '),draw(-1,10),write(' '),draw(1,10),write(' '),draw(2,10),write(' '),draw(3,10),write(' '),draw(4,10),write(' '),draw(5,10),write(' '),draw(6,10),write(' '),draw(7,10),write(' '),draw(8,10), write(' '), draw(9,10),write('  ').
drawLinea9:-
writeln(''),
write('        '),draw(1,9),write(' '),draw(2,9),write(' '),draw(3,9),write(' '),draw(4,9),write(' '),draw(5,9),write(' '),draw(6,9),write(' '),draw(7,9),write(' '),draw(8,9), write(' '), draw(9,9), write(' ').
drawLinea8:-
writeln(''),
write('       '),draw(-1,8),write(' '),draw(1,8),write(' '),draw(2,8),write(' '),draw(3,8),write(' '),draw(4,8),write(' '),draw(5,8),write(' '),draw(6,8),write(' '),draw(7,8),write(' '),draw(8,8), write(' '), draw(9,8),write('  ').
drawLinea7:-
writeln(''),
write('      '),draw(-2,7),write(' '),draw(-1,7),write(' '),draw(1,7),write(' '),draw(2,7),write(' '),draw(3,7),write(' '),draw(4,7),write(' '),draw(5,7),write(' '),draw(6,7),write(' '),draw(7,7), write(' '), draw(8,7), write(' '), draw(9,7),write('  ').
drawLinea6:-
writeln(''),
write('     '), draw(-3,6), write( ' ' ), draw(-2,6), write( ' ' ), draw(-1,6), write( ' ' ), draw(1,6), write( ' ' ), draw(2,6), write( ' ' ), draw(3,6), write( ' ' ), draw(4,6), write( ' ' ), draw(5,6), write( ' ' ), draw(6,6), write(' '), draw(7,6), write(' '), draw(8,6), write(' '), draw(9,6),write( '   ' ).
drawLinea5:-
writeln(''),
write('    '),draw(-4,5),write(' '), draw(-3,5),write(' '), draw(-2,5),write(' '), draw(-1,5),write(' '), draw(1,5),write(' '), draw(2,5),write(' '), draw(3,5),write(' '), draw(4,5),write(' '),draw(5,5), write(' '), draw(6,5), write(' '), draw(7,5), write(' '), draw(8,5), write(' '), draw(9,5),write('    ').

drawLinea1:-
writeln(''),
write('                '),draw(1,1),write('        ').
drawLinea2:-
writeln(''),
write('               '),draw(1,2),write(' '),draw(2,2),write('       ').
drawLinea3:-
writeln(''),
write('              '),draw(1,3),write(' '),draw(2,3),write(' '),draw(3,3),write('      ').
drawLinea4:-
writeln(''),
write('             '),draw(1,4),write(' '),draw(2,4),write(' '),draw(3,4),write(' '),draw(4,4),write('     ').



% Llamar para jugar la computadora

jugarComputadoraR( IA ):-
    tableroPos( Pos ),
    minimaxab( Pos , 1 , 3 , MejorMov , _ , IA ),
    !,
    retractall(computadoraR( _ , _ )),
    nth1( 1 , MejorMov , Mejor ),
    forall(

        miembro( Elem , Mejor ),
        (
            nth1( 1 , Elem , X1 ), nth1( 2 , Elem , Y1 ), assert(computadoraR( X1 , Y1 ))
         )

        ),
	drawtablero( 1 ),
        !.


jugarJugadorV( IA ):-
    tableroPos( Pos ),
    minimaxab( Pos , 1 , 2 , MejorMov , _ , IA ),
    ! ,
    retractall(jugadorV( _ , _ )),
    nth1( 2 , MejorMov , Mejor ),
	forall(
            miembro( Elem , Mejor ),
            (
			nth1( 1 , Elem , X1 ), nth1( 2 , Elem , Y1 ), assert(jugadorV( X1 , Y1 ))
            )
	),
	drawtablero( 1 ),
        ! .

% -- Mover JugadorVerde
%
moverJugadorV( X1 , Y1 , X2 , Y2 ):-
    jugadorV( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorV( X1 , Y1 )),
    assert(jugadorV( X2 , Y2 )),
    jugarComputadoraR(1),
    !.

% -- Mover Computadora Roja
%
moverComputadoraR( X1 , Y1 , X2 , Y2 ):-
    computadoraR( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(computadoraR( X1 , Y1 )),
    assert(computadoraR( X2 , Y2 )),
    !.




% para ver un partido entre 2 IA
juegoIA:-
    repeat,
    jugarComputadoraR( 1 ),
    jugarJugadorV( 2 ).

newGame:-
    retractall(computadoraR( _ ,_ )),
    retractall(jugadorV( _ , _ )),
    assert(computadoraR( 1 , 17 )),
    assert(computadoraR( 1 , 16 )),
    assert(computadoraR( 2 , 16 )),
    assert(computadoraR( 1 , 15 )),
    assert(computadoraR( 2 , 15 )),
    assert(computadoraR( 3 , 15 )),
    assert(computadoraR( 1 , 14 )),
    assert(computadoraR( 2 , 14 )),
    assert(computadoraR( 3 , 14 )),
    assert(computadoraR( 4 , 14 )),
    assert(jugadorV( 1 , 1 )),
    assert(jugadorV( 1 , 2 )),
    assert(jugadorV( 2 , 2 )),
    assert(jugadorV( 1 , 3 )),
    assert(jugadorV( 2 , 3 )),
    assert(jugadorV( 3 , 3 )),
    assert(jugadorV( 1 , 4 )),
    assert(jugadorV( 2 , 4 )),
    assert(jugadorV( 3 , 4 )),
    assert(jugadorV( 4 , 4 )),
    drawtablero(1).






