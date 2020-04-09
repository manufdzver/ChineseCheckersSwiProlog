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

% -- Mover JugadorVerde
%
moverJugadorV( X1 , Y1 , X2 , Y2 ):-
    jugadorV( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorV( X1 , Y1 )),
    assert(jugadorV( X2 , Y2 )),
    !.

% -- Mover Computadora Roja
%
moverComputadoraR( X1 , Y1 , X2 , Y2 ):-
    computadoraR( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(computadoraR( X1 , Y1 )),
    assert(computadoraR( X2 , Y2 )),
    !.



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
% -- Tamaño de una lista
tamañoLista( [] , 0 ).
tamañoLista([ _ | R ], T ):-
    tamañoLista( R , T2 ),
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
    tamañoLista( M , Mov).

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




