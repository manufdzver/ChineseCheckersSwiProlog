
%.................TABLERO INICIAL.............................
%
% Cada posicion se compone de dos coordenadas (X,Y).
% X pertenece de (1,9)
% Y pertenece de (1,17)
% Estos componentes son dinamicos ya que se alteran a lo largo del
% juego.
%

:- dynamic modoJuego/1.
modoJuego(1).

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
jugadorA( -3 , 13 ).
jugadorA( -2 , 13 ).
jugadorA( -1 , 13 ).
jugadorA( 0 , 13 ).
jugadorA( -2 , 12 ).
jugadorA( -1 , 12 ).
jugadorA( 0 , 12 ).
jugadorA( -1 , 11 ).
jugadorA( 0 , 11 ).
jugadorA( 0 , 10 ).

:- dynamic jugadorB/ 2.
jugadorB( 0 , 8).
jugadorB( 0 , 7 ).
jugadorB( -1 , 7 ).
jugadorB( 0 , 6 ).
jugadorB( -1 , 6 ).
jugadorB( -2 , 6 ).
jugadorB( 0 , 5 ).
jugadorB( -1 , 5 ).
jugadorB( -2 , 5 ).
jugadorB( -3 , 5 ).

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
    G is abs( Y - 10 ),
    Y > 4,
    Y =< 13,
    (   (X =< 0, X >= (0-G),!);
        (X >=(9-G), X =< 9)).


% -- Checar si dos casillas son diferentes
%
diferentes(X1 , Y1 , X2 , Y2):-
    (X1 \== X2,!;
    Y1 \== Y2).

% -- Checar si un dos casillas son adjuntas


% Adjuntos horizontales
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    Y1 == Y2,
    G is abs( X1 - X2 ),
    G  ==  1, !.

% Adjuntos verticales
%
% Caso X1==X2
%
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    X1 == X2,
    G is abs( Y1 - Y2 ),
    G  ==  1, !.

%Casos X1 - X2 = 1

%Subiendo
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 < 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2,
    H == -1,
    G is Y1-Y2,
    G  ==  -1, !.

adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 >= 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2,
    H == 1,
    G is Y1 - Y2,
    G  ==  -1, !.

%Bajando
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 =< 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2,
    H == 1,
    G is Y1-Y2,
    G  ==  1, !.

adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 > 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2,
    H == -1,
    G is Y1 - Y2,
    G  ==  1, !.

% Salida/Entrada de X negativas y Salida/Entrada de excesos de X.
% Adjuntos horizontales
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla2( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla( X2 , Y2 ))   ),
    Y1 == Y2,
    G is abs( X1 - X2 ),
    G  ==  1, !.

%Adjuntos Verticales
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla2( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla( X2 , Y2 ))   ),
    X1 == X2,
    G is abs( Y1 - Y2 ),
    G  ==  1, !.

adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla2( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla( X2 , Y2 ))   ),
    H is abs(X1-X2),
    H == 1,
    G is abs( Y1 - Y2 ),
    G  ==  1, !.




%.................CONDICIONES DE LA CASILLA......................
%
% Una casilla puede estar libre u ocupada
%
libre( X , Y ):-
    ((casilla( X , Y ),!);casilla2(X,Y)),
    not(ocupada(X,Y)).

ocupada( X , Y ):-
    ((casilla( X , Y ),!);casilla2(X,Y)),
    (   jugadorV( X, Y );
        jugadorN( X, Y );
        jugadorZ( X, Y );
        jugadorB( X, Y );
        jugadorA( X, Y );
    computadoraR( X, Y )
    ).


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
    Diff is (X1 - X2),
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
    Diff is (X2 - X1) ,
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
    Y1 =< 9,
    Y2 < 9,
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
    Y1 >= 9,
    Y2 > 9,
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

%.................SALTOS MÚLTIPLES..............................
%

%------ Saltos dobles.

% Primer salto horizontal.
salto2Fact(X1,Y1,X2,Y2):- 
    %saltoFact(X1,Y1,X2,Y2);
    (   (X4 is X1 + 2, saltoFact(X1,Y1,X4,Y1),saltoFact(X4,Y1,X2,Y2));
        (X3 is X1 - 2, saltoFact(X1,Y1,X3,Y1),saltoFact(X3,Y1,X2,Y2))
    ),!.

%- BAJANDO

% Primer salto vertical.
salto2Fact(X1,Y1,X2,Y2):-
    %saltoFact(X1,Y1,X2,Y2);
    (
        Y1 > 10,
        Y3 is Y1 - 2,
        (
            (saltoFact(X1,Y1,X1,Y3), saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 + 2, saltoFact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
        )
    ), !.

salto2Fact(X1,Y1,X2,Y2):-
    %saltoFact(X1,Y1,X2,Y2);
    (
        Y1 =:= 10,
        Y3 is Y1 - 2,
        (
            (X3 is X1 - 1, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2));
            (X4 is X1 + 1, saltoFact(X1,Y1,X4,Y3),saltoFact(X4,Y3,X2,Y2))
         )
    ), !.

salto2Fact(X1,Y1,X2,Y2):-
    %saltoFact(X1,Y1,X2,Y2);
    (
        Y1=<9,
        Y3 is Y1 - 2,
        (
            (saltoFact(X1,Y1,X1,Y3),saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 - 2, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2))
        )
    ), !.


%- SUBIENDO
% Primer salto vertical.
salto2Fact(X1,Y1,X2,Y2):-
    %saltoFact(X1,Y1,X2,Y2);
    (
        Y1 >= 9,
        Y3 is Y1 + 2,
        (
            (saltoFact(X1,Y1,X1,Y3), saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 - 2, saltoFact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
        )
    ), !.

salto2Fact(X1,Y1,X2,Y2):-
    %saltoFact(X1,Y1,X2,Y2);
    (
        Y1 =:= 8,
        Y3 is Y1 + 2,
        (
            (X3 is X1 - 1, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2));
            (X4 is X1 + 1, saltoFact(X1,Y1,X4,Y3),saltoFact(X4,Y3,X2,Y2))
         )
    ), !.

salto2Fact(X1,Y1,X2,Y2):-
    %saltoFact(X1,Y1,X2,Y2);
    (
        Y1<8,
        Y3 is Y1 + 2,
        (
            (saltoFact(X1,Y1,X1,Y3),saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 + 2, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2))
        )
    ), !.

%------ Saltos Triples.

% Último salto Horizontal
salto3Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto2Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto2Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!.

% Bajando -- 2 Casos
% Caso Y1!=14
salto3Fact(X1,Y1,X2,Y2):-
    not(Y1=:=14),
    Y3 is Y2+2, 
    (   (salto2Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2)); 
        (X3 is X2-2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.

% Caso Y1=14 (Se salta el eje Y=9 en último salto)
salto3Fact(X1,Y1,X2,Y2):-
    Y1=:=14,
    Y3 is Y2+2, 
    (   (X3 is X2+1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))     
    ),!.

% Subiendo -- 2 Casos
% Caso Y1!=4
salto3Fact(X1,Y1,X2,Y2):-
    not(Y1=:=4),
    Y3 is Y2-2, 
    (   (salto2Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2)); 
        (X3 is X2-2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.

% Caso Y1=4 (Se salta el eje Y=9 en último salto)
salto3Fact(X1,Y1,X2,Y2):-
    Y1=:=4,
    Y3 is Y2-2, 
    (   (X3 is X2+1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2)) 
    ),!.

%------ Saltos Cuádruples.

% Último salto Horizontal
salto4Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto3Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto3Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!.

% Último salto vertical
%
% Bajando -- 2 casos 
salto4Fact(X1,Y1,X2,Y2):-
    not(Y1=:=16),
    Y3 is Y2+2,
    (   (salto3Fact(X1,Y1,X2,Y3),saltoFact(X2,Y3,X2,Y2));
        (X3 is X2-2, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.

salto4Fact(X1,Y1,X2,Y2):-
    Y1=:=16,
    Y3 is Y2+2,
    (   (X3 is X2+1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2)) 
    ),!.


% Subiendo
salto4Fact(X1,Y1,X2,Y2):-
    not(Y1=:=2),
    Y3 is Y2-2,
    (   (salto3Fact(X1,Y1,X2,Y3),saltoFact(X2,Y3,X2,Y2));
        (X3 is X2-2, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto3Fact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2))
    ),!.

salto4Fact(X1,Y1,X2,Y2):-
    Y1=:=2,
    Y3 is Y2+2,
    (   (X3 is X2+1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2)) 
    ),!.

%------ Saltos Quíntuples

% Último salto Horizontal:
salto5Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto4Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto4Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!.

% Último salto Vertical:
%
% Bajando.
salto5Fact(X1,Y1,X2,Y2):-
    Y3 is Y2+2,
    (   (salto4Fact(X1,Y1,X2,Y3), saltoFact(X1,Y3,X2,Y2));
        (X3 is X2+2, salto4Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.

% Subiendo.
salto5Fact(X1,Y1,X2,Y2):-
    Y3 is Y2-2,
    (   (salto4Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2));
        (X3 is X2+2, salto4Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.

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
    (   movVal2(X1 , Y1 , X2 , Y2 );
        pasoFact( X1 , Y1 , X2 , Y2 )
    ),!.

movVal2( X1 , Y1 , X2 , Y2 ):-
    (   salto5Fact(X1, Y1, X2, Y2);
        salto4Fact(X1, Y1, X2, Y2);
        salto3Fact(X1, Y1, X2, Y2);
        salto2Fact(X1, Y1, X2, Y2);
        saltoFact(X1, Y1, X2, Y2)
    ),! .



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
    jugadorV( 4 , 14 ),
    writeln("¡Gana el jugador VERDE!").

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
    computadoraR( 4 , 4 ),
    writeln("¡Gana el jugador ROJO!").



% ................EVALUACIONES PARA LA HEURISTICA............
%
% -- Tamaño de una lista
tamañoLista( [] , 0 ).
tamañoLista([ _ | R ], T ):-
    tamañoLista( R , T2 ),
    T is T2 + 1.

% -- Encontrar las posiciones de la computadoraR y jugadores, y guardarlas en una lista.
%
computadoraRPos(Pos):-
    findall( [ X , Y ], computadoraR(X , Y), Pos ).

jugadorVPos(Pos):-
    findall( [ X , Y ], jugadorV(X , Y), Pos ).

jugadorAPos(Pos):-
    findall( [ X , Y ], jugadorA(X , Y), Pos ).

jugadorBPos(Pos):-
    findall( [ X , Y ], jugadorB(X , Y), Pos ).

jugadorZPos(Pos):-
    findall( [ X , Y ], jugadorZ(X , Y), Pos ).

jugadorNPos(Pos):-
    findall( [ X , Y ], jugadorN(X , Y), Pos ).

% Obtiene lista de todos las fichas del tablero (cada una en una lista de [X,Y])
% Modo 2 jugadores
tableroPos(Pos, 1):-
    computadoraRPos(P1),
    jugadorVPos(P2),
    append([P1], [P2], Pos ).

% Modo 3 jugadores
tableroPos(Pos, 2):-
    computadoraRPos(P1),
    jugadorBPos(P2),
    append([P1], [P2], Pos ).

% Modo 4 jugadores
tableroPos(Pos, 3):-
    computadoraRPos(P1),
    jugadorAPos(P2),
    append([P1], [P2], Pos).

% Modo 6 jugadores
tableroPos(Pos, 4):-
    computadoraRPos(P1),
    jugadorAPos(P2), 
    append([P1], [P2], Pos ).


% -- Valor de las posiciones
%
% Primero evaluamos cuantos movimientos posibles tiene una pieza
% unitarios
%
miembro( X , [ X | _ ]).
miembro( X , [ _ | L ]):-
    miembro(X , L).

%movilidad( X , Y , Mov):-
 %   findall( X2 ,( Z is X - 1 , W is X + 1 , numlist(Z , W , DX), Z2 is Y - 1 , W2 is Y + 1 , numlist(Z2 , W2 , DY), miembro(X2 , DX), miembro( Y2 ,DY ), pasoFact( X , Y , X2 , Y2 )), M),
  %  tamañoLista( M , Mov).

% -- Distancia al eje central
%
% Caso EJE VERTICAL (Rojo-Verde)
%
distCentro(X , Y , Dist):-
    Y > 9,
    Aux is 18 - Y,
    Centro is Aux / 2,
    Dist is abs(Centro - X),!.

distCentro(X , Y , Dist):-
    Y =< 9 ,
    Centro is Y / 2,
    Dist is abs(Centro - X),!.

% Caso EJE DIAGONAL (Amarillo-Azul), 2 sub-casos
%
% Caso Y>9 (Mitad superior del tablero)
distCentroAZ(X, Y, Dist):-
    Y>9,
    Z is 5-((Y-9)*2),
    Dist is abs(Z-X)/2.
% Caso Y<9 (Mitad superior del tablero)
distCentroAZ(X, Y, Dist):-
    Y=<9,
    Z is (9-Y)+5,
    Dist is abs(Z-X)/2.

% Caso EJE DIAGONAL (Blanco-Negro), 2 sub-casos
%
% Caso Y>9 (Mitad superior del tablero)
distCentroBN(X, Y, Dist):-
    Y>9,
    Z is 5+(Y-9),
    Dist is abs(Z-X)/2.
% Caso Y<9 (Mitad superior del tablero)
distCentroBN(X, Y, Dist):-
    Y=<9,
    Z is 5-((9-Y)*2),
    Dist is abs(Z-X)/2.

% -- Distancia a la meta
%
% Para caso vertical (Rojo-Verde), Rojo: Dist = 18-Y, Verde: Dist = Y.
%
% Caso EJE DIAGONAL (Amarillo-Azul) 3 sub-casos:
%
% Caso dentro de la zona azul
distMetaAZ(X, Y, Dist):-
    Y<9,
    X>Y,
    Dist is 4-X+Y.
% Caso Y<=9 (Mitad inferior del tablero)
distMetaAZ(X, Y, Dist):-
    Y =< 9,
    X =< Y,
    Dist is Y-X+4.
% Caso Y>9 (Mitad superior del tablero)
distMetaAZ(X, Y, Dist):-
    Y > 9,
    X =< Y,
    Dist is 13-X.

% Caso EJE DIAGONAL 2 (Blanco-Negro) 2 sub-casos:
%
% Caso Y<= 9 (Mitad inferior del tablero)
distMetaBN(X, Y, Dist):-
    Y=<9,
    Dist is 3+X.

% Caso Y>9 (Mitad superior del tablero)
distMetaBN(X, Y, Dist):-
    Y > 9,
    Dist is X+Y-6.


%.....................HEURISTICA.............................
%

% En pos le damos tableroPos, que tiene todas las posiciones del
% tablero en una lista de listas, primero la de la computadora y luego la del jugadorV...
%

% --Modo 2 jugadores
heur(Pos, _ , Val, Modo):-
	Modo == 1 ,
	nth1(1 , Pos , Piezascompu), nth1(2 , Pos , PiezasjugadorV),
	heurComputadoraRAvanza(Piezascompu , Valcompu),
	heurJugadorVAvanza(PiezasjugadorV , ValjugadorV),
	Val is Valcompu - ValjugadorV.

% --Modo 3 jugadores
heur(Pos, _ , Val, Modo):-
    Modo == 2 ,
    nth1(1 , Pos , Piezascompu), nth1(2 , Pos , PiezasjugadorB),nth1(3 , Pos , PiezasjugadorZ),
    heurComputadoraRAvanza(Piezascompu , Valcompu),
    heurJugadorBAvanza(PiezasjugadorB , ValjugadorB),
    heurJugadorZAvanza(PiezasjugadorZ , ValjugadorZ),
    Val is Valcompu - (ValjugadorB + ValjugadorZ).

% --Modo 4 jugadores
heur(Pos, _ , Val, Modo):-
    Modo == 3 ,
    nth1(1 , Pos , Piezascompu), 
    nth1(2 , Pos , PiezasjugadorA),nth1(3 , Pos , PiezasjugadorV),nth1(4 , Pos , PiezasjugadorZ),
    heurComputadoraRAvanza(Piezascompu , Valcompu),
    heurJugadorAAvanza(PiezasjugadorA , ValjugadorA),
    heurJugadorZAvanza(PiezasjugadorZ , ValjugadorZ),
    heurJugadorVAvanza(PiezasjugadorV , ValjugadorV),
    Val is Valcompu - (ValjugadorA + ValjugadorZ + ValjugadorV).

% --Modo 6 jugadores
heur(Pos, _ , Val, Modo):-
    Modo == 4 ,
    nth1(1 , Pos , Piezascompu), 
    nth1(2 , Pos , PiezasjugadorA),nth1(3 , Pos , PiezasjugadorB),
    nth1(4 , Pos , PiezasjugadorV),nth1(5 , Pos , PiezasjugadorZ),nth1(6 , Pos , PiezasjugadorN),
    heurComputadoraRAvanza(Piezascompu , Valcompu),
    heurJugadorAAvanza(PiezasjugadorA , ValjugadorA),
    heurJugadorZAvanza(PiezasjugadorZ , ValjugadorZ),
    heurJugadorVAvanza(PiezasjugadorV , ValjugadorV),
    heurJugadorBAvanza(PiezasjugadorB , ValjugadorB),
    heurJugadorNAvanza(PiezasjugadorN , ValjugadorN),
    Val is Valcompu - (ValjugadorA + ValjugadorZ + ValjugadorV+ValjugadorB+ValjugadorN).


% -- Aplicación de heurística a piezas de la COMPUTADORA (ROJO)
%
% Se aplica la heurística a la pieza en la posición X,Y
% Es igual a valorComputadoraR (Podemos quitar valorComputadoraR)
heurAPiezaComputadoraR( X , Y , Val ):-
    distCentro( X , Y , Dist ),
    Val is (( 18 - Y ) * 4 - Dist * 3 ).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
% Es igual a valorTableroComputadora (Podemos quitar valorTableroComputadora)
heurComputadoraRAvanza( [] , 0 ).
heurComputadoraRAvanza([ T | Q ], Val ):-
	nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaComputadoraR( X , Y , Val2 ),
	heurComputadoraRAvanza( Q , Val1 ),
    Val is Val1 + Val2.

% -- Aplicación de heurística a piezas del JUGADOR VERDE
%
% Se aplica la heurística a la pieza en la posición X,Y
% Es igual a valorJugadorV (podemos quitar valorJugadorV)
heurAPiezaJugadorV( X , Y , Val ):-
    distCentro( X , Y , Dist ),
    Val is ((Y*4) - (Dist*3)).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
% Es igual a valorTableroJugador (podemos quitar valorTableroJugador)
heurJugadorVAvanza( [] , 0 ).
heurJugadorVAvanza([ T | Q ], Val ):-
	nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorV( X , Y , Val2 ),
	heurJugadorVAvanza( Q , Val1 ),
    Val is Val1 + Val2.

% -- Aplicación de heurística a piezas del JUGADOR AMARILLO
%
% Se aplica la heurística a la pieza en la posición X,Y
heurAPiezaJugadorA(X, Y, Val):-
    distCentroAZ(X,Y,DistC),
    distMetaAZ(X,Y,DistM),
    Val is ((16-DistM)*4)-(DistC*3).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
heurJugadorAAvanza([], 0).
heurJugadorAAvanza([T | Q], Val):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorA(X, Y, Val2),
    heurJugadorAAvanza(Q,Val1),
    Val is Val1 + Val2.

% -- Aplicación de heurística a piezas del JUGADOR AZUL
%
% Se aplica la heurística a la pieza en la posición X,Y
heurAPiezaJugadorZ(X, Y, Val):-
    distCentroAZ(X,Y,DistC),
    distMetaAZ(X,Y,DistM),
    Val is (DistM*4)-(DistC*3).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
heurJugadorZAvanza([], 0).
heurJugadorZAvanza([T | Q], Val):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorZ(X, Y, Val2),
    heurJugadorZAvanza(Q,Val1),
    Val is Val1 + Val2.

% -- Aplicación de heurística a piezas del JUGADOR BLANCO
%
% Se aplica la heurística a la pieza en la posición X,Y
heurAPiezaJugadorB(X, Y, Val):-
    distCentroBN(X,Y,DistC),
    distMetaBN(X,Y,DistM),
    Val is (DistM*4)-(DistC*3).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
heurJugadorBAvanza([], 0).
heurJugadorBAvanza([T | Q], Val):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorB(X, Y, Val2),
    heurJugadorBAvanza(Q,Val1),
    Val is Val1 + Val2.

% -- Aplicación de heurística a piezas del JUGADOR NEGRO
%
% Se aplica la heurística a la pieza en la posición X,Y
heurAPiezaJugadorN(X, Y, Val):-
    distCentroBN(X,Y,DistC),
    distMetaBN(X,Y,DistM),
    Val is ((16-DistM)*4)-(DistC*3).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
heurJugadorNAvanza([], 0).
heurJugadorNAvanza([T | Q], Val):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorN(X, Y, Val2),
    heurJugadorNAvanza(Q,Val1),
    Val is Val1 + Val2.




%...................POS. ACCESIBLES............................
%

% Revisa si es posible seguir saltando desde una posicion X,Y dada
% Entonces sirve para ver si puede realizar cadenas de saltos.
saltosExtra([],[]).
saltosExtra([L|Q], Res):-
    saltosExtra(Q, Res1),
    nth1(1,L,X), nth1(2,L,Y),
    Lowx is X-2, Highx is X+2,
    Lowy is Y-2, Highy is Y+2,
    numlist(Lowx,Highx,DX),
    findall([X2, Y2],
            (miembro( Y2 , [Lowy,Y,Highy] ), miembro( X2 , DX ),libre(X2,Y2), saltoFact( X , Y , X2 , Y2 )),
            M),
    union(M,Res1,Res).

    
% Devuelve la lista de movimientos posibles desde la posicion X, Y
espaciosDisponiblesComputadoraR( X , Y , M):-
    numlist( 1 , 9 , DX ),numlist( 1 , Y , DY ), 
    findall([X2, Y2],
            (miembro( Y2 , DY ), miembro( X2 , DX ),libre(X2,Y2), saltoFact( X , Y , X2 , Y2 )),
            M1),
    findall([Xp, Yp],
            (miembro( Yp , DY ), miembro( Xp , DX ),libre(Xp,Yp), pasoFact( X , Y , Xp , Yp )),
            Mp),
    saltosExtra(M1,M2),
    saltosExtra(M2,M3),
    saltosExtra(M3,M4),
    saltosExtra(M4,M5),
    union(M1,M2,R12),
    union(R12,M3,R123),
    union(R123,M4,R1234),
    union(R1234,M5,Maux),
    union(Maux,Mp,Mres),
    subtract(Mres,[[X,Y]],M).

espaciosDisponiblesJugadorV( X , Y , M ):-
    numlist( 1 , 9 , DX ),numlist( 1, 17 , DY ), 
    findall([ X2 , Y2 ],
            (miembro( Y2 , DY ), miembro( X2 , DX ),libre(X2,Y2), saltoFact( X , Y , X2 , Y2 )),
            M1),
    findall([Xp, Yp],
            (miembro( Yp , DY ), miembro( Xp , DX ),libre(Xp,Yp), pasoFact( X , Y , Xp , Yp )),
            Mp),
    saltosExtra(M1,M2),
    saltosExtra(M2,M3),
    saltosExtra(M3,M4),
    saltosExtra(M4,M5),
    union(M1,M2,R12),
    union(R12,M3,R123),
    union(R123,M4,R1234),
    union(R1234,M5,Maux),
    union(Maux,Mp,Mres),
    subtract(Mres,[[X,Y]],M).

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

%
% Combina elem con todos los elementos de la lista y lo guarda en Lista.

% Junta todas las opciones posibles del jugador en turno con las del otro jugador
% Entonces quedan todas las configuraciones posibles del tablero completo.
distribucionJugador( [] , _ , [] ).
distribucionJugador([ T | Q ], Elem , Lista ):-
    distribucionJugador( Q , Elem , L2 ),
    append([ T ], [ Elem ], L1 ),
    append([ L1 ], L2 , Lista ). 

distribucionComputadoraR( [] , _ , [] ) .
distribucionComputadoraR([ T | Q ], Elem , Lista ):-
    distribucionComputadoraR( Q , Elem , L2 ),
    append([ Elem ], [ T ], L1 ),
    append([ L1 ], L2 , Lista ).


% -- Sucesores de la posición actual
% Todos los próximos movimientos posibles y como queda la configuración
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
	espaciosDisponibles( Xi , Yi , L , Turno , AI ), %Obtiene posibles movimientos desde Xi, Yi en L
	subtract( Pos , [[ Xi , Yi ]], G ), %Quita de la lista de fichas la ficha Xi,Yi, pues es la que se movió
	succL( L , G , PosList1 ), %Regresa una lista de listas, donde cada elemento contiene las posiciones de las 10 piezas, con cada mov posible de L
	succCalc( Q , Pos , PosList2 , Turno , AI ),
	append( PosList1 , PosList2 , PosList ). % Regresa una lista de listas con todas las configuraciones posibles del jugador en turno


% SUCC se apoya de las dos funciones anteriores y lo que hace es hacer
% un movimineto, meterlo a una lista y meter como quedan todas las demas
% posiciones

% Obtiene TODOS los movimientos posibles de la COMPUTADORA.
succ([Compu|Players], Turno , PosList , Modo ):-
	Modo == 1 ,
    Turno == 1 ,
	succCalc( Compu , Compu , Temp , Turno , Modo ), %Temp tiene todos las configuraciones posibles de las fichas del jugador en turno. (Lista de listas)
    nth1(1,Players,JugV),
    distribucionJugador(Temp , JugV , PosList ),!. % Poslist tiene todas las configuraciones posibles del tablero.
    %Poslist = [ [Compu1, JugadorV],[Compu2, JugadorV], ...] donde Compu_i y Jugador son listas de parejas [X,Y] de las 10 fichas

% -- Modo de juego 2 jugadores
% Obtiene TODOS los movimientos posibles del JUGADOR_V.
succ([Compu|Players], Turno , PosList , Modo ):-
	Modo == 1 ,
    Turno == 2 ,
    nth1(1,Players,Jug),
	succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
	distribucionComputadoraR( Temp , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero


% -- Modo de juego 6 jugadores
% Obtiene TODOS los movimientos posibles del JUGADOR_A.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 2 ,
    nth1(1,Players,Jug),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux),
    append(Temp, Aux, Players2),
    distribucionComputadoraR( Players2 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_B.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 3 ,
    nth1(2,Players,Jug), nth1(1,Players,Jug1),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux), subtract(Aux, Jug1, Aux2),
    append(Jug1, Temp, Players2), append(Players2, Aux2, Players3),
    distribucionComputadoraR( Players3 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_V.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 4 ,
    nth1(3,Players,Jug),nth1(1,Players,Jug1),nth1(2,Players,Jug2),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux), subtract(Aux, Jug1, Aux2), subtract(Aux2, Jug2, Aux3),
    append(Jug1, Jug2, Players2), append(Players2, Temp, Players3), append(Players3, Aux3, Players4),
    distribucionComputadoraR( Players4 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_Z.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 5 ,
    nth1(4,Players,Jug),nth1(5,Players,Jug5),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux), subtract(Aux, Jug5, Aux2),
    append(Aux2,Temp,Players2), append(Players2,Jug5,Players3),
    distribucionComputadoraR( Players3 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_N.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 6 ,
    nth1(5,Players,Jug),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux),
    append(Aux,Temp,Players2),
    distribucionComputadoraR( Players2 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero


% ...................MINIMAX CON PODA ALPHA BETA........................

% MAX = 1, MIN = 2
siguienteJugador( 1 , 2 ).
siguienteJugador( 2 , 1 ).

copia(Posicion , Valor , Posicion , Valor).

% El siguiente metodo guarda el mejor movimiento posible de dos
% que compara y servirá para el metodo minimax
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
% Lista vacía
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

% ----- Minimax alpha beta: llamada
minimaxab( Posicion , Turno , Profundidad , MejorMov , Valor , Modo ):-
	succ( Posicion , Turno , X , Modo ),% Simulación del movimiento, X = lista de posiciones dispués de jugar
	siguienteJugador( Turno , OtroJugador ), % Cambio de jugador
	decr( Profundidad , P2 ), % Profundidad de disminución
	alphabeta( X , OtroJugador , P2 , -999 , 999 , MejorMov , Valor , Modo ).  % Alpha y Beta se inicializan a -999 y 999


% alphabeta(ListaTablero, Turno, Profundidad, alpha, beta, MejorMov, Valor, ModoJuego)
% CASO BASE: Si ya no hay mas jugadores (No hay lista de sus fichas)
alphabeta( [] , _ , _ , _ , _ , _ , _ , _ ).

% CASO LIMITE DE PROFUNDIDAD MINIMIZA (BUSCA EL MINIMO)
alphabeta([ E | L ], 1 , P , A , B , MejorMov , Valor , Modo ):-
        testDepth( P , E , 1 ), % hoja final alcanzada o final del juego
        heur( E , 1 , ValE , Modo ), % Evaluación del "tablero E" en ValE
        (   ( A =< ValE , %if A<=ValE:
                alphabeta( L , 1 , P , A , B , MejorMovL , ValL , Modo ),
                recordMin( ValE , ValL , E , MejorMovL , MejorMov , Valor ));%Else:
            copia( E , ValE , MejorMov , Valor )).  % Memorización de los mejores, o es el y anterior o copiamos lo que teniamos

% CASO LIMITE DE PROFUNDIDAD MAXIMZA (BUSCA EL MAXIMO)
alphabeta([ E | L ], 2 , P , A , B , MejorMov , V , Modo ):-
        testDepth( P , E , 2 ),
        heur( E , 2 , ValE , Modo ),
        (   ( B >= ValE ,
                alphabeta( L , 2 , P , A , B , MejorMovL , ValL , Modo ),
                recordMax( ValE , ValL , E , MejorMovL , MejorMov , V ));
                copia( E , ValE , MejorMov , V )).

alphabeta([ E | L ], 1 , P , A , B , MejorMov , V , Modo ):-
        succ( E , 1 , X , Modo ),
        not(vacio( X )),
        P \= 0 ,
        decr( P , P2 ),
        alphabeta( X , 2 , P2 , A , B , MejorMovX , ValX , Modo ),
        (   ( ValX >= A ,
                min( ValX , B , Bbis ),
                alphabeta( L , 1 , P , A , Bbis , MejorMovL , ValL , Modo ),
                recordMin( ValX , ValL , E , MejorMovL , MejorMov , V ));
            copia( MejorMovX , ValX , MejorMov , V )).


alphabeta([ E | L ], 2 , P , A , B , MejorMov , V , Modo ):-
        succ( E , 2 , X , Modo ),
        not(vacio( X )),
        P \= 0 ,
        decr( P , P2 ),
        alphabeta( X , 1 , P2 , A , B , MejorMovX , ValX , Modo ),
        (   ( ValX =< B ,
                max( ValX , A , Abis ),
                alphabeta( L , 2 , P , Abis , B , MejorMovL , ValL , Modo ),
                recordMax( ValX , ValL , E , MejorMovL , MejorMov , V ));
            copia( MejorMovX , ValX , MejorMov , V )).


% .........................Dibujar..............................
%

% Draw o si el cuadro existe, 1 para la CPU, 2 para el jugador
draw( A , B ):-
    computadoraR( A , B ),
    ansi_format([bold,fg(red)], 'R~w', [""]),
    !.
draw( A , B ):-
    jugadorA( A , B ),
    ansi_format([bold,fg(yellow)], 'A~w', [""]),
    !.
draw( A , B ):-
    jugadorB( A , B ),
    ansi_format([bold,fg(magenta)], 'B~w', [""]),
    !.
draw( A , B ):-
    jugadorV( A , B ),
    ansi_format([bold,fg(green)], 'V~w', [""]),
    !.
draw( A , B ):-
    jugadorZ( A , B ),
    ansi_format([bold,fg(blue)], 'Z~w', [""]),
    !.
draw( A , B ):-
    jugadorN( A , B ),
    ansi_format([bold,fg(black)], 'N~w', [""]),
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
    write("NaN").

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
write('    '),draw(-3,13),write(' '), draw(-2,13),write(' '), draw(-1,13),write(' '), draw(0,13),write(' '), draw(1,13),write(' '), draw(2,13),write(' '), draw(3,13),write(' '), draw(4,13),write(' '),draw(5,13), write(' '), draw(6,13), write(' '), draw(7,13), write(' '), draw(8,13), write(' '), draw(9,13),write('    ').
drawLinea12:-
writeln(''),
write('     '), draw(-2,12), write( ' ' ), draw(-1,12), write( ' ' ), draw(0,12), write( ' ' ), draw(1,12), write( ' ' ), draw(2,12), write( ' ' ), draw(3,12), write( ' ' ), draw(4,12), write( ' ' ), draw(5,12), write( ' ' ), draw(6,12), write(' '), draw(7,12), write(' '), draw(8,12), write(' '), draw(9,12),write( '   ' ).
drawLinea11:-
writeln(''),
write('      '),draw(-1,11),write(' '),draw(0,11),write(' '),draw(1,11),write(' '),draw(2,11),write(' '),draw(3,11),write(' '),draw(4,11),write(' '),draw(5,11),write(' '),draw(6,11),write(' '),draw(7,11), write(' '), draw(8,11), write(' '), draw(9,11),write('  ').
drawLinea10:-
writeln(''),
write('       '),draw(0,10),write(' '),draw(1,10),write(' '),draw(2,10),write(' '),draw(3,10),write(' '),draw(4,10),write(' '),draw(5,10),write(' '),draw(6,10),write(' '),draw(7,10),write(' '),draw(8,10), write(' '), draw(9,10),write('  ').
drawLinea9:-
writeln(''),
write('        '),draw(1,9),write(' '),draw(2,9),write(' '),draw(3,9),write(' '),draw(4,9),write(' '),draw(5,9),write(' '),draw(6,9),write(' '),draw(7,9),write(' '),draw(8,9), write(' '), draw(9,9), write(' ').
drawLinea8:-
writeln(''),
write('       '),draw(0,8),write(' '),draw(1,8),write(' '),draw(2,8),write(' '),draw(3,8),write(' '),draw(4,8),write(' '),draw(5,8),write(' '),draw(6,8),write(' '),draw(7,8),write(' '),draw(8,8), write(' '), draw(9,8),write('  ').
drawLinea7:-
writeln(''),
write('      '),draw(-1,7),write(' '),draw(0,7),write(' '),draw(1,7),write(' '),draw(2,7),write(' '),draw(3,7),write(' '),draw(4,7),write(' '),draw(5,7),write(' '),draw(6,7),write(' '),draw(7,7), write(' '), draw(8,7), write(' '), draw(9,7),write('  ').
drawLinea6:-
writeln(''),
write('     '), draw(-2,6), write( ' ' ), draw(-1,6), write( ' ' ), draw(0,6), write( ' ' ), draw(1,6), write( ' ' ), draw(2,6), write( ' ' ), draw(3,6), write( ' ' ), draw(4,6), write( ' ' ), draw(5,6), write( ' ' ), draw(6,6), write(' '), draw(7,6), write(' '), draw(8,6), write(' '), draw(9,6),write( '   ' ).
drawLinea5:-
writeln(''),
write('    '),draw(-3,5),write(' '), draw(-2,5),write(' '), draw(-1,5),write(' '), draw(0,5),write(' '), draw(1,5),write(' '), draw(2,5),write(' '), draw(3,5),write(' '), draw(4,5),write(' '),draw(5,5), write(' '), draw(6,5), write(' '), draw(7,5), write(' '), draw(8,5), write(' '), draw(9,5),write('    ').

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
jugarComputadoraR( Modo ):-
    tty_clear,
    tableroPos( Pos, Modo ),
    minimaxab( Pos , 1 , Modo , MejorMov , _ , 1 ),
    !,
    retractall(computadoraR( _ , _ )),
    nth1( 1 , MejorMov , Mejor ),
    forall(

        miembro( Elem , Mejor ),
        (
            nth1( 1 , Elem , X1 ), nth1( 2 , Elem , Y1 ), assert(computadoraR( X1 , Y1 ))
         )

        ),
    tty_clear,
    drawTablero,
    modoJuego(M),
    (   (M==1, writeln("Turno del VERDE"));
        (M==2, writeln("Turno del BLANCO"));
        (M==3, writeln("Turno del AMARILLO"));
        (M==4, writeln("Turno del AMARILLO"))
        ),!.

% -- Mover las fichas
%
% -- Mover Computadora ROJA
moverComputadoraR( X1 , Y1 , X2 , Y2 ):-
    tty_clear,
    computadoraR( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(computadoraR( X1 , Y1 )),
    assert(computadoraR( X2 , Y2 )),
    modoJuego(M),
    drawTablero,
    (   (M==1, writeln("Turno del VERDE"));
        (M==2, writeln("Turno del BLANCO"));
        (M==3, writeln("Turno del AMARILLO"));
        (M==4, writeln("Turno del AMARILLO"))
        ),
    !.

 % -- Mover Jugador VERDE   
moverJugadorV( X1 , Y1 , X2 , Y2 ):-
    tty_clear,
    jugadorV( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorV( X1 , Y1 )),
    assert(jugadorV( X2 , Y2 )),
    modoJuego(M),
    drawTablero,
    (   (M==1, jugarComputadoraR(1));
        (M==3, writeln("Turno del AZUL"));
        (M==4, writeln("Turno del AZUL"))
        ),
    !.

% -- Mover Jugador AMARILLO
moverJugadorA( X1 , Y1 , X2 , Y2 ):-
    tty_clear,
    jugadorA( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorA( X1 , Y1 )),
    assert(jugadorA( X2 , Y2 )),
    modoJuego(M),
    drawTablero,
    (   (M==3, writeln("Turno del VERDE"));
        (M==4, writeln("Turno del BLANCO"))
        ),
    !.

% -- Mover Jugador BLANCO
moverJugadorB( X1 , Y1 , X2 , Y2 ):-
    tty_clear,
    jugadorB( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorB( X1 , Y1 )),
    assert(jugadorB( X2 , Y2 )),
    modoJuego(M),
    drawTablero,
    (   (M==2, writeln("Turno del AZUL"));
        (M==4, writeln("Turno del VERDE"))
        ),
    !.

% -- Mover Jugador NEGRO
moverJugadorN( X1 , Y1 , X2 , Y2 ):-
    tty_clear,
    jugadorB( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorB( X1 , Y1 )),
    assert(jugadorB( X2 , Y2 )),
    modoJuego(M),
    drawTablero,
    (   (M==4, jugarComputadoraR(1))
        ),
    !.

% -- Mover Jugador AZUL
moverJugadorZ( X1 , Y1 , X2 , Y2 ):-
    tty_clear,
    jugadorZ( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorZ( X1 , Y1 )),
    assert(jugadorZ( X2 , Y2 )),
    modoJuego(M),
    drawTablero,
    (   (M==2, jugarComputadoraR(1));
        (M==3, jugarComputadoraR(1));
        (M==4, writeln("Turno del NEGRO"))
        ),
    !.

newGame:-
    tty_clear,
    retract(modoJuego(_)),
    retractall(computadoraR( _ ,_ )),
    retractall(jugadorV( _ , _ )),
    retractall(jugadorB( _ , _ )),
    retractall(jugadorZ( _ , _ )),
    retractall(jugadorN( _ , _ )),
    retractall(jugadorA( _ , _ )),

    writeln("************************************"),
    writeln("            DAMAS CHINAS"),
    writeln("************************************"),
    writeln(" "),
    writeln("Selecciona la opción deseada para jugar:"),
    writeln("-> Escribe el número correspondiente seguido de un punto (.) <-"),
    writeln("NOTA: La computadora siempre mueve las fichas ROJAS "),
    writeln("1. 1H - 1C"),
    writeln("2. 2H - 1C"),
    writeln("3. 3H - 1C"),
    writeln("4. 5H - 1C"),
    read(X),
    tty_clear,
    iniciarJuego(X).


iniciarJuego(X):-
    X==1,
    inicialRojo,
    inicialVerde,
    assert(modoJuego(1)),
    drawTablero,
    writeln("Turno del VERDE"),!.

iniciarJuego(X):-
    X==2,
    inicialRojo,
    inicialBlanco,
    inicialAzul,
    assert(modoJuego(2)),
    drawTablero,
    writeln("Turno del BLANCO"),!.

iniciarJuego(X):-
    X==3,
    inicialRojo,
    inicialVerde,
    inicialAmarillo,
    inicialAzul,
    assert(modoJuego(3)),
    drawTablero,
    writeln("Turno del AMARILLO"),!.

iniciarJuego(X):-
    X==4,
    inicialRojo,
    inicialVerde,
    inicialAzul,
    inicialNegro,
    inicialBlanco,
    inicialAmarillo,
    assert(modoJuego(4)),
    drawTablero,
    writeln("Turno del AMARILLO"),!.

inicialRojo:-
    assert(computadoraR( 1 , 17 )),
    assert(computadoraR( 1 , 16 )),
    assert(computadoraR( 2 , 16 )),
    assert(computadoraR( 1 , 15 )),
    assert(computadoraR( 2 , 15 )),
    assert(computadoraR( 3 , 15 )),
    assert(computadoraR( 1 , 14 )),
    assert(computadoraR( 2 , 14 )),
    assert(computadoraR( 3 , 14 )),
    assert(computadoraR( 4 , 14 )).

inicialVerde:-
    assert(jugadorV( 1 , 1 )),
    assert(jugadorV( 1 , 2 )),
    assert(jugadorV( 2 , 2 )),
    assert(jugadorV( 1 , 3 )),
    assert(jugadorV( 2 , 3 )),
    assert(jugadorV( 3 , 3 )),
    assert(jugadorV( 1 , 4 )),
    assert(jugadorV( 2 , 4 )),
    assert(jugadorV( 3 , 4 )),
    assert(jugadorV( 4 , 4 )).

inicialAmarillo:-
    assert(jugadorA( -3 , 13 )),
    assert(jugadorA( -2 , 13 )),
    assert(jugadorA( -1 , 13 )),
    assert(jugadorA( 0 , 13 )),
    assert(jugadorA( -2 , 12 )),
    assert(jugadorA( -1 , 12 )),
    assert(jugadorA( 0 , 12 )),
    assert(jugadorA( -1 , 11 )),
    assert(jugadorA( 0 , 11 )),
    assert(jugadorA( 0 , 10 )).

inicialBlanco:-
    assert(jugadorB( 0 , 8 )),
    assert(jugadorB( 0 , 7 )),
    assert(jugadorB( -1 , 7 )),
    assert(jugadorB( 0 , 6 )),
    assert(jugadorB( -1 , 6 )),
    assert(jugadorB( -2 , 6 )),
    assert(jugadorB( 0 , 5 )),
    assert(jugadorB( -1 , 5 )),
    assert(jugadorB( -2 , 5 )),
    assert(jugadorB( -3 , 5 )).

inicialAzul:-
    assert(jugadorZ( 6 , 5 )),
    assert(jugadorZ( 7 , 5 )),
    assert(jugadorZ( 8 , 5 )),
    assert(jugadorZ( 9 , 5 )),
    assert(jugadorZ( 7 , 6 )),
    assert(jugadorZ( 8 , 6 )),
    assert(jugadorZ( 9 , 6 )),
    assert(jugadorZ( 8 , 7 )),
    assert(jugadorZ( 9 , 7 )),
    assert(jugadorZ( 9 , 8 )).

inicialNegro:-
    assert(jugadorN( 6 , 13 )),
    assert(jugadorN( 7 , 13 )),
    assert(jugadorN( 8 , 13 )),
    assert(jugadorN( 9 , 13 )),
    assert(jugadorN( 7 , 12 )),
    assert(jugadorN( 8 , 12 )),
    assert(jugadorN( 9 , 12 )),
    assert(jugadorN( 8 , 11 )),
    assert(jugadorN( 9 , 11 )),
    assert(jugadorN( 9 , 10 )).

    



