% Proyecto DAMAS CHINAS

% Santiago Borobia ....165323
% Manuel Fernandez ....166496
% Eugenio García ..... 164592

% El siguiente código se escribió para desarrollar una inteligencia artificial
% con la capacidad de jugar a las damas chinas.

% Dividimos el código en secciones para facilitar su lectura.


%................. I. FICHAS .............................
%
% Cada casilla del tablero se compone de dos coordenadas (X,Y).
% X pertenece de (-3,14)
% Y pertenece de (1,17)
% De esta forma cada ficha se define como:
    % nombreJugador(X, Y).

% Hay 6 jugadores posibles: Rojo, Amarillo, Moradas, Verde, Azul y Negro.
% La COMPUTADORA siempre mueve las fichas ROJAS

% Las fichas son componentes dinamicos, ya que se alteran a lo largo del
% juego.
%

% Este predicado sirve como una variable para saber el modo de juego,
% pues hay 4 distintos modos posibles: 2, 3, 4 o 6 jugadores.
:- dynamic modoJuego/1.
modoJuego(1).

% Posición inicial de las fichas ROJAS (Computadora)
:- dynamic computadoraR/ 2.
%computadoraR( 1 , 17 ).
%computadoraR( 1 , 16 ).
%computadoraR( 2 , 16 ).
%computadoraR( 1 , 15 ).
%computadoraR( 2 , 15 ).
%computadoraR( 3 , 15 ).
%computadoraR( 1 , 14 ).
%computadoraR( 2 , 14 ).
%computadoraR( 3 , 14 ).
%computadoraR( 4 , 14 ).

% Posición inicial de las fichas AMARILLAS
:- dynamic jugadorA/ 2.
%jugadorA( -3 , 13 ).
%jugadorA( -2 , 13 ).
%jugadorA( -1 , 13 ).
%jugadorA( 0 , 13 ).
%jugadorA( -2 , 12 ).
%jugadorA( -1 , 12 ).
%jugadorA( 0 , 12 ).
%jugadorA( -1 , 11 ).
%jugadorA( 0 , 11 ).
%jugadorA( 0 , 10 ).

% Posición inicial de las fichas MORADAS
:- dynamic jugadorB/ 2.
%jugadorB( 0 , 8).
%jugadorB( 0 , 7 ).
%jugadorB( -1 , 7 ).
%jugadorB( 0 , 6 ).
%jugadorB( -1 , 6 ).
%jugadorB( -2 , 6 ).
%jugadorB( 0 , 5 ).
%jugadorB( -1 , 5 ).
%jugadorB( -2 , 5 ).
%jugadorB( -3 , 5 ).
% NOTA: primero se pensó en hacer las fichas BLANCAS, pero luego se cambió
% a MORADAS, porque el blanco se confunde con el fondo del tablero.

% Posición inicial de las fichas VERDES
:- dynamic jugadorV/ 2.
%jugadorV( 1 , 1 ).
%jugadorV( 1 , 2 ).
%jugadorV( 2 , 2 ).
%jugadorV( 1 , 3 ).
%jugadorV( 2 , 3 ).
%jugadorV( 3 , 3 ).
%jugadorV( 1 , 4 ).
%jugadorV( 2 , 4 ).
%jugadorV( 3 , 4 ).
%jugadorV( 4 , 4 ).

% Posición inicial de las fichas AZULES
:- dynamic jugadorZ/ 2.
%jugadorZ( 6 , 5 ).
%jugadorZ( 7 , 5 ).
%jugadorZ( 8 , 5 ).
%jugadorZ( 9 , 5 ).
%jugadorZ( 7 , 6 ).
%jugadorZ( 8 , 6 ).
%jugadorZ( 9 , 6 ).
%jugadorZ( 8 , 7 ).
%jugadorZ( 9 , 7 ).
%jugadorZ( 9 , 8 ).
% NOTA: Las fichas azules tienen la letra "Z" porque las amarillas ya ocupaban la "A"

% Posición inicial de las fichas NEGRAS
:- dynamic jugadorN/ 2.
%jugadorN( 6 , 13 ).
%jugadorN( 7 , 13 ).
%jugadorN( 8 , 13 ).
%jugadorN( 9 , 13 ).
%jugadorN( 7 , 12 ).
%jugadorN( 8 , 12 ).
%jugadorN( 9 , 12 ).
%jugadorN( 8 , 11 ).
%jugadorN( 9 , 11 ).
%jugadorN( 9 , 10 ).


% Los siguientes predicados sirven para encontrar las posiciones de las fichas
% de la computadora y los jugadores.
% Las posiciones se almacenan en una lista de parejas ordenadas (X, Y).
% => Lista = [[X1, Y1], [X2, Y2], ...]
% El predicado findall/3 realiza una búsqueda exhaustiva para encontrar 
% todas las posibilidades que cumplan cierto objetivo:
%       findall(template, objetivo, Resultado)
% * El resultado se almacena en la forma del template *
%
% Encuentra todas las fichas ROJAS
computadoraRPos(Pos):-
    findall( [ X , Y ], computadoraR(X , Y), Pos ).

% Encuentra todas las fichas VERDES
jugadorVPos(Pos):-
    findall( [ X , Y ], jugadorV(X , Y), Pos ).

% Encuentra todas las fichas AMARILLAS
jugadorAPos(Pos):-
    findall( [ X , Y ], jugadorA(X , Y), Pos ).

% Encuentra todas las fichas MORADAS
jugadorBPos(Pos):-
    findall( [ X , Y ], jugadorB(X , Y), Pos ).

% Encuentra todas las fichas AZULES
jugadorZPos(Pos):-
    findall( [ X , Y ], jugadorZ(X , Y), Pos ).

% Encuentra todas las fichas NEGRAS
jugadorNPos(Pos):-
    findall( [ X , Y ], jugadorN(X , Y), Pos ).


% Los siguientes predicados obtienen una lista con todas las 
% fichas del TABLERO (cada una en una lista de [X,Y]).
% Segun el modo de juego, se buscan las fichas de los jugadores existentes.
%
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

%................. II. CONDICIONES DEL TABLERO ......................
%

% -- Predicados para verificar la existencia de una casilla en la posicion X, Y.
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


% -- Verifica si dos casillas son diferentes entre ellas.
%
diferentes(X1 , Y1 , X2 , Y2):-
    (X1 \== X2,!;
    Y1 \== Y2).


% -- Verifica si dos casillas son adyacentes entre ellas
% Primero se verifica que las dos casillas existan en el tablero.
% Luego se revisa que, en efecto, estén adjuntas.

% Adjuntos horizontales
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 )); %Revisa que las casillas existan
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    Y1 == Y2, %Estan en la misma fila
    G is abs( X1 - X2 ),
    G  ==  1, !. %La distancia entre ellas es de 1

% Adjuntos verticales:
% Hay distintos casos, debido a la disposicion del tablero.
% Dos casillas juntas pueden compartir la misma X,
% o bien tener una distancia de 1 entre ellas.
%
% Caso 1: X1==X2
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 )); %Revisa que las casillas existan
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    X1 == X2, %La X es igual para las dos
    G is abs( Y1 - Y2 ),
    G  ==  1, !. %La distancia entre ellas es de 1

% Caso 2: X1 - X2 = +-1 
%
% Subiendo (X2 está en una fila más alta que X1)
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 < 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))), %Revisa que las casillas existan
    H is X1-X2,
    H == -1, % X2 está a 1 posición a la derecha de X1
    G is Y1-Y2,
    G  ==  -1, !. % Distancia entre ellas es de 1

adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 >= 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2,
    H == 1, % X2 está a 1 posición a la izquierda de X1
    G is Y1 - Y2,
    G  ==  -1, !. % Distancia entre ellas es de 1

% Bajando (X2 está en una fila más baja que X1)
adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 =< 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2, % X2 está a 1 posición a la izquierda de X1
    H == 1,
    G is Y1-Y2,
    G  ==  1, !. % Distancia entre ellas es de 1

adjuntos( X1 , Y1 , X2 , Y2 ):-
    Y1 > 9,
    (   (casilla( X1 , Y1 ), casilla( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla2( X2 , Y2 ))),
    H is X1-X2, % X2 está a 1 posición a la derecha de X1
    H == -1,
    G is Y1 - Y2,
    G  ==  1, !. % Distancia entre ellas es de 1

% Casos especiales:
% 1. Salida/Entrada de X negativas y Salida/Entrada de X mayores a 9.
% Estas sirven para salir o entrar de los triángulos del tablero que 
% están en posición diagonal (Amarillo-Azul y Morado-Negro)
%
% Adjuntos horizontales
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla2( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla( X2 , Y2 ))   ), % Revisa que las casillas existan
    Y1 == Y2, %Están en la misma fila
    G is abs( X1 - X2 ),
    G  ==  1, !. %Distancia entre ellas es de 1

%Adjuntos Verticales
adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla2( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla( X2 , Y2 ))   ), % Revisa que las casillas existan
    X1 == X2, %Comparten misma X
    G is abs( Y1 - Y2 ),
    G  ==  1, !. %Distancia entre ellas es de 1

adjuntos( X1 , Y1 , X2 , Y2 ):-
    (   (casilla( X1 , Y1 ), casilla2( X2 , Y2 ));
        (casilla2( X1 , Y1 ), casilla( X2 , Y2 ))   ), % Revisa que las casillas existan
    H is abs(X1-X2),
    H == 1, %Varían en una posición en X
    G is abs( Y1 - Y2 ),
    G  ==  1, !. %Distancia entre ellas es de 1



%................. III. CONDICIONES DE LAS CASILLAS ......................
%
% Una casilla puede estar libre u ocupada
%
libre( X , Y ):-
    ((casilla( X , Y ),!);casilla2(X,Y)), %Existe la casilla
    not(ocupada(X,Y)). %No está ocupada

ocupada( X , Y ):-
    ((casilla( X , Y ),!);casilla2(X,Y)), %existe la casilla
    (   jugadorV( X, Y );
        jugadorN( X, Y );
        jugadorZ( X, Y );
        jugadorB( X, Y );
        jugadorA( X, Y );
    computadoraR( X, Y ) 
    ). %Alguno de los jugadores está ocupándola


%................. IV. MOVIMIENTOS DE LAS FICHAS ..............................
%
% A continuacion mostrados todos los posibles movimientos/saltos
% factibles de las distintas piezas.
%
% -- A) SALTOS UNITARIOS
%
% Saltos sobre la MISMA FILA.
%
% Hacia la Izquierda
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ), %Revisa que origen != destino
    libre( X2 , Y2 ), %Revisa que el destino esté libre
    Diff is (X1 - X2),
    Diff == 2, %La distancia entre origen y destino es de 2 casillas
    Xi is X1 - 1, %X2 está a la izquierda de X1
    Y1 == Y2,
    ocupada( Xi , Y1 ), %La casilla entre origen y destino está ocupada
    !.

% Hacia la Derecha
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ), %Revisa que origen != destino
    libre( X2 , Y2 ),
    Diff is (X2 - X1) , 
    Diff == 2, %La distancia entre origen y destino es de 2 casillas
    Xi is X1 + 1, %X2 está a la derecha de X1
    Y1 == Y2,
    ocupada( Xi , Y1 ), %La casilla entre origen y destino está ocupada
    !.

% Saltos hacia arriba o abajo (DISTINTA FILA)
% 
% Mitad inferior del tablero (Y < 9).
%
% Subiendo (Y2 está arriba de Y1)
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ), %Revisa que origen != destino
    libre( X2 , Y2 ), %El destino está libre
    Y1 < 9 , Y2 =< 9, %El salto se da en la mitad inferior del tablero
    DiffX is X2 - X1,
    DiffY is Y2 - Y1,
    ( DiffX == 2 ; DiffX == 0 ), %X puede ser igual o cambiar por 2
    DiffY ==2, %La distanica en Y debe ser de 2
    Xi is X1 + ( DiffX / 2),
    Yi is Y1 + 1,
    ocupada( Xi , Yi ), %La casilla entre origen y destino esta ocupada
    !.

% Bajando (Y2 está abajo de Y1)
% Análogo a subiendo, pero ahora la distancia entre origen y destino es negativa
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

% Mitad superior dle tablero (Y >= 9).
%
% Subiendo (Y2 está arriba de Y1)
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ), %Revisa que origen != destino
    libre( X2 , Y2 ), %La casilla destino está libre
    Y1 >= 9,
    Y2 > 9, %El salto se da en la mitad superior del tablero
    DiffX is X1 - X2,
    DiffY is Y2 - Y1,
    ( DiffX == 2 ; DiffX == 0 ), %X puede ser igual o cambiar por 2
    DiffY ==2, %La distancia en Y debe ser de 2 casillas
    Xi is X1 - ( DiffX / 2),
    Yi is Y1 + 1,
    ocupada( Xi , Yi ), %La casilla entre origen y destino dbe estar ocupada
    !.

% Bajando (Y2 está abajo de Y1)
% Análogo a subiendo, pero ahora la distancia entre origen y destino es negativa
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

% Caso especial: El salto CRUZA la mitad del tablero 
%
% Subiendo (origen en mitad inferior y destino en mitad superior)
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 8, %Para cruzar la mitad (Y=9) el origen debe estar en la fila 8
    Y2 == 10 , % y el destino en la fila 10
    DiffX is X2 - X1 , %X2 está a la derecha de X1
    DiffX == 1,
    Yi is Y1 + 1 ,
    ocupada( X2 , Yi ),
    !.

% Subiendo_2
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 8 , %Para cruzar la mitad (Y=9) el origen debe estar en la fila 8
    Y2 == 10 , % y el destino en la fila 10
    DiffX is X1 - X2 , %X2 está a la izquierda de X1
    DiffX == 1,
    Yi is Y1 + 1 ,
    ocupada( X1 , Yi ),
    !.

% Bajando (origen en mitad superior y destino en mitad inferior)
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 10 , %Para cruzar la mitad (Y=9) el origen debe estar en la fila 10
    Y2 == 8 ,% y el destino en la fila 8
    DiffX is X2 - X1 , %X2 está a la derecha de X1
    DiffX == 1,
    Yi is Y1 - 1 ,
    ocupada( X2 , Yi ), %Casilla intermedia ocupada
    !.

% Bajando2
% Analogo al anterior, pero ahora X2 esta a la izquierda de x1
saltoFact( X1 , Y1 , X2 , Y2 ):-
    diferentes( X1 , Y1 , X2 , Y2 ),
    libre( X2 , Y2 ),
    Y1 == 10 ,
    Y2 == 8 ,
    DiffX is X1 - X2 , %X2 está a la izquierda de X1
    DiffX == 1,
    Yi is Y1 - 1 , 
    ocupada( X1 , Yi ), %Casilla intermedia ocupada
    !.

% -- B) SALTOS MÚLTIPLES (Cadenas de n saltos)
%
% Primero se realiza un salto de n-1, y finalmente se realiza un 
% salto unitario más, para obtener los n saltos.
%
% i) SALTOS DOBLES:
% Primero un salto unitario y luego otro unitario.
%
% Hay 2 casos, primer salto horizontal y primer salto en vertical.
%
% Caso 1: Primer salto horizontal.
salto2Fact(X1,Y1,X2,Y2):- 
    (   (X4 is X1 + 2, saltoFact(X1,Y1,X4,Y1),saltoFact(X4,Y1,X2,Y2));
        (X3 is X1 - 2, saltoFact(X1,Y1,X3,Y1),saltoFact(X3,Y1,X2,Y2))
    ),!. %El primer salto se hace a la derecha (X1+2), o a la izquierda (X1-2).
            % Luego se realiza un último salto desde el destino del primero.

% Caso 2: Primer salto vertical. (Subiendo o bajando)
%
% BAJANDO (Destino está más abajo que origen)
%
% Si el origen es más alto de la fila 10, el primer salto
% ocurre en la mitad superior del tablero...
salto2Fact(X1,Y1,X2,Y2):-
    (
        Y1 > 10,
        Y3 is Y1 - 2, %Primer salto 2 filas abajo de origen
        (
            (saltoFact(X1,Y1,X1,Y3), saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 + 2, saltoFact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
        ) %La X del primer salto puede ser la misma o aumentar en 2
    ), !.

% Si el origen está en la fila 10, el primer salto cruza la mitad del tablero (Y=9).
salto2Fact(X1,Y1,X2,Y2):-
    (
        Y1 =:= 10,
        Y3 is Y1 - 2,
        (
            (X3 is X1 - 1, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2));
            (X4 is X1 + 1, saltoFact(X1,Y1,X4,Y3),saltoFact(X4,Y3,X2,Y2))
         )% La X del primer salto puede aumentar o disminuir en 1
    ), !.

% Si el origen es más bajo de la fila 9, el primer salto
% ocurre en la mitad inferior del tablero...
salto2Fact(X1,Y1,X2,Y2):-
    (
        Y1=<9,
        Y3 is Y1 - 2,
        (
            (saltoFact(X1,Y1,X1,Y3),saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 - 2, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2))
        ) %La X del primer salto puede ser la misma o aumentar en 2
    ), !.


% SUBIENDO (Destino está más arriba que origen)
%
% Si el origen es más alto de la fila 9, el primer salto
% ocurre en la mitad superior del tablero...
salto2Fact(X1,Y1,X2,Y2):-
    (
        Y1 >= 9,
        Y3 is Y1 + 2, %Primer salto 2 filas arriba de origen
        (
            (saltoFact(X1,Y1,X1,Y3), saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 - 2, saltoFact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
        )% La X del primer salto puede ser la misma o disminuir en 2
    ), !.

% Si el origen está en la fila 8, el primer salto cruza la mitad del tablero (Y=9).
salto2Fact(X1,Y1,X2,Y2):-
    (
        Y1 =:= 8,
        Y3 is Y1 + 2, 
        (
            (X3 is X1 - 1, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2));
            (X4 is X1 + 1, saltoFact(X1,Y1,X4,Y3),saltoFact(X4,Y3,X2,Y2))
         ) % La X del primer salto puede aumentar o disminuir en 1
    ), !.

% Si el origen es más bajo de la fila 9, el primer salto
% ocurre en la mitad inferior del tablero...
salto2Fact(X1,Y1,X2,Y2):-
    (
        Y1<8,
        Y3 is Y1 + 2,
        (
            (saltoFact(X1,Y1,X1,Y3),saltoFact(X1,Y3,X2,Y2));
            (X3 is X1 + 2, saltoFact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2))
        )% La X del primer salto puede ser la misma o aumentar en 2
    ), !.


% ii) SALTOS TRIPLES:
% Primero un salto doble y luego otro unitario.

% Hay 2 casos: el último salto unitario es horizontal o vertical
%
% Caso 1: Último salto Horizontal
salto3Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto2Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto2Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!. % La X del origen del ultimo salto puede variar en 2 con el destino

% Caso 2: Último salto Vertical
%
% BAJANDO
%
% Si el origen del salto doble NO es la fila 14, el útlimo salto no cruza
% la mitad del tablero (Y=9).
salto3Fact(X1,Y1,X2,Y2):-
    not(Y1=:=14),
    Y3 is Y2+2, %Destino del salto doble esta arriba del destino final
    (   (salto2Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2)); 
        (X3 is X2-2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.% La X final puede ser la misma o variar por 2 con respecto a
        % la x del último salto

% Si el origen del salto doble es la fila 14, el último salto
% cruza la mitad del tablero (Y=9).
salto3Fact(X1,Y1,X2,Y2):-
    Y1=:=14, %Origen en la fila 14
    Y3 is Y2+2, %Destino del salto doble esta arriba del destino final
    (   (X3 is X2+1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))     
    ),!.% La X final puede variar por 1 con respecto a la x del último salto

% SUBIENDO
%
% Todo es análogo a BAJANDO, pero ahora el destino está más arriba que el origen.
% 
% Si el origen del salto doble NO es la fila 4, el útlimo salto no cruza
% la mitad del tablero (Y=9).
salto3Fact(X1,Y1,X2,Y2):-
    not(Y1=:=4),
    Y3 is Y2-2, 
    (   (salto2Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2)); 
        (X3 is X2-2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.

% Si el origen del salto doble es la fila 14, el último salto
% cruza la mitad del tablero (Y=9).
salto3Fact(X1,Y1,X2,Y2):-
    Y1=:=4,
    Y3 is Y2-2, 
    (   (X3 is X2+1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto2Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2)) 
    ),!.

% iii) SALTOS CUÁDRUPLES
% Primero un salto triple y luego otro unitario.

% Hay 2 casos: el último salto unitario es horizontal o vertical
% 
% Caso 1: Último salto Horizontal
salto4Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto3Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto3Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!.% La Y del salto final permanece igual, la x puede variar en 2
        % dependiendo se se salta a la derecha o a la izquierda.  

% Caso 2: Último salto vertical
%
% BAJANDO
% 
% Si el origen del salto doble NO es la fila 16, el útlimo salto no cruza
% la mitad del tablero (Y=9).
salto4Fact(X1,Y1,X2,Y2):-
    not(Y1=:=16),
    Y3 is Y2+2, %Origen está más arriba que destino
    (   (salto3Fact(X1,Y1,X2,Y3),saltoFact(X2,Y3,X2,Y2));
        (X3 is X2-2, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!. % La X final puede ser la misma o variar por 2 con respecto a
         % la x del último salto

% Si el origen del salto doble es la fila 16, el último salto
% cruza la mitad del tablero (Y=9).
salto4Fact(X1,Y1,X2,Y2):-
    Y1=:=16,
    Y3 is Y2+2,
    (   (X3 is X2+1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2)) 
    ),!. % La X final puede variar por 1 con respecto a la x del último salto


% SUBIENDO
% Todo es análogo a BAJANDO, pero ahora el destino está más arriba que el origen.
% 
% Si el origen del salto doble NO es la fila 2, el útlimo salto no cruza
% la mitad del tablero (Y=9).
salto4Fact(X1,Y1,X2,Y2):-
    not(Y1=:=2),
    Y3 is Y2-2,
    (   (salto3Fact(X1,Y1,X2,Y3),saltoFact(X2,Y3,X2,Y2));
        (X3 is X2-2, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2+2, salto3Fact(X1,Y1,X3,Y3),saltoFact(X3,Y3,X2,Y2))
    ),!.

% Si el origen del salto doble es la fila 2, el último salto
% cruza la mitad del tablero (Y=9).
salto4Fact(X1,Y1,X2,Y2):-
    Y1=:=2,
    Y3 is Y2-2,
    (   (X3 is X2+1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2));
        (X3 is X2-1, salto3Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2)) 
    ),!.

% iv) SALTOS QUÍNTUPLES
% Primero un salto cuadruple y luego otro unitario.
%
% Hay 2 casos: el último salto unitario es horizontal o vertical
% 
% Caso 1: Último salto Horizontal
salto5Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto4Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto4Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!. % La Y del salto final permanece igual, la x puede variar en 2
         % dependiendo se se salta a la derecha o a la izquierda.  

% Caso 2: Último salto vertical
%
% BAJANDO
% 
% No importa donde sea el origen, el último salto no cruzará la fila 9
salto5Fact(X1,Y1,X2,Y2):-
    Y3 is Y2+2, % Origen está mas arriba que destino
    (   (salto4Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2));
        (X3 is X2+2, salto4Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!. % la X del destino puede ser la misma o disminuir en 2
         % con respecto al último salto

% SUBIENDO
% 
% No importa donde sea el origen, el último salto no cruzará la fila 9
salto5Fact(X1,Y1,X2,Y2):-
    Y3 is Y2-2, % Origen está más abajo que destino
    (   (salto4Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2));
        (X3 is X2+2, salto4Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.% la X del destino puede ser la misma o crecer en 2
        % con respecto al último salto

% v) SALTOS SEXTUPLES
% Primero un salto quintuple y luego otro unitario.
%
% Hay 2 casos: el último salto unitario es horizontal o vertical
% 
% Caso 1: Último salto Horizontal
salto6Fact(X1,Y1,X2,Y2):-
    (   (X3 is X2-2, salto5Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2));
        (X3 is X2+2, salto5Fact(X1,Y1,X3,Y2), saltoFact(X3,Y2,X2,Y2))
    ),!. % La Y del salto final permanece igual, la x puede variar en 2
         % dependiendo se se salta a la derecha o a la izquierda.  

% Caso 2: Último salto vertical
%
% BAJANDO
% 
% No importa donde sea el origen, el último salto no cruzará la fila 9
salto6Fact(X1,Y1,X2,Y2):-
    Y3 is Y2+2, % Origen está mas arriba que destino
    (   (salto5Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2));
        (X3 is X2+2, salto5Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!. % la X del destino puede ser la misma o disminuir en 2
         % con respecto al último salto

% SUBIENDO
% 
% No importa donde sea el origen, el último salto no cruzará la fila 9
salto6Fact(X1,Y1,X2,Y2):-
    Y3 is Y2-2, % Origen está más abajo que destino
    (   (salto5Fact(X1,Y1,X2,Y3), saltoFact(X2,Y3,X2,Y2));
        (X3 is X2+2, salto5Fact(X1,Y1,X3,Y3), saltoFact(X3,Y3,X2,Y2))
    ),!.% la X del destino puede ser la misma o crecer en 2
        % con respecto al último salto

% -- C) PASOS A CASILLAS (Movimientos sin salto)
%
pasoFact( X1 , Y1 , X2 , Y2 ):-
    adjuntos( X1 , Y1 , X2 , Y2 ), %El origen y el destino deben estar adjuntos
    libre( X2 , Y2 ) . %El destino debe estar libre


%................. V) MOVIMIENTOS VÁLIDOS ...........................
%
% Un movimiento es valido si existe un paso factible o un salto factible
% a el.
%
% Primero prueba si se puede saltar desde el origen hasta el destino.
% Si no puede saltar, prueba si se puede mover sin salto.
movVal( X1 , Y1 , X2 , Y2 ):-
    (   movVal2(X1 , Y1 , X2 , Y2 );
        pasoFact( X1 , Y1 , X2 , Y2 )
    ),!. %Con que uno se pueda, el movimiento es válido

% Revisa si se puede realizar alguno de los distintos tipos de salto:
movVal2( X1 , Y1 , X2 , Y2 ):-
    (   salto6Fact(X1, Y1, X2, Y2);
        salto5Fact(X1, Y1, X2, Y2);
        salto4Fact(X1, Y1, X2, Y2);
        salto3Fact(X1, Y1, X2, Y2);
        salto2Fact(X1, Y1, X2, Y2);
        saltoFact(X1, Y1, X2, Y2)
    ),!. %Con que uno se pueda, el movimiento es válido



% ................. VI) FIN DEL JUEGO .............................
%
% El juego termina cuando todas las fichas de un jugador han llegado 
% al triángulo opuesto al que empezaron.
%
% Gana el verde si sus fichas llegan a donde comienzan las rojas
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
    writeln("************************"),
    writeln("¡Gana el jugador VERDE!"),
    writeln("************************"),
    drawTablero,
    writeln("Para una nueva partida, llamar a newGame. ").

% Gana el rojo si sus fichas llegan a donde comienzan las verdes
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
    writeln("************************"),
    writeln("¡Gana el jugador ROJO!"),
    writeln("************************"),
    drawTablero,
    writeln("Para una nueva partida, llamar a newGame. ").

% Gana el amarillo si sus fichas llegan a donde comienzan las azules
ganoJugadorA:-
    jugadorA( 6 , 5 ),
    jugadorA( 7 , 5 ),
    jugadorA( 8 , 5 ),
    jugadorA( 9 , 5 ),
    jugadorA( 7 , 6 ),
    jugadorA( 8 , 6 ),
    jugadorA( 9 , 6 ),
    jugadorA( 8 , 7 ),
    jugadorA( 9 , 7 ),
    jugadorA( 9 , 8 ),
    writeln("************************"),
    writeln("¡Gana el jugador AMARILLO!"),
    writeln("************************"),
    drawTablero,
    writeln("Para una nueva partida, llamar a newGame. ").

% Gana el azul si sus fichas llegan a donde comienzan las amarillas
ganoJugadorZ:-
    jugadorZ( -3 , 13 ),
    jugadorZ( -2 , 13 ),
    jugadorZ( -1 , 13 ),
    jugadorZ( 0 , 13 ),
    jugadorZ( -2 , 12 ),
    jugadorZ( -1 , 12 ),
    jugadorZ( 0 , 12 ),
    jugadorZ( -1 , 11 ),
    jugadorZ( 0 , 11 ),
    jugadorZ( 0 , 10 ),
    writeln("************************"),
    writeln("¡Gana el jugador AZUL!"),
    writeln("************************"),
    drawTablero,
    writeln("Para una nueva partida, llamar a newGame. ").

% Gana el morado si sus fichas llegan a donde comienzan las negras
ganoJugadorB:-
    jugadorB( 6 , 13 ),
    jugadorB( 7 , 13 ),
    jugadorB( 8 , 13 ),
    jugadorB( 9 , 13 ),
    jugadorB( 7 , 12 ),
    jugadorB( 8 , 12 ),
    jugadorB( 9 , 12 ),
    jugadorB( 8 , 11 ),
    jugadorB( 9 , 11 ),
    jugadorB( 9 , 10 ),
    writeln("************************"),
    writeln("¡Gana el jugador MORADO!"),
    writeln("************************"),
    drawTablero,
    writeln("Para una nueva partida, llamar a newGame. ").

% Gana el negro si sus fichas llegan a donde comienzan las moradas
ganoJugadorN:-
    jugadorN( 0 , 8),
    jugadorN( 0 , 7 ),
    jugadorN( -1 , 7 ),
    jugadorN( 0 , 6 ),
    jugadorN( -1 , 6 ),
    jugadorN( -2 , 6 ),
    jugadorN( 0 , 5 ),
    jugadorN( -1 , 5 ),
    jugadorN( -2 , 5 ),
    jugadorN( -3 , 5 ),
    writeln("************************"),
    writeln("¡Gana el jugador NEGRO!"),
    writeln("************************"),
    drawTablero,
    writeln("Para una nueva partida, llamar a newGame. ").

% Temrina el juego si alguno de los jugadores gana.
finDeljuego:-
    ganoJugadorN;
    ganoJugadorB;
    ganoJugadorZ;
    ganoJugadorA;
    ganoJugadorV;
    ganoComputadoraR.



% ................ VII) EVALUACIONES PARA LA HEURISTICA............
%
% Aquí se realizan los cálculos que involucra nuestra función heurística,
% con la que podemos evaluar que tan favorable es el posicionamiento de un
% jugador para aumentar sus probabilidades de ganar.
% Entre mayor sea el valor de la función, el posicionamiento de las fichas es mejor.
%
% Nuestra función cuenta de 2 partes: distancia al eje del camino, y distancia a la meta.
% Al final la función se calcula como: 
%       F = distMeta - distEje 
%
% A) DISTANCIA AL EJE
%
% Según el jugador, el eje de su camino es distinto. 
% Entonces notamos 3 ejes: Verde-Rojo, Amarillo-Azul y Morado-Negro
% 
%
% i) EJE ROJO-VERDE
% Se obtiene el número de espacios en la fila y se divide entre 2, para obtener el espacio central
% Luego se calcula la distancia de la ficha al espacio central
%
%Mitad superior del tablero
distCentro(X , Y , Dist):-
    Y > 9, 
    Aux is 18 - Y, %Obtiene el total de espacios en la fila Y
    Centro is Aux / 2,
    Dist is abs(Centro - X),!. %Calcula distancia al centro

%Mitad inferior del tablero
distCentro(X , Y , Dist):-
    Y =< 9 , %El total de espacios en la fila Y es igual que Y.
    Centro is Y / 2,
    Dist is abs(Centro - X),!. %Calcula distancia al centro

% ii) AMARILLO-AZUL
% Como el eje está en diagonal al tablero, hay que hacer un cálculo distinto:
% Dada la disposición del tablero, la distancia al eje diagonal se obtiene
% aproximadamente como la mitad de la distancia en X de la ficha al eje. 
%
% Mitad superior del tablero
distCentroAZ(X, Y, Dist):-
    Y>9, 
    Z is 5-((Y-9)*2), %Obtiene la distancia en X de la ficha al eje.
    Dist is abs(Z-X)/2. %Distancia al eje = mitad de la distancia en X

% Mitad inferior del tablero
distCentroAZ(X, Y, Dist):-
    Y=<9,
    Z is (9-Y)+5, %Obtiene la distancia en X de la ficha al eje.
    Dist is abs(Z-X)/2. %Distancia al eje = mitad de la distancia en X

% iii) MORADO-NEGRO
% La idea es la misma que en el caso anterior.
% La distancia al eje es aprox. la mitad de su distancia en X.

% Mitad superior del tablero
distCentroBN(X, Y, Dist):-
    Y>9,
    Z is 5+(Y-9), %Obtiene la distancia en X de la ficha al eje.
    Dist is abs(Z-X)/2. %Distancia al eje = mitad de la distancia en X

% Mitad inferior del tablero
distCentroBN(X, Y, Dist):-
    Y=<9,
    Z is 5-((9-Y)*2), %Obtiene la distancia en X de la ficha al eje.
    Dist is abs(Z-X)/2. %Distancia al eje = mitad de la distancia en X

% B) DISTANCIA A LA META
% 
% Se obtiene como la distancia de la ficha hasta la 
% punta más lejana del triángulo de meta.
%
% i) ROJO-VERDE
% Rojo: Dist = 18-Y, Verde: Dist = Y.
% El cálculo se hace directamente en la evaluación heurística.
%
% ii) AMARILLO-AZUL
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

% iii) MORADO-NEGRO
%
% Caso Y<= 9 (Mitad inferior del tablero)
distMetaBN(X, Y, Dist):-
    Y=<9,
    Dist is 3+X.

% Caso Y>9 (Mitad superior del tablero)
distMetaBN(X, Y, Dist):-
    Y > 9,
    Dist is X+Y-6.


%..................... VIII) FUNCIÓN HEURISTICA .............................
%
% La función heurística se calcula como la distacia a la meta menos la distancia al eje.
% Esto pues nuestra estrategia es avanzar lo más que se pueda, pero buscando que 
% las fichas permanezcan juntas en el centro para que puedan haber más saltos.
%
% Además consideramos más importante acercarse a la meta que permanecer cerca al eje,
% por lo que ponderamos la función de la siguiente forma:
%        Val_Heur = distMeta * 4 - distEje *2.

% Segun el modo de juego se toman las posiciones de las fichas existentes.
% En "Pos" le damos tableroPos (visto en la sección I), 
% que tiene todas las posiciones del tablero en una lista de listas, 
% primero la de la computadora y luego la de los jugadores en el orden de los turnos del juego
%

% Aplicación de la heurística a piezas de la COMPUTADORA (ROJO)
%
% Se aplica la heurística a la pieza en la posición X,Y
heurAPiezaComputadoraR( X , Y , Val ):-
    distCentro( X , Y , Dist ),
    Val is (( 18 - Y ) * 4 - Dist * 3 ).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
heurComputadoraRAvanza( [] , 0 ).
heurComputadoraRAvanza([ T | Q ], Val ):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaComputadoraR( X , Y , Val2 ),
    heurComputadoraRAvanza( Q , Val1 ),
    Val is Val1 + Val2.

% Aplicación de heurística a piezas del JUGADOR VERDE
%
% Se aplica la heurística a la pieza en la posición X,Y
heurAPiezaJugadorV( X , Y , Val ):-
    distCentro( X , Y , Dist ),
    Val is ((Y*4) - (Dist*3)).

% Suma el total de las evaluaciones de cada pieza para evaluar su situación
% => Valor total jugador = Suma(Valor de cada pieza).
heurJugadorVAvanza( [] , 0 ).
heurJugadorVAvanza([ T | Q ], Val ):-
    nth1( 1 , T , X ),
    nth1( 2 , T , Y ),
    heurAPiezaJugadorV( X , Y , Val2 ),
    heurJugadorVAvanza( Q , Val1 ),
    Val is Val1 + Val2.

% Aplicación de heurística a piezas del JUGADOR AMARILLO
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

% Aplicación de heurística a piezas del JUGADOR AZUL
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

% Aplicación de heurística a piezas del JUGADOR MORADO
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

% Aplicación de heurística a piezas del JUGADOR NEGRO
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

% Para la computadora, la función heurística se calcula como el valor de sus fichas,
% menos el valor de las fichas de los contrincantes.
% Según el modo se obtiene el valor de las fichas existentes para hacer el cálculo.
%
% --Modo 2 jugadores
heur(Pos, _ , Val, Modo):-
	Modo == 1 ,
	nth1(1 , Pos , Piezascompu), nth1(2 , Pos , PiezasjugadorV), %obtiene las listas de las fichas
	heurComputadoraRAvanza(Piezascompu , Valcompu), %aplica la funcion heurística a la computadora
	heurJugadorVAvanza(PiezasjugadorV , ValjugadorV), %aplica la funcion heuristica a los jugadores
	Val is Valcompu - ValjugadorV. %Obtiene el valor para la computadora

% --Modo 3 jugadores
% análogo al anterior pero resta el valor de 2 jugadores humanos.
heur(Pos, _ , Val, Modo):-
    Modo == 2 ,
    nth1(1 , Pos , Piezascompu), nth1(2 , Pos , PiezasjugadorB),nth1(3 , Pos , PiezasjugadorZ),
    heurComputadoraRAvanza(Piezascompu , Valcompu),
    heurJugadorBAvanza(PiezasjugadorB , ValjugadorB),
    heurJugadorZAvanza(PiezasjugadorZ , ValjugadorZ),
    Val is Valcompu - (ValjugadorB + ValjugadorZ).

% --Modo 4 jugadores
% análogo al anterior pero resta el valor de 3 jugadores humanos.
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
% análogo al anterior pero resta el valor de 5 jugadores humanos.
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


%................... IX) POS. ACCESIBLES ............................
%
% Para obtener todos los movimientos posibles de una ficha en una posición inicial
% utilizamos el predicado findall/3 que explicamos anteriormente en la sección I.
% Así, para una posición inicial X,Y, se realiza una búsqueda exhaustiva para encontrar 
% todos los posibles destinos.
%
% Primero se calculan todos los pasos sin salto posibles, luego todos los saltos unitarios
% posibles. Luego, para cada salto unitario posible, se calculan todos los posibles 
% encadenamientos de saltos. 
% Al final se unen las listas de los destinos obtenidos en cada caso para obtener el total de 
% destinos posibles a partir de un origen (X, Y).
    
% Devuelve la lista de movimientos posibles para las fichas ROJAS desde la posicion X, Y.
% Realiza cálculos para buscar sólo movimientos hacia abajo, pues no es necesario
% incluir los saltos hacia arriba, ya que no lo acercan a su meta.
espaciosDisponiblesComputadoraR( X , Y , M):-
    numlist( 1 , 9 , DX ),numlist( 1 , Y , DY ), %Revisa solo saltos hacia abajo.
    findall([X2, Y2],
            (miembro( Y2 , DY ), miembro( X2 , DX ),libre(X2,Y2), saltoFact( X , Y , X2 , Y2 )),
            M1), %Obtiene todos los saltos unitarios posibles y guarda los destinos en M1
    findall([Xp, Yp],
            (miembro( Yp , DY ), miembro( Xp , DX ),libre(Xp,Yp), pasoFact( X , Y , Xp , Yp )),
            Mp), %Obtiene todos los pasos sin salto  posibles y guarda los destinos en Mp
    saltosExtra(M1,M2), %Obtiene los saltos posibles a partir de cada destino de M1 y los guarda en M2 (saltos dobles)
    saltosExtra(M2,M3), %Obtiene los saltos posibles a partir de cada destino de M2 y los guarda en M3 (saltos triples)
    saltosExtra(M3,M4), %Obtiene los saltos posibles a partir de cada destino de M3 y los guarda en M4 (saltos cuadruples)
    saltosExtra(M4,M5), %Obtiene los saltos posibles a partir de cada destino de M4 y los guarda en M5 (saltos quintuples)
    saltosExtra(M5,M6), %Obtiene los saltos posibles a partir de cada destino de M5 y los guarda en M6 (saltos sextuples)
    union(M1,M2,R12),
    union(R12,M3,R123),
    union(R123,M4,R1234),
    union(R1234,M5,R12345),
    union(R12345,M6,Maux),
    union(Maux,Mp,Mres), %Realiza la unión de todos los destinos encontrados
    subtract(Mres,[[X,Y]],M). %Se elimina la posición de origen de la lista de destinos posibles.
    % M  es el resultado final de todos los posibles destinos

% Igual que en el caso de las piezas ROJAS, obtiene los destinos posibles para los demás colores
% Todos los demás jugadores pueden subir o bajar (dependiendo el jugador),
% por lo tanto no se restringe la búsqueda como en el caso de las fichas ROJAS.
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
    saltosExtra(M5,M6),
    union(M1,M2,R12),
    union(R12,M3,R123),
    union(R123,M4,R1234),
    union(R1234,M5,R12345),
    union(R12345,M6,Maux),
    union(Maux,Mp,Mres),
    subtract(Mres,[[X,Y]],M).


% Revisa si es posible seguir saltando desde una posicion X,Y dada
% Entonces sirve para ver si se pueden realizar cadenas de saltos.
saltosExtra([],[]).
saltosExtra([L|Q], Res):-
    saltosExtra(Q, Res1), %Para todos los destinos de la lista
    nth1(1,L,X), nth1(2,L,Y), %obtene las coordenadas X y Y.
    Lowx is X-2, Highx is X+2, %El siguiente salto puede estar en un rango de [X-2, X+2]
    Lowy is Y-2, Highy is Y+2, %El siguiente salto puede estar en un rango de [Y-2, Y+2]
    numlist(Lowx,Highx,DX),
    findall([X2, Y2],
            (miembro( Y2 , [Lowy,Y,Highy] ), miembro( X2 , DX ),libre(X2,Y2), saltoFact( X , Y , X2 , Y2 )),
            M), %Busca todos los posibles saltos desde la posicion dada.
    union(M,Res1,Res). %Se unen todos los destinos encontrados


% Este predicado es el que llama a los de arriba. Según el turno se obtienen los movimientos
% posibles de la computadora o de los jugadores.
%
% Turno 1 juega la computadora, turno > 2 juega un jugador
espaciosDisponibles(X , Y , M , Turno , R):-
    R == 1 ,
    ((Turno == 1, espaciosDisponiblesComputadoraR( X , Y , M ),! ); (Turno >= 2, espaciosDisponiblesJugadorV( X , Y , M ) ,! )).


% ................... X) MINIMAX CON PODA ALFA-BETA ........................
%
% Se realiza la búsqueda del movimiento de la computadora con el algoritmo minimax con poda alfa-beta.
% Para hacer esto se va construyendo el árbol de búsqueda mientras se avanza en cada nivel de profundidad.
% Cada nivel le corresponde a un jugador distinto, empezando con el nivel 1 para la computadora y
% luego los demás jugadores en el orden de los turnos del juego.
% Así, cada nodo es una configuración del tablero posible, donde varía la posición de una ficha 
% del color correspondiente al jugador en el nivel que se está.
% En cada nivel habrá tantos nodos como movimientos posibles para cada jugador.
% Los nodos hoja dan la configuración del tablero cuando todos los jugadores ya simularon su movimiento,
% de forma que se le aplica la función heurística al tablero encontrado para saber su valor.
% Según sea el caso se actualiza alfa o beta y se continua con la búsqueda.
%
% Se usan distintos predicados para ir formando el árbol de búsqueda.
% Básicamente se obtienen listas de posiciones de las fichas de cada jugador y se
% generan otras listas que incluyen las posiciones de los demás jugadores para así
% ir generando nodos (listas de tableros completos)...


% A) Generación de NODOS:
%
% El siguiente predicado unta todas las opciones posibles del jugador en turno con las de
% los demás jugadores otro jugador
% Entonces quedan todas las configuraciones posibles del tablero completo.
% La primera lista tiene a todos los tableros que tienen a los demás jugadores, pero no al actual.
% Elem tiene las posiciones del jugador actual. 
% Entonces se junta Elem a cada uno de los tableros para completarlos y generar los nodos.
% 
% Este añade al jugador al final de la lista de los tableros
distribucionJugador( [] , _ , [] ).
distribucionJugador([ T | Q ], Elem , Lista ):-
    distribucionJugador( Q , Elem , L2 ),
    append([ T ], [ Elem ], L1 ),
    append([ L1 ], L2 , Lista ). 

% Este añade al jugador (computadora) al inicio la lista de los tableros.
distribucionComputadoraR( [] , _ , [] ) .
distribucionComputadoraR([ T | Q ], Elem , Lista ):-
    distribucionComputadoraR( Q , Elem , L2 ),
    append([ Elem ], [ T ], L1 ),
    append([ L1 ], L2 , Lista ).


% B) Obtiene MOVIMIENTOS POSIBLES para un jugador:
%
% La primera lista tiene todas las posiciones de las fichas de un jugador, menos la que se quiere mover.
% La segunda lista tiene todos los posibles destinos de la ficha que se quiere mover.
% Entonces succL junta los posibles destinos de una ficha con todas las otras fichas,
% para obtener todas las configuraciones posibles del jugador en turno.
succL([] , _ , []).
succL([ Elem | QL ], G , Res):-
	succL(QL , G , Res1),
	append([ Elem ], G , Res2),
	append(Res1 , [ Res2 ], Res).

% La primera lista contiene todas las posiciones iniciales de las fichas del jugador en turno.
% A cada una de las fichas le aplica succL para obtener todas las posibles configuraciones
% de las fichas del jugador en turno, según todos los movimientos posibles.
succCalc([] , _ , [] , _ , _ ).
succCalc([ T | Q ], Pos , PosList , Turno , AI ):-
	nth1( 1 , T , Xi ),
    nth1( 2 , T , Yi ), %Obtiene las coordenadas iniciales Xi, Yi de cada ficha.
	espaciosDisponibles( Xi , Yi , L , Turno , AI ), %Obtiene posibles movimientos desde Xi, Yi en L
	subtract( Pos , [[ Xi , Yi ]], G ), %Quita de la lista de fichas la ficha Xi,Yi, pues es la que se movió
	succL( L , G , PosList1 ), %Regresa una lista de listas, donde cada elemento contiene las posiciones de las 10 piezas, con cada mov posible de L
	succCalc( Q , Pos , PosList2 , Turno , AI ), %Realiza el cálculo para todas las fichas.
	append( PosList1 , PosList2 , PosList ). % Regresa una lista de listas con todas las configuraciones posibles del jugador en turno


% SUCC se apoya de las dos funciones anteriores.
% Entonces obtiene todas las configuraciones posibles del jugador en turno y, 
% luego llama a distribucionXXXX() para meter cada configuración a un nodo distinto.
%
% Obtiene TODOS los movimientos posibles de la COMPUTADORA y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
	Modo == 1 ,
    Turno == 1 ,
	succCalc( Compu , Compu , Temp , Turno , Modo ), %Temp tiene todos las configuraciones posibles de las fichas del jugador en turno. (Lista de listas)
    nth1(1,Players,JugV),
    distribucionJugador(Temp , JugV , PosList ),!. % Poslist tiene todas las configuraciones posibles del tablero.
    %Poslist = [ [Compu1, JugadorV],[Compu2, JugadorV], ...] donde Compu_i y Jugador son listas de parejas [X,Y] de las 10 fichas

% -- Modo de juego 2 jugadores
% Obtiene TODOS los movimientos posibles del JUGADOR_V y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
	Modo == 1 ,
    Turno == 2 ,
    nth1(1,Players,Jug),
	succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
	distribucionComputadoraR( Temp , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero


% -- Modo de juego 6 jugadores
% Obtiene TODOS los movimientos posibles del JUGADOR_A y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 2 ,
    nth1(1,Players,Jug),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux),
    append(Temp, Aux, Players2),
    distribucionComputadoraR( Players2 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_B y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 3 ,
    nth1(2,Players,Jug), nth1(1,Players,Jug1),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux), subtract(Aux, Jug1, Aux2),
    append(Jug1, Temp, Players2), append(Players2, Aux2, Players3),
    distribucionComputadoraR( Players3 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_V y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 4 ,
    nth1(3,Players,Jug),nth1(1,Players,Jug1),nth1(2,Players,Jug2),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux), subtract(Aux, Jug1, Aux2), subtract(Aux2, Jug2, Aux3),
    append(Jug1, Jug2, Players2), append(Players2, Temp, Players3), append(Players3, Aux3, Players4),
    distribucionComputadoraR( Players4 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_Z y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 5 ,
    nth1(4,Players,Jug),nth1(5,Players,Jug5),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux), subtract(Aux, Jug5, Aux2),
    append(Aux2,Temp,Players2), append(Players2,Jug5,Players3),
    distribucionComputadoraR( Players3 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero

% Obtiene TODOS los movimientos posibles del JUGADOR_N y los mete al nodo.
succ([Compu|Players], Turno , PosList , Modo ):-
    Modo == 4 ,
    Turno == 6 ,
    nth1(5,Players,Jug),
    succCalc( Jug , Jug , Temp , Turno , Modo ), %Calcula los posibles escenarios del jugador
    subtract(Players, Jug, Aux),
    append(Aux,Temp,Players2),
    distribucionComputadoraR( Players2 , Compu , PosList ),!. %Obtiene todos los posibles escenarios del tablero


% C) Recorre el ARBOL DE BÚSQUEDA

% Minimax alpha beta: llamada inicial
minimaxab( Posicion , Turno , Profundidad , MejorMov , Valor , Modo ):-
	succ( Posicion , Turno , X , Modo ),% Simulación del movimiento, X = lista de posiciones después de jugar
	siguienteJugador( Turno , OtroJugador ), % Cambio de jugador
	decr( Profundidad , P2 ), % Profundidad disminuye en 1
	alphabeta( X , OtroJugador , P2 , -999 , 999 , MejorMov , Valor , Modo ).  % Se inicia la búsqueda y se inicializan alfa= -999 y beta=999


% alphabeta(ListaTablero, Turno, Profundidad, alpha, beta, MejorMov, Valor, ModoJuego)
% Dependiendo del turno, maximiza o minimiza los valores.
%
% CASO BASE: Si ya no hay mas jugadores (No hay lista de sus fichas)
alphabeta( [] , _ , _ , _ , _ , _ , _ , _ ).

% CASO LIMITE DE PROFUNDIDAD, MINIMIZA (BUSCA EL MINIMO)
% Llega a un nodo hoja
alphabeta([ E | L ], 1 , P , A , B , MejorMov , Valor , Modo ):-
        testDepth( P , E , 1 ), % hoja final alcanzada o final del juego
        heur( E , 1 , ValE , Modo ), % Evaluación del "tablero E" en ValE
        (   ( A =< ValE , %Si A<=ValE => busca en el siguiente nodo hoja
                alphabeta( L , 1 , P , A , B , MejorMovL , ValL , Modo ), %Busca en el siguiente nodo del mismo nivel
                recordMin( ValE , ValL , E , MejorMovL , MejorMov , Valor )); %Almacena el valor mínimo
                %Else: PODA
            copia( E , ValE , MejorMov , Valor )).  % Memorización de los mejores, o es el y anterior o copiamos lo que teniamos

% CASO LIMITE DE PROFUNDIDAD, MAXIMZA (BUSCA EL MAXIMO)
% Analogo al anterior pero maximizando
alphabeta([ E | L ], 2 , P , A , B , MejorMov , V , Modo ):-
        testDepth( P , E , 2 ),
        heur( E , 2 , ValE , Modo ),
        (   ( B >= ValE ,
                alphabeta( L , 2 , P , A , B , MejorMovL , ValL , Modo ),
                recordMax( ValE , ValL , E , MejorMovL , MejorMov , V ));
                copia( E , ValE , MejorMov , V )).

% CASO NODO INTERNO.
% Realiza la búsqueda MINIMIZANDO
alphabeta([ E | L ], 1 , P , A , B , MejorMov , V , Modo ):-
        succ( E , 1 , X , Modo ), % Simulación del movimiento, X = lista de posiciones después de jugar
        not(vacio( X )),
        P \= 0 , %Aun no ha llegado al nodo hoja
        decr( P , P2 ), %Profundidad disminuye en 1
        alphabeta( X , 2 , P2 , A , B , MejorMovX , ValX , Modo ), %continua con la busqueda en profundidad
        (   ( ValX >= A , %Si valX > alpha => busca en el siguiente nodo del mismo nivel
                min( ValX , B , Bbis ), %Actualiza beta
                alphabeta( L , 1 , P , A , Bbis , MejorMovL , ValL , Modo ), %continua busqueda
                recordMin( ValX , ValL , E , MejorMovL , MejorMov , V )); %Guarda el minimo
            copia( MejorMovX , ValX , MejorMov , V )). %Si valX < alpha , PODA.

% Análogo al anterior pero MAXIMIZANDO.
alphabeta([ E | L ], 2 , P , A , B , MejorMov , V , Modo ):-
        succ( E , 2 , X , Modo ), % Simulación del movimiento, X = lista de posiciones después de jugar
        not(vacio( X )),
        P \= 0 ,
        decr( P , P2 ),
        alphabeta( X , 1 , P2 , A , B , MejorMovX , ValX , Modo ), %continua con la busqueda en profundidad
        (   ( ValX =< B , %Si valX > alpha => busca en el siguiente nodo del mismo nivel
                max( ValX , A , Abis ), %Actualiza alfa
                alphabeta( L , 2 , P , Abis , B , MejorMovL , ValL , Modo ),
                recordMax( ValX , ValL , E , MejorMovL , MejorMov , V ));
            copia( MejorMovX , ValX , MejorMov , V )). %Si ValX>beta => PODA


% D) HERRAMIENTAS ADICIONALES para minimax
%
% Cambia de turno.
% MAX = 1, MIN = 2
siguienteJugador( 1 , 2 ).
siguienteJugador( 2 , 1 ).

% Copia los primeros dos valores en los ultimos dos:
% copia(Input1, Input2, Output1, Output2)
copia(Posicion , Valor , Posicion , Valor).

% El siguiente metodo guarda el mejor movimiento posible de dos
% que compara y sirve para el metodo minimax
%
% Guarda la configuración con la mayor heurística
recordMax( X , X , MejorMovX , _ , MejorMovX , X ).
recordMax( X , Y , MejorMovX , _ , MejorMovX , X ):-
    X >= Y.
recordMax( X , Y , _ , MejorMovY , MejorMovY , Y ):-
    X =< Y.

% Guarda la configuración con la menor heurística
recordMin( X , X , MejorMovX , _ , MejorMovX , X ).
recordMin( X , Y , MejorMovX , _ , MejorMovX , X ):-
    X =< Y.
recordMin( X , Y , _ , MejorMovY , MejorMovY , Y ):-
    X >= Y.

%Decremento de profundidad.
decr( P , P2 ):-
    P2 is P - 1.

% Revisa si una lista está vacía.
vacio([]).

% Si ya llegó a la profundidad máxima.
testDepth( P , _ , _ ):-
    P  ==  0.

% Regresa máximo entre A y B
max( A , A , A ).
max( A , B , A ):-
    A > B .
max( A , B , B ):-
    A < B.

% Regresa minimo entre A y B
min( A , A , A ).
min( A , B , A ):-
    A < B.
min( A , B , B ):-
    A > B.

% ......................... XI) DIBUJAR TABLERO ..............................
%
% Cada ficha tiene una letra, según su color (R, A, B, V, Z y N).
% Los espacios vacíos se representan con un punto (.)
% Para dibujar el tablero, decidimos hacerlo por filas. Entonces, se tiene un predicado 
% para cada una de las 17 filas.

% Draw: dibuja el símbolo correspondiente, dependiendo de si se trata de una ficha 
% o un espacio vacío.
% *** NOTA: investigando encontramos que el predicado ansi_format() puede escribir
% *** en la terminal APLICANDO UN COLOR al símbolo que se escribe.
%
% Dibuja las fichas ROJAS 
draw( A , B ):-
    computadoraR( A , B ),
    ansi_format([bold,fg(red)], 'R~w', [""]),
    !.
% Dibuja las fichas AMARILLAS 
draw( A , B ):-
    jugadorA( A , B ),
    ansi_format([bold,fg(yellow)], 'A~w', [""]),
    !.
% Dibuja las fichas MORADAS 
draw( A , B ):-
    jugadorB( A , B ),
    ansi_format([bold,fg(magenta)], 'B~w', [""]),
    !.
% Dibuja las fichas VERDES 
draw( A , B ):-
    jugadorV( A , B ),
    ansi_format([bold,fg(green)], 'V~w', [""]),
    !.
% Dibuja las fichas AZULES 
draw( A , B ):-
    jugadorZ( A , B ),
    ansi_format([bold,fg(blue)], 'Z~w', [""]),
    !.
% Dibuja las fichas NEGRAS 
draw( A , B ):-
    jugadorN( A , B ),
    ansi_format([bold,fg(black)], 'N~w', [""]),
    !.
% Dibuja las CASILLAS
draw( A , B ):-
    casilla( A , B ),
    write(.),
    !.
draw( A , B ):-
    casilla2( A , B ),
    write(.),
    !.
% En el caso de algún error...
draw( _ , _ ):-
    write("NaN").

% Como se menciona más arriba, se dibuja el tablero por filas.
% Se empieza desde la fila más alta (Y=17) y se sigue a la más baja (Y=1).
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

% Predicados que escriben los símbolos en el lugar correspondiente de cada fila.
% Se utilizan espacios para darle forma al tablero y obtener la estrella de 6 puntas.
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



% ......................... XII) MOVIMIENTO DE FICHAS  ..............................

% A) Llama al juego de la computadora
%
jugarComputadoraR( Modo ):-
    cls, % Limpia la pantalla
    tableroPos( Pos, Modo ), %Obtiene las posiciones de todas las fichas del tablero.
    minimaxab( Pos , 1 , Modo , MejorMov , _ , 1 ), %Aplica la búsqueda minimax y encuentra la configuración óptima del tablero.
    !,
    retractall(computadoraR( _ , _ )), % Elimina todas las fichas de la computadora
    nth1( 1 , MejorMov , Mejor ), % Toma las fichas de la computadora del tablero óptimo encontrado
    forall( %Para cada ficha en la lista de las posiciones de las fichas de la computadora:
        miembro( Elem , Mejor ),
        (
            nth1( 1 , Elem , X1 ), nth1( 2 , Elem , Y1 ), assert(computadoraR( X1 , Y1 ))
         ) % Se obtienen las coordenadas X y Y, 

        ),
    drawTablero, %Dibuja el tablero con el nuevo movimiento
    not(finDeljuego), %Pregunta si alguien ya ganó.
    modoJuego(M), %Segun el modo de juego, indica el siguiente turno.
    (   (M==1, writeln("Turno del VERDE"));
        (M==2, writeln("Turno del BLANCO"));
        (M==3, writeln("Turno del AMARILLO"));
        (M==4, writeln("Turno del AMARILLO"))
        ),!.

% B) Mover las fichas de los jugadores
%
% Mover Computadora ROJA
moverComputadoraR( X1 , Y1 , X2 , Y2 ):-
    cls, %Limpia la pantalla
    computadoraR( X1 , Y1 ), %Revisa si existe la ficha Roja en la posicion X1, Y1.
    movVal( X1 , Y1 , X2 , Y2 ), %Revisa si el movimiento es válido.
    retract(computadoraR( X1 , Y1 )),
    assert(computadoraR( X2 , Y2 )), %Realiza el movimiento
    not(finDeljuego), %Pregunta si alguien ya ganó.
    modoJuego(M),
    drawTablero, %Segun el modo de juego indica el siguiente turno.
    (   (M==1, writeln("Turno del VERDE"));
        (M==2, writeln("Turno del BLANCO"));
        (M==3, writeln("Turno del AMARILLO"));
        (M==4, writeln("Turno del AMARILLO"))
        ),
    !.

 % Mover Jugador VERDE 
 % Igual que el caso anterior pero con las fichas VERDES  
moverJugadorV( X1 , Y1 , X2 , Y2 ):-
    cls,
    jugadorV( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorV( X1 , Y1 )),
    assert(jugadorV( X2 , Y2 )),
    not(finDeljuego),
    modoJuego(M),
    drawTablero,
    (   (M==1, jugarComputadoraR(1));
        (M==3, writeln("Turno del AZUL"));
        (M==4, writeln("Turno del AZUL"))
        ),
    !.

% -- Mover Jugador AMARILLO
% Igual que el caso anterior pero con las fichas AMARILLAS  
moverJugadorA( X1 , Y1 , X2 , Y2 ):-
    cls,
    jugadorA( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorA( X1 , Y1 )),
    assert(jugadorA( X2 , Y2 )),
    not(finDeljuego),
    modoJuego(M),
    drawTablero,
    (   (M==3, writeln("Turno del VERDE"));
        (M==4, writeln("Turno del BLANCO"))
        ),
    !.

% -- Mover Jugador MORADO
% Igual que el caso anterior pero con las fichas MORADO  
moverJugadorB( X1 , Y1 , X2 , Y2 ):-
    cls,
    jugadorB( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorB( X1 , Y1 )),
    assert(jugadorB( X2 , Y2 )),
    not(finDeljuego),
    modoJuego(M),
    drawTablero,
    (   (M==2, writeln("Turno del AZUL"));
        (M==4, writeln("Turno del VERDE"))
        ),
    !.

% -- Mover Jugador NEGRO
% Igual que el caso anterior pero con las fichas NEGRO  
moverJugadorN( X1 , Y1 , X2 , Y2 ):-
    cls,
    jugadorB( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorB( X1 , Y1 )),
    assert(jugadorB( X2 , Y2 )),
    not(finDeljuego),
    modoJuego(M),
    drawTablero,
    (   (M==4, jugarComputadoraR(1))
        ),
    !.

% -- Mover Jugador AZUL
% Igual que el caso anterior pero con las fichas AZUL  
moverJugadorZ( X1 , Y1 , X2 , Y2 ):-
    cls,
    jugadorZ( X1 , Y1 ),
    movVal( X1 , Y1 , X2 , Y2 ),
    retract(jugadorZ( X1 , Y1 )),
    assert(jugadorZ( X2 , Y2 )),
    not(finDeljuego),
    modoJuego(M),
    drawTablero,
    (   (M==2, jugarComputadoraR(1));
        (M==3, jugarComputadoraR(1));
        (M==4, writeln("Turno del NEGRO"))
        ),
    !.

% ......................... XIII) INICIO DEL JUEGO  ..............................
%
% Para iniciar una nueva partida.
newGame:-
    cls, %Limpia la pantalla
    retract(modoJuego(_)), %Elimina el modo de juego
    retractall(computadoraR( _ ,_ )), %Elimina todas las fichas
    retractall(jugadorV( _ , _ )),
    retractall(jugadorB( _ , _ )),
    retractall(jugadorZ( _ , _ )),
    retractall(jugadorN( _ , _ )),
    retractall(jugadorA( _ , _ )),
    %Dibuja el menu
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
    read(X), %recibe el modo de juego deseado y crea el tablero correspondiente.
    cls,
    iniciarJuego(X).

% Inicializa le modo 2 jugadores
iniciarJuego(X):-
    X==1,
    inicialRojo, %Obtiene las posiciones iniciales de las fichas
    inicialVerde,
    assert(modoJuego(1)),
    drawTablero,
    writeln("Turno del VERDE"),!.

% inicializa el modo 3 jugadores
iniciarJuego(X):-
    X==2,
    inicialRojo, %Obtiene las posiciones iniciales de las fichas
    inicialBlanco,
    inicialAzul,
    assert(modoJuego(2)),
    drawTablero,
    writeln("Turno del BLANCO"),!.

% inicializa el modo 4 jugadores
iniciarJuego(X):-
    X==3,
    inicialRojo, %Obtiene las posiciones iniciales de las fichas
    inicialVerde,
    inicialAmarillo,
    inicialAzul,
    assert(modoJuego(3)),
    drawTablero,
    writeln("Turno del AMARILLO"),!.

% inicializa el modo 6 jugadores
iniciarJuego(X):-
    X==4,
    inicialRojo, %Obtiene las posiciones iniciales de las fichas
    inicialVerde,
    inicialAzul,
    inicialNegro,
    inicialBlanco,
    inicialAmarillo,
    assert(modoJuego(4)),
    drawTablero,
    writeln("Turno del AMARILLO"),!.

%Obtiene las posiciones iniciales de las fichas segun su color
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


% ................ XIV) EXTRAS ..................
%
% Revisa si X existe dentro de una lista.
miembro( X , [ X | _ ]).
miembro( X , [ _ | L ]):-
    miembro(X , L).


% Limpiar pantalla 
cls :- 
    %%cls.
    write('\e[2J').
    



