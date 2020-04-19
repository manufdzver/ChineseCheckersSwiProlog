
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

