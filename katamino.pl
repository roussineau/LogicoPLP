:- use_module(piezas).

% -- Ej 1:
sublista(Descartar, Tomar, Lista, Res) :- append(Descartado, NoDescartado, Lista), append(Res, _, NoDescartado), length(Res, Tomar), length(Descartado, Descartar).

% -- Ej 2:
tablero(K, T) :- length(T, 5), todosLongIguales(T, K).
%revisar, tratar de hacer sin aux

todosLongIguales([], _).
todosLongIguales([L | Ls], N) :- length(L, N), todosLongIguales(Ls, N).

% -- Ej 3:
tamaño([Fila | Filas], Alto, Ancho) :- length(Fila, Ancho), length([Fila | Filas], Alto).

% -- Ej 4:
coordenadas([Fila | _Filas], IJ) :- length(Fila, M), between(1, 5, I), between(1, M, J), IJ = (I, J).

% -- Ej 5:
kPiezas(K, PS) :- nombrePiezas(L), tomar(K, L, PS).

tomar(0, _XS, []).
tomar(K, [X | XS], [X | TomarKm1DeXS]) :- K > 0, Km1 is K - 1, tomar(Km1, XS, TomarKm1DeXS).
tomar(K, [_X | XS], TomarKDeXS) :- K > 0, length(XS, L), L >= K, tomar(K, XS, TomarKDeXS).

% -- Ej 6:
seccionTablero(Tablero, Alto, Ancho, (I, J), Seccion) :- Im1 is I - 1, Jm1 is J - 1, sublista(Im1, Alto, Tablero, CorteAlto), subListar(CorteAlto, Jm1, Ancho, Seccion).
%Por ahi se puede hacer mejor, revisar

subListar([], _A, _B, []).
subListar([L | LS], A, B, [Sublistado | Sublistados]) :- sublista(A, B, L, Sublistado), subListar(LS, A, B, Sublistados).

% -- Ej 7:
ubicarPieza([Fila | Tablero], Id):- pieza(Id, Pieza), tamaño(Pieza, AltoPieza, AnchoPieza), length(Fila, AnchoTablero), F2 is 6 - AltoPieza, C2 is AnchoTablero - AnchoPieza + 1, between(1, F2, I), between(1, C2, J), seccionTablero([Fila | Tablero], AltoPieza, AnchoPieza, (I,J), Pieza).
%revisar, hacer mas declarativo

% -- Ej 8:
ubicarPiezas(_Tablero, _Poda, []). 
ubicarPiezas(Tablero, Poda, [Id | Ids]) :- ubicarPieza(Tablero, Id), poda(Poda, Tablero), ubicarPiezas(Tablero, Poda, Ids).

poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

% -- Ej 9:
llenarTablero(Poda, Columnas, Tablero) :- tablero(Columnas, Tablero), kPiezas(Columnas, Piezas), ubicarPiezas(Tablero, Poda, Piezas).

% -- Ej 10:
cantSoluciones(Poda, Columnas, N) :- findall(T, llenarTablero(Poda, Columnas, T), TS), length(TS, N).

% ?- time(cantSoluciones(sinPoda, 3, N)).
%%19,214,618 inferences, 1.375 CPU in 1.381 seconds (100% CPU, 13974268 Lips)
%N = 28.

% ?- time(cantSoluciones(sinPoda, 4, N)).
%% 1,020,317,265 inferences, 73.078 CPU in 73.521 seconds (99% CPU, 13962007 Lips)
%N = 200.

% -- Ej 11:
todosGruposLibresModulo5(Tablero) :- todasCoordsLibres(Tablero, Coords), agrupar(Coords, Grupos), todosMod5(Grupos).

todosMod5([]).
todosMod5([X | XS]) :- length(X, L), 0 is mod(L, 5), todosMod5(XS).
%revisar: Esto esta MAL, piden explicitamente que no sea recursivo

coordLibre((I, J), T) :- nth1(I, T, FilaIesima), nth1(J, FilaIesima, Pos), var(Pos).

todasCoordsLibres(T, Res) :- findall(IJ, coordLibre(IJ, T), Res).
