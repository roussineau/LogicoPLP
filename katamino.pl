:- use_module(piezas).

% Ejercicio 1
%! sublista(+D,+T,+L,-R)
% genera una sublista
sublista(D,T,L,R):- append(N,M,L),append(R,_,M),length(R,T),length(N,D).


% Ejercicio 2 PREGUNTAR
tablero(K,Xs):- length(Xs,5),tableroAux(K,Xs).
tableroAux(_,[]).
tableroAux(M,[H|T]):- length(H,M),tableroAux(M,T).

% Ejercicio 3, Preguntar si esta bien que se imprima el tablero
tamaño([H|T],F,C):- length([H|T],F),length(H,C).


% Ejercicio 4, BUG donde se queda buscando al final
coordenadas([Fila | _], IJ) :- length(Fila, M), between(1, 5, I), between(1, M, J), IJ = (I, J).

%ejercicio 5 PREGUNTAR
%insertar(X,L,Lx):- append(A,B,L),append(A,[X|B],Lx).		

%kPiezas(0,[]):-!.
%kPiezas(K,[H|T]):-nombrePiezas(L),member(H, L),Km1 is K - 1,kPiezas(Km1,T),not(member(H,T)),sort([H|T],[H|T]).

%kPiezas(1,[H]):-nombrePiezas(L),member(H, L).
%kPiezas(K,[H1,H2|T]):-K>1,nombrePiezas(L),member(H1, L),Km1 is K - 1,kPiezas(Km1,[H2|T]),not(member(H1,[H2|T])),H1@=<H2.

%%anda pero es horrible, revisar
%kPiezas(0, []) :- !.
%kPiezas(12, L) :- nombrePiezas(L), !.
%%kPiezas(K, [Letra | Cola]) :- nombrePiezas(L), member(Letra, L), Km1 is K - 1, kPiezas(Km1, Cola), sort([Letra | Cola], [Letra | Cola]).
%kPiezas(K, PS) :- aux(K, PS, 0).
%aux(0, [], _) :- !.
%aux(K, [Letra | Letras], Count) :- Km1 is K - 1, Tomo is 12 - Count, CountM1 is Count + 1, nombrePiezas(Piezas), sublista(Count, Tomo, Piezas, %Opciones), member(Letra, Opciones), aux(Km1, Letras, CountM1), not(member(Letra, Letras)).


%Ejercicio 6
seccionTablero(T,Alto,Ancho,(I,J),ST):- I2 is I-1, J2 is J-1, sublista(I2,Alto,T,R),seccionprima(R,Ancho,J2,ST).
seccionprima([],_,_,[]).
seccionprima([H|T],Ancho,J,[H1|T1]):- sublista(J,Ancho,H,H1),seccionprima(T,Ancho,J,T1).

%ejercicio 7
ubicarPieza(T,Id):- pieza(Id, Pieza), tamaño(Pieza,F,C),coordenadas(T,IJ),seccionTablero(T,F,C,IJ,Pieza).

%ejercicio 8
poda(sinPoda, _).

ubicarPiezas(Tablero,[]).
ubicarPiezas(Tablero,[H|T]):-ubicarPieza(Tablero,H),ubicarPiezas(Tablero,T).

%ejercicio 9
llenarTablero(C,Tablero):- tablero(C,Tablero),kPiezas(C,Xs),ubicarPiezas(T,Xs).

