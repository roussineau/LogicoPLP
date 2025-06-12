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


% Ejercicio 4
coordenadas([Fila | _], IJ) :- length(Fila, M), between(1, 5, I), between(1, M, J), IJ = (I, J).

%ejercicio 5 PREGUNTAR
%insertar(X,L,Lx):- append(A,B,L),append(A,[X|B],Lx).

kPiezas(0,[]):-!.
kPiezas(K,[H|T]):-nombrePiezas(L),member(H, L),Km1 is K - 1,kPiezas(Km1,T),not(member(H,T)),sort([H|T],[H|T]).

%kPiezas(1,[H]):-nombrePiezas(L),member(H, L).
%kPiezas(K,[H1,H2|T]):-K>1,nombrePiezas(L),member(H1, L),Km1 is K - 1,kPiezas(Km1,[H2|T]),not(member(H1,[H2|T])),H1@=<H2.

%Ejercicio 6
 seccionTablero(T,Alto,Ancho,(I,J),ST):- I2 is I-1, J2 is J-1, sublista(I2,Alto,T,R),seccionprima(R,Ancho,J2,ST).
 seccionprima([],_,_,[]).
seccionprima([H|T],Ancho,J,[H1|T1]):- sublista(J,Ancho,H,H1),seccionprima(T,Ancho,J,T1).