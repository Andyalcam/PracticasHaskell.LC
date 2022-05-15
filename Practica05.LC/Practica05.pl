pertenece(E, [X|XS]) :- E = X; pertenece(E, XS).

/*longitud([],0).
longitud([_|XS],N) :- longitud(XS, M), N is M+1.*/

/*indice(_,[],-1).
indice(E,[X],1) :- E = X.
% indice(E,[X],Y) :- E = X.
indice(E,[X|XS],I) :- E = X; indice(E, [XS], N), I is N+1.*/


destino(los_mochis, el_fuerte).
destino(el_fuerte, bahuichivo).
destino(bahuichivo, divisadero).
destino(divisadero, creel).

destino(X,Y,L) :- destino(X,Y), L = [X|[Y]].

viaje(X,Y,L) :- destino(X,Z),destino(Z,Y), L = [1].

fst([X|_],X).  
snd([_|XS],XS). 