pertenece(E, [X|XS]) :- E = X; pertenece(E, XS).

longitud([],0).
longitud([_|XS],N) :- longitud(XS, M), N is M+1.

/* indice(_,[],-1).
	indice(E,[X],0) :- E = X.
	indice(E,[X|XS],I) :- indice(E,[XS],N) , I is N+1. 
*/

