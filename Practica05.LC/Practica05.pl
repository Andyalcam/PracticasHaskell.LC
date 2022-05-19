pertenece(E,[X]) :- E = X.
pertenece(E, [X|XS]) :- E = X; pertenece(E, XS).

indice(X,[X|_],0).
indice(E,[_|XS],I) :- indice(E, XS, N), I is N+1.

destino(los_mochis, el_fuerte).
destino(el_fuerte, bahuichivo).
destino(bahuichivo, divisadero).
destino(divisadero, creel).
regreso(X,Y) :- destino(Y,X).
viaje(W,Z) :- destino(W,Z).
viaje(X,Z) :- destino(X,Y), viaje(Y,Z).
viajeR(W,Z) :- regreso(W,Z).
viajeR(X,Z) :- regreso(X,Y), viajeR(Y,Z).

estacionesI(X,Z,[X,Z]) :- destino(X,Z).
estacionesI(X,Z,[X|C]) :- destino(X,Y), estacionesI(Y,Z,C).

estacionesR(X,Z,[X,Z]) :- regreso(X,Z).
estacionesR(X,Z,[X|C]) :- regreso(X,Y), estacionesR(Y,Z,C).

modCero(N,X) :- 0 is N mod X.
 
numDivAux(_,1,1).
numDivAux(N,X,ND) :- X>1, modCero(N,X), NX is X-1, numDivAux(N,NX,SD), ND is 1+SD.
numDivAux(N,X,ND) :- X>1, not(modCero(N,X)), NX is X-1, numDivAux(N,NX,ND).

numDiv(N,ND) :- numDivAux(N,N,ND).
 
primo(N) :- numDiv(N,2).

elimina_dups([],[]).
elimina_dups([X],[X]).
elimina_dups([X,X|XS],L) :- elimina_dups([X|XS],L).
elimina_dups([X,Y|YS],[X|ZS]) :- X \= Y, elimina_dups([Y|YS],ZS).

gustar(alexia,leer).
gustar(david,leer).
clima(nuboso).
clima(lluvioso).
odio(alexia,clima_lluvioso).
queLee(alexia,alicia_en_el_Pais_de_las_Maravillas,lewis_Carrol).
lee(david,clima_lluvioso).
lee(david,clima_nuboso).


esposo(autor,viuda).
esposo(padre,hija1).
hija(hija1,viuda).
hija(hija1,autor).
hija(hija2,hija1).
hija(hija2,padre).
hijo(autor,padre).
hijo(padre,autor).
hijo(hijo3,viuda).
hijo(hijo3,autor).
suegro(X,Y) :- (hija(A,X);hijo(A,X)), esposo(A,Y).
madrastra(X,Y) :- esposo(X,A), (hija(Y,A);hijo(Y,A)).
yerno(X,Y) :- esposo(X,A), hija(A,Y).
hermanos(X,Y) :- (hija(X,A);hijo(X,A)), (hija(Y,A);hijo(Y,A)).
nieta(X,Y) :- (hija(X,A); hijo(X,A)), (hija(A,Y); hijo(A,Y)).
abuelo(X,Y) :- (hija(Y,A); hijo(Y,A)), (hija(A,X); hijo(A,X)).
cuñado(X,Y) :- esposo(X,A), hermanos(A,Y).
tio(X,Y) :- hermanos(X,A), (hija(Y,A);hijo(Y,A)).
nuera(X,Y) :- esposo(A,X), hijo(A,Y).
bisnieto(X,Y) :- (hija(X,A); hijo(X,A)), (hija(A,B); hijo(A,B)), (hija(B,Y); hijo(B,Y)).
