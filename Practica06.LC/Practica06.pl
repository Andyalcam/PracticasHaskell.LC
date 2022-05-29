%%% 1. Convertidor Binario
letra(a,1100001).
letra(b,1100010).
letra(c,1100011).
letra(d,1100100).
letra(e,1100101).
letra(f,1100110).
letra(g,1100111).
letra(h,1101000).
letra(i,1101001).
letra(j,1101010).
letra(k,1101011).
letra(l,1101100).
letra(m_,1101101).
letra(n,1101110).
letra(o,1101111).
letra(p,1110000).
letra(q_,1110001).
letra(r,1110010).
letra(s_,1110011).
letra(t,1110100).
letra(u,1110101).
letra(v,1110110).
letra(w,1110111).
letra(x_,1111000).
letra(y_,1111001).
letra(z,1111010).

convertir(X,Y) :- letra(X,Y).
convertir([X],[Y]) :- letra(X,Y).
convertir([X|XS],[Y|YS]) :- letra(X,Y), convertir(XS,YS).

%%% 2. Mundo de Cubos
sobre(rojo, gris).
sobre(azul, rojo).
sobre(rosa, amarillo).
hastaArriba(gris).
hastaArriba(verde).
hastaArriba(amarillo).

bloqueado(X) :- not(hastaArriba(X)), sobre(X, _).

hastaAbajo(X) :- sobre(X,_), not(sobre(_, X)).

mover(_,Y) :- not(bloqueado(Y)).

%%% 3. Autómata

%%% 4. MergeSort

% ordernar una lista mediante metodo burbuja
%retorna la longitud de una lista
long([],0):-!.
long([_|M],V):-long(M,B),!, V is B + 1.

%método de ordenamiento burbuja, va comparando de a pares de números consecutivos
burbujaAux([],[]).
burbujaAux([X],[X]).
burbujaAux([X,Y],[X,Y]):-X=<Y,!.
burbujaAux([X,Y],[Y,X]):-!.
burbujaAux([X,Y|M],[X|L]):-X=<Y,!,burbujaAux([Y|M],L),!.
burbujaAux([X,Y|M],[Y|L]):-burbujaAux([X|M],L),!.

%llama al burbujeo N veces, donde N es la longitud de la lista
burbuja(L1,L1,0):-!.
burbuja(L1,L1,1):-!.
burbuja(L1,L2,N):-burbujaAux(L1,L3),!,N1 is N - 1, burbuja(L3,L2,N1),!.

mezclarAux([],[],[]):-!.
mezclarAux(L1,[],L1):-!.
mezclarAux([],L2,L2):-!.
mezclarAux([X],[Y],LMerge) :- X>=Y,!,LMerge = [Y|[X]].
mezclarAux([X],[Y],LMerge) :- X<Y,!, LMerge = [X|[Y]].
mezclarAux([X|XS],[Y|YS],LMerge) :- X<Y,!, mezclarAux(XS,[Y|YS],LM), LMerge = [X|LM].
mezclarAux([X|XS],[Y|YS],LMerge) :- X>=Y,!, mezclarAux([X|XS],YS,LM), LMerge = [Y|LM].

ordenar(L,LOrd) :- long(L,Long),burbuja(L,LOrd,Long).

mezclar([],[],[]).
mezclar(L1,[],L1).
mezclar([],L2,L2).
mezclar(L1, L2, LMerge) :- ordenar(L1,LOrd1), ordenar(L2,LOrd2), mezclarAux(LOrd1,LOrd2,LMerge).




