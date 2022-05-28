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
%hastaAbajo(X) :- not(hastaArriba(X)); not(sobre(_, X)).

mover(_,Y) :- not(bloqueado(Y)).
