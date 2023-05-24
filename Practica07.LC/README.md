# Practica06.LC
## Alvarado Camacho Andrea			318064343
## Mondragon Segoviano Alfonso		115000957

- - - -

Ejercicio 1.  
Dada la firma escrita en el pdf, definimos la suma y el producto de forma parecida a Haskell, teniendo un caso base y un caso inductivo usando la definición de lógica que vimos en clase.

Ejercicio 2.

Aux1. Neutro Suma. Definimos un lema para demostrar que ∀x que pertenece a los naturales si se le suma cero el resultado será x. Demostrándolo por inducción sobre x, donde el caso base es trivial porque cero = cero+cero, en el caso inductivo sustituimos la x por su sucesor y por la definicion de suma, simplificamos y tenemos como resultado el sucesor de la suma de x con cero, dado que teniamos como hipotesis la suma de x con cero, podemos decir que es trivial ya que tenemos el sucesor de x igual al sucesor de x.

Aux2. Lema Aditivo. Para todo x,y vamos a demostrar que la suma de x con el sucesor de y es igual al sucesor de la suma de x con y. Demostrándolo por induccion sobre x, donde el caso base llegamos a algo trivial ya que sabemos que cualquier numero sumado con cero, tendremos como resultado ese mismo número, en el caso inductivo igual por la definición de la suma y por nuestra hipótesis de inducción llegamos a algo trivial con el sucesor del sucesor de la suma de x con y.

Aux3. Conmutatividad sobre la suma. Para todo x,y desmostraremos por inducción sobre y, donde el caso base llegamos a algo trivial debido a como definimos la suma solamente podemos simplificar cuando cero es el primer parámetro de la suma, dando como resultado x y volvemos a escribir x como un Neutro Suma y dada la definición de Neutro Suma se llega a algo trivial, en el caso inductivo llegamos que la suma del sucesor de y con x, al simplificarlo tenemos como resultado de el sucesor de la suma de y con x, con la definición de suma y con nuestra hipótesis de inducción llegamos a algo trivial.

Aux4. Asociatividad sobre la suma. a + (b + c) = (a + b) + c. Haciendo inducción sobre a, donde el caso base es trivial, en el caso inductivo simplificamos la suma como su definición lo dice, vamos a simplificar las expresiones de suma conforme a la definición inductiva que dimos, sustituimos con nuestra hipótesis de inducción y llegamos a que es algo trivial.

Aux5. Análogo al Aux4.

Con los auxiliares mencionados pudimos resolver este ejercicio. Para todo n,m,r que pertenezca a los naturales vamos a demostrar la distributividad del producto sobre la suma. Lo demostraremos con inducción sobre n, en el caso base llegamos a que cero es igual a cero por lo que decimos que es algo trivial, en el caso inductivo reescribimos el producto como lo definimos en su caso recursivo, es decir, en una suma utilizando nuestra hipótesis de inducción y renombramos a los productos nm como x y nr como y, ahora lo que tenemos que demostrar es que (x + y) + (m + r) = (x + m) + (y + r), aplicamos conmutatividad al lado izquierdo de la igualdad para después asociar como nuestro auxiliar 5 volver aplicar conmutatividad asociar como nuestro auxiliar 4 y aplicamos conmutatividad de nuestro auxiliar 5 y ahora nos queda la siguiente igualdad (r + x) + (y + m) = (x + m) + (y + r), volvemos aplicar asociatividad como nuestro auxiliar 5, conmutatividad de el término específico de m + y para que nos dé y + m, aplicamos asociatividad en nuestro auxiliar 4 y por último asociatividad de nuestro auxiliar 5 por lo que llegamos a algo trivial y queda demostrado.

Ejercicio 3.
P.D. Para cualquier número n que pertenezca a los naturales, su sucesor es distinto de cero. Para esta demostración tambien la haremos por induccion sobre n. Como lo que buscamos demostrar es una desigualdad, la reescribiremos como si esa igualdad se cumple, entonces implica false, de modo que nuestra meta es False. Para el caso base tenemos como hipotesis que Suc cero = cero, lo cual a simple vista es falso, por lo que con un discriminate terminamos este caso. Ahora pasamos con el caso inductivo, de manera análoga lo analizamos cuando la igualdad implica False y de nuevo notemos que como hipotesis tenemos algo obviamente falso: Suc (Suc n) = cero, ya que a pesar de que n = cero, por tener dos "Suc" antes, ya lo hace diferente de cero.

P.D. Para cualesquiera números n y m que pertenezcan a los naturales, si el sucesor de n es igual al sucesor de m, entonces n es igual a m.
Theorem SucIgual: forall n m: naturales, (Suc n = Suc m) -> (n = m).
A este no le supimos :c, ahi dejamos nuestro humilde intento.


Ejercicio 4.
Definimos nuestras variables, la proposición y empezamos la demostración. Introducimos las hipótesis, destruímos el si y solo si en dos implicaciones y hacemos lo mismo con la conclusión para demostrar la primera implicación, hacemos una intro para solo demostrar p o q, aplicamos disyunción derecha y como teníamos a q por hipótesis hacemos un assumption y quedó demostrado nuestra primera implicación, ahora demostraremos la segunda implicación, volvemos hacer una intro para tener a p o q como hipótesis, destruímos esa p o q, eso quiere decir que con p como hipótesis tenemos que llegar a q por lo que hacemos un apply con nuestra hipótesis 0, dejándonos por demostrar ahora a p y dado que a p ya la teniamos como hipótesis ya queda demostrada esa parte del destruct, ahora tenemos que demostrar q con q como hipótesis pero es trivial por lo que queda demostrado.

Ejercicio 5.
Definimos nuestras variables, la proposición y empezamos la demostración. Introducimos las hipótesis y todas las conjunciones que tenemos las separamos en hipótesis, dado que en la conclucion tenemos -t y eso se puede ver expresado como t implica falso, aplicamos intro y ahora tenemos a t como hipótesis y nuestra meta como false, aplicamos apply con h0 ya que -r lo podemos ver como r implicando a falso, dejándonos ahora por demostrar a r, destruímos a p o s entonces con p en las hipótesis tenemos que demostrar a r, aplicamos nuestra hipótesis de p -> q -> r, dejándonos como submetas a p y q, como a p la tenemos como hipótesis entonces demostraremos q, le aplicamos nuestra hipótesis 2, teniendo como nueva meta a t, la cuál ya teníamos como hipótesis lo cuál ya queda demostrado, por último demostramos el otro lado del destruct, es decir que con s en las hipótesis queremos demostrar r, volvemos aplicar nuestras hipotesis de p -> q -> r y volviendonos a dejar por demostrar a p y q, hacemos un destruct de s -> falso, para demostrar solamente a s y como s es nuestra hipótesis, queda demostrado, y ya por último demostraremos q, aplicamos t -> q para demostrar t la cual es exactamente una de nuestras hipótesis y no hay más metas por demostrar.