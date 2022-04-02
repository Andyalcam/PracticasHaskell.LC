# Practica3.LC
## Alvarado Camacho Andrea			318064343
## Mondragon Segoviano Alfonso		115000957

- - - -

## FUNCIONES AUXILIARES

Función auxiliar de Forma Normal Negativa que aplica ley de DeMorgan a toda la fórmula

* **`fnnAux :: prop -> prop`**

Función auxiliar de fnnAux para aplicar ley de DeMorgan

* **`negAux :: prop -> prop`**

Función auxiliar de Forma Normal Conjuntiva para aplicar distribución 

* **`dis :: prop -> prop`**

Función auxiliar de unit para iterar las cláusulas en busca de una literal 

* **`unitAux :: Modelo -> Formula -> Solucion`**

Función auxiliar para iterar una cláusula

* **`auxCF :: Clausula -> Literal`**

Función auxiliar de unit para verificar si una literal se encuentra en una cláusula

* **`auxC :: Clausula -> Bool`**

Función auxiliar de unit para saber si dentro de una formula hay una cláusula unitaria

* **`formulaUnit :: Formula -> Bool`**

Función auxiliar de unit para saber si una lista solamente contiene un elemento

* **`lista :: [a] -> Bool`**

Función auxiliar de unit para saber si se tiene una literal

* **`varUnit :: Literal -> Bool`**

Función auxiliar de elim que ayuda a iterar el modelo

* **`elimAuxM :: Modelo -> Formula -> Solucion`**

Función auxiliar de elim que ayuda a iterar las cláusulas

* **`elimAuxC :: Modelo -> Formula -> Solucion`**

Función auxiliar para saber si una literal se encuentra en un modelo

* **`containsP :: Literal -> Modelo -> Bool`**

Función auxiliar para saber si una literal se encuentra en una fórmula

* **`containsLF :: Literal -> Formula -> Bool`**

Funcíón auxiliar para saber si una literal fórmula

* **`containsMF :: Literal -> Formula -> Bool`**

Función auxiliar para saber si una literal del modelo está su complemento en la fórmula

* **`containsNMF :: Modelo -> Formula -> Bool`**

Función que devuelve la cola de una cláusula en una fórmula

* **`auxCola :: Clausula -> Formula`**

Función auxiliar de red para encontrar una literal complementaria y quitarla de la cláusula

* **`redAux :: Modelo -> Formula -> Solucion`**

Función auxiliar de split para obtener la primer literal de la primer cláusula de la fórmula

* **`splitAux :: Solución -> Solucion`**

Función auxiliar de split para obtener la primer literal de l primer cláusula de la fórmula y negarla

* **`splitAuxN :: Solucion -> Solucion`**

Función auxiliar de conflict que busca si hay algún conflicto con alguna literal y su complementaria

* **`conflictAux :: Solucion -> Bool`**

Función auxiliar que compara dos listas de literales, devuelve un True si son iguales y un False en caso contrario

* **`equalsL :: Clausula -> Clausula -> Booll`**

Función auxiliar que compara dos listas de listas de literales, devuelve un True si son iguales y un False en caso contrario

* **`equalsF :: Formula -> Formula -> Bool`**

