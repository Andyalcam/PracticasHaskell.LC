# Practica1.LC
## Alvarado Camacho Andrea			318064343
## Mondragon Segoviano Alfonso		115000957

- - - -

## FUNCIONES AUXILIARES
* **`prodRepsAux :: [Int] -> (Int,Int)`**

Función que ayuda a prodReps que regresa una tupla en donde el primer elemento corresponde al total de apariciones de un número, y el segundo elemento corresponde al número que se contaron las repeticiones en la lista.
* **`repetidos :: (Eq a) => [a] -> \[a\]`**

Función que dada una lista regresa otra lista que contiene los números que se repiten el la primera, es decir si en la lista dada algún número solo tiene una aparición en la misma, no será agregado a la lista que regresa la función. Esta función la utilizamos en prodReps con el fin de acomodar estos números y sus repeticiones en las tuplas que regresa esa primera función auxiliar.
* **`reversa :: String -> String`**

Función que regresa la reversa de un String.
* **`restar :: Binario -> Binario -> Binario`**

Función que resta cualquier número binario con cualquier otro.
* **`acarreoMinuendo :: Binario -> Binario -> Binario`**

Función que nos ayuda a la resta de binarios al encontrar casos de acarreo negativos, es decir cuando querramos restar 0 - 1 en la primera parte de la resta.
* **`acarreoSustraendo :: Binario -> Binario -> Binario`**

Función que nos ayuda a la resta de binarios al encontrar casos de acarreo negativos, es decir cuando querramos restar 0 - 1 en la segunda parte de la resta.