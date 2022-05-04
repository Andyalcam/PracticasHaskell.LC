# Practica4.LC
## Alvarado Camacho Andrea			318064343
## Mondragon Segoviano Alfonso		115000957

- - - -

## FUNCIONES AUXILIARES


Función auxiliar que nos indica si un elemento está contenido en una lista.
* **`contains :: (Eq a) => a -> [a] -> Bool`**


Función auxiliar que elimina todos los elementos repetidos de una lista.
* **`sinRepetidos :: (Eq a) => [a] -> [a]`**

Función auxiliar que devuelve las variables de una fórmula.
* **`vars :: Form -> [Nombre]`**

Función auxiliar que devuelve la lista de variables de una formula.
* **`varsAux :: Form -> [Nombre]`**

Función auxiliar que devuelve la lista de variables de términos.
* **`varsTerm :: Term -> [Nombre]`**

Función auxiliar que devuelve la lista de variables de funciones.
* **`varsFun :: [Term] -> [Nombre]`**

Función auxiliar que elimina un elemento dado en una lista.
* **`quita :: (Eq a) => a -> [a] -> [a]`**

Función auxiliar que aplica sustitución a una lista de términos.
* **`sustTermFun :: [Term] -> Subst -> [Term]`**

Función auxiliar que devuelve true si dos listas son iguales y false en caso contrario.
* **`equals :: (Eq a) => [a] -> [a] -> Bool`**

Función auxiliar que devuelve true si dos listas son iguales sin importar su orden y false en caso contrario
* **`equalsAux :: (Eq a) => [a] -> [a] -> Bool`**

