# Practica1.LC
## Alvarado Camacho Andrea			318064343
## Mondragon Segoviano Alfonso		115000957

- - - -

## FUNCIONES AUXILIARES
* **`vNeg :: Bool -> Bool`**

Función que simula la tabla de verdad de la negación en lógica proposicional.
* **`rvImpl :: (Bool,Bool) -> Bool`**

Función que simula la tabla de verdad de una implicación en lógica proposicional.
* **`contains :: (Eq a) => a -> [a] -> Bool`**

Función auxiliar para saber si un elemento pertenece o esta contenido en una lista.
* **`varsAux :: Prop -> [String]`**

Función auxiliar que obtiene la lista de todas las variables y sus apariciones de una proposición, es decir con elementos repetidos.
* **`modelosAux :: [Estado] -> Prop -> [Estado]`**

Función auxiliar que dado todos los posibles estados de una proposicion y dicha proposicion, regresa una lista de estados, con solo aquellos estados que hacen que la interpretacion de la proposicion sea verdadera.
* **`tautologiaAux :: [Estado] -> Prop -> Bool`**

Función auxiliar que dado todos los posibles estados de una proposicion y la proposición dicha, regresa true si para todos los estados la interpretación es verdadera.
* **`sinRepetidos :: (Eq a) => [a] -> [a]`**

Función auxiliar que regresa una lista despues de quitar sus elementos repetidos.
* **`satisfAux :: [Estado] -> Prop -> Bool`**

Función auxiliar que dado todos los posibles estados de una proposicion y la proposición dicha, regresa true si para algun estado la interpretación es verdadera, es decir, con que exista un modelos que haga que la interpretación de la proposcion sea verdadera, la función regresará true.
* **`contradAux :: [Estado] -> Prop -> Bool`**

Función auxiliar que dado todos los posibles estados de una proposicion y la proposición dicha, regresa true si para todos los estados la interpretación es falsa, es decir, si existe tan solo un modelo que haga que la interpretacíon de la proposición sea verdadera, la función regresará false.
* **`equals :: Prop -> Prop -> Bool`**

Función auxiliar que dadas dos proposiciones regresa true si son totalmente iguales o son conmutativas respecto a la disyunción y conjunción.