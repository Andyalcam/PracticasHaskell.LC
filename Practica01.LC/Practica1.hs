{--anagrama :: [a] -> [a] -> Bool
anagrama x y = False

segmento :: Int -> Int -> [a] 
segmento x y = []

prodReps :: [a] -> Int
prodReps [x] = 1


elimina :: [a] -> Int -> [a]
elimina [x] i = []--}

{--
---	Equipo Omeguita
---	Alvarado Camacho Andrea		318064343
---	Mondragón Segoviano Alfonso	115000957
--}

module Practica1 where


--1. anagrama. Función que decide si dos palabras son un anagrama.
anagrama :: String -> String -> Bool
anagrama s1 s2 = error "Sin implementar."


--2. segmento. Función que devuelve la parte de la lista 
--             comprendida por los índices.
segmento :: Int -> Int -> [Int] -> [Int] 
segmento n _ _
    | n < 0 = [] --Indice inicio menor a 0
segmento _ m (x:xs)
    | m > length (x:xs) = [-1] --Indice de fin mayor a longitud del arreglo   
segmento _ _ [] = [] --Cuando se ingresa una lista vacía 
segmento n m (x:xs)
    | n == 0 = if m <= -1 -- Para indicar el fin de la iteración
                    then []
                    else [x] ++ segmento n (m-1) (xs)
    | otherwise = segmento (n-1) (m-1) (xs) -- Cuando aún no entramos dentro del rango 



--3. prodReps. Función que devuelve el producto del número con más 
--             repeticiones de una lista.
prodReps :: [Int] -> Int
prodReps l = error "Sin implementar."


--4. esEspejo. Función que determina si una fecha es espejo.
esEspejo :: String -> Bool
esEspejo [] = False
esEspejo [x] = True
esEspejo (x:xs) = if reversa (x:xs) == (x:xs)
                    then True
	                else False

--Función auxiliar para esEspejo.
reversa :: String -> String
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]


--5. elimina. Función que elimina de la lista el número del índice.
elimina :: [Int] -> Int -> [Int]
elimina [] _ = []
elimina _ n 
    | n < 0 = []
elimina (x:xs) n 
    | n == 0 = xs
    | otherwise = [x] ++ elimina (xs) (n-1)


--6. Binario. Tipo de dato para representación de binario.
data Binario = U | Cero Binario | Uno Binario

instance Show Binario where
    show U = "1"
    show (Cero b) = show b ++ "0" 
    show (Uno b) = show b ++ "1"

--a. suma. Función que obtiene el resultado de la suma de dos binarios.
suma :: Binario -> Binario -> Binario
suma b1 b2 = error "Sin implementar."

--b. antecesor. Función que obtiene el antencesor de un binario.
antecesor b = error "Sin implementar."

--OA. Tipo de dato para las operaciones aritméticas binarias. 
data OA = No | Suma OA OA | Resta OA OA | Producto OA OA 
            | Division OA OA | Modulo OA OA

type No = String          

--7. Definición de la instancia de la clase Show para mostrar las OA.            
--instance Show OA where
