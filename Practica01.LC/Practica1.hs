{--
--- Equipo Omeguita
--- Alvarado Camacho Andrea     318064343
--- Mondragón Segoviano Alfonso 115000957
--}

module Practica1 where

import Data.List
import Data.Char

--1. anagrama. Función que decide si dos palabras son un anagrama.
anagrama :: String -> String -> Bool
anagrama [] _ = False
anagrama _ [] = False
anagrama (x:xs) (y:ys) = let s1 =sort(x:xs); s2 = sort(y:ys) in if s1 == s2
                                                                 then True
                                                                 else False

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
prodReps (x:xs) = let tupla = prodRepsAux(x:xs) in (fst(tupla)^snd(tupla))

--Función auxiliar para prodReps 
-- Param [a] lista de los repetidos
-- return (las veces que se repite, numero que se repite) jsjsjs
prodRepsAux :: [Int] -> (Int,Int)
prodRepsAux [] = (0,0)
prodRepsAux [x] = (1,x)
prodRepsAux (x:xs) = (1 + fst( prodRepsAux(repetidos(x:xs) ) ), snd(prodRepsAux(repetidos(x:xs))))

--Función auxiliar regresa una lista de los números repetidos en una lista
repetidos :: (Eq a) => [a] -> [a]
repetidos [x] = []
repetidos (x:xs) 
    | not(x `elem` xs) = repetidos xs
    | otherwise = x : repetidos xs

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
data Binario = U | C | Cero Binario | Uno Binario 

instance Show Binario where
    show U = "1"
    show C = "0"
    show (Cero b) = show b ++ "0" 
    show (Uno b) = show b ++ "1"

--a. suma. Función que obtiene el resultado de la suma de dos binarios.
suma :: Binario -> Binario -> Binario
suma b1 C = b1
suma U U = (Cero U)
suma U (Uno b1) = (Cero (suma U b1))
suma U (Cero b1) = (Uno (b1))
suma b1 U = suma U (b1)
suma (Cero b1) (Cero b2) = (Cero (suma b1 b2))
suma (Uno b1) (Cero b2) = (Uno (suma b1 b2))
suma (Uno b1) (Uno b2) = (Cero (suma U (suma b1 b2)))
suma (Cero b1) (Uno b2) = (Uno (suma b1 b2))

--b. antecesor. Función que obtiene el antencesor de un binario.
antecesor :: Binario -> Binario
antecesor U = C
antecesor b = restar b U

--Funcion restar, auxiliar para antecesor
restar :: Binario -> Binario -> Binario
restar U U = C
restar C C = C
restar U C = U 
restar b1 C = b1
restar (Cero U) (U) = (Uno C)
restar (Cero b1) (Uno U) = restar (acarreoMinuendo (Cero b1) b1) (acarreoSustraendo (Uno U) (Cero b1))
restar (Cero b1) (U) = restar (acarreoMinuendo (Cero b1) b1) (acarreoSustraendo U (Cero b1))
restar (Uno b1) (U) = (Cero (restar b1 C))
restar (Cero b1) (Cero U) = (Cero (restar b1 U))
restar (Cero b1) (Uno b2) = restar (acarreoMinuendo (Cero b1) b1) (acarreoSustraendo (Uno b2) (Cero b1))
restar (Cero b1) (Cero b2) = (Cero (restar b1 b2))
restar (Uno b1) (Cero b2) = (Uno (restar b1 b2))
restar (Uno b1) (Uno b2) = (Cero (restar b1 b2))

--Funcion auxiliar de resta para el minuendo
acarreoMinuendo :: Binario -> Binario -> Binario
acarreoMinuendo (Cero b1) (Cero b2) = Uno(Cero b2)
acarreoMinuendo (Cero b1) (Uno b2) = Uno(Cero b2)
acarreoMinuendo (Cero b1) U = (Uno C)

--Funcion auxiliar de resta para el sustraendo
acarreoSustraendo :: Binario -> Binario -> Binario
acarreoSustraendo (U) (Cero (Uno b2)) = (Cero C)
acarreoSustraendo (U) (Cero (Cero b2)) = (Cero U)
acarreoSustraendo (U) _ = U

--OA. Tipo de dato para las operaciones aritméticas binarias. 
data OA = No String| Suma OA OA | Resta OA OA | Producto OA OA 
            | Division OA OA | Modulo OA OA

type No = String    

--7. Definición de la instancia de la clase Show para mostrar las OA.
instance Show OA where
    show (No n) = n
    show (Suma n m) = "(" ++ show n ++ "+" ++ show m ++ ")"
    show (Resta n m) = "(" ++ show n ++ "-" ++ show m ++ ")"
    show (Producto n m) = "(" ++ show n ++ "*" ++ show m ++ ")"
    show (Division n m) = "(" ++ show n ++ "÷" ++ show m ++ ")"
    show (Modulo n m) = "(" ++ show n ++ "%" ++ show m ++ ")"





