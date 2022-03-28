{--
--- Equipo Omeguita
--- Alvarado Camacho Andrea     318064343
--- Mondragón Segoviano Alfonso 115000957
--}

module Practica2 where

--Prop. Tipo de datos para proposiciones lógicas.
data Prop = PTrue | PFalse | PVar String | PNeg Prop | POr Prop Prop 
                  | PAnd Prop Prop | PImpl Prop Prop | PEquiv Prop Prop

--Estado. Lista de variables asignadas como verdaderas.
type Estado = [String]

--Instancia Show para Prop.
instance Show Prop where
  show PTrue = "T"
  show PFalse = "F"
  show (PVar x) = "\"" ++ x ++ "\""
  show (PNeg (PNeg q)) = show q
  show (PNeg p) = "¬" ++ show p 
  show (POr p1 p2) = "(" ++ show p1 ++ " v " ++ show p2 ++ ")"
  show (PAnd p1 p2) = "(" ++ show p1 ++ " ^ " ++ show p2 ++ ")"
  show (PImpl p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (PEquiv p1 p2) = "(" ++ show p1 ++ " <--> " ++ show p2 ++ ")" 


--1. interp. Función que evalua una proposición dado el estado.
interp :: Estado -> Prop -> Bool

interp l (PVar x) = vNeg(vNeg (contains x l))
interp l (PNeg p) = vNeg(interp l p)
interp l (POr p1 p2) = (interp l p1) || (interp l p2)
interp l (PAnd p1 p2) = (interp l p1) && (interp l p2)
interp l (PImpl p1 p2) = vImpl((interp l p1),(interp l p2))
interp l (PEquiv p1 p2) = interp l (elimEquiv(PEquiv p1 p2))

--Aux1. vNeg. Función auxiliar para saber la tabla de verdad de la negación
vNeg :: Bool -> Bool
vNeg True = False
vNeg False = True

--Aux2. vImpl. Función auxiliar para saber la tabla de verdad de la implicación
vImpl :: (Bool,Bool) -> Bool
vImpl (True,False) = False
vImpl (x,y) = True

--Aux3. contains. Función auxiliar para saber si un elemento pertenece o esta contenido en una lista
contains :: (Eq a) => a -> [a] -> Bool
contains a [] = False
contains a (x:xs)
  | a == x = True
  | otherwise = a `contains` xs

--2. estados. Función que devuelve una lista de todas las combinaciones
--              posibles de los estados de una proposición.
estados :: Prop -> [Estado]
estados p = subconj(vars p)

--3. vars. Función que obtiene la lista de todas las variables de una
--          proposición.
vars :: Prop -> [String]
vars p = sinRepetidos(varsAux p)

--Aux4. varsAux. Función auxiliar que obtiene la lista de todas las variables y sus apariciones de una
--          proposición.
varsAux :: Prop -> [String]
varsAux (PVar x) = [x]
varsAux (PNeg p) = varsAux p 
varsAux (POr p1 p2) = varsAux p1 ++ varsAux p2
varsAux (PAnd p1 p2) = varsAux p1 ++ varsAux p2
varsAux (PImpl p1 p2) = varsAux p1 ++ varsAux p2
varsAux (PEquiv p1 p2) = varsAux p1 ++ varsAux p2

--4. subconj. Función que devuelve el conjunto potencia de una lista.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = [(x:ys) | ys <- subconj xs] ++ subconj xs

--5. modelos. Función que devuelve la lista de todos los modelos posibles
--              para una proposición.
modelos :: Prop -> [Estado]
modelos p = modelosAux (subconj(vars(p))) (p)

--Aux5. modelosAux. Función auxiliar que dado todos los posibles estados de una proposicion
--              y la proposicion dicha, regresa una lista de estados en los que la interpretacion es verdadera
modelosAux :: [Estado] -> Prop -> [Estado]
modelosAux [] _ = []
modelosAux (x:xs) f 
    | interp x f = [x] ++ modelosAux xs f
    | otherwise = modelosAux xs f

--6. tautologia. Función que dice si una proposición es tautología.
tautologia :: Prop -> Bool
tautologia p = tautologiaAux (subconj(vars(p))) (p)

--Aux6. tautologiaAux. Función auxiliar que dado todos los posibles estados de una proposicion
--              y la proposición dicha, regresa true si para todos los estados la interpretación es verdadera
tautologiaAux :: [Estado] -> Prop -> Bool
tautologiaAux [] _ = True 
tautologiaAux (x:xs) f
    | interp x f = tautologiaAux xs f 
    | otherwise = False

--Aux7. sinRepetidos. Función auxiliar para quitar elementos repetidos de una lista
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos [x] = [x]
sinRepetidos (x:xs) 
    | (x `contains` xs) = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

--7. satisfen. Función que resuelve si una proposición es satisfacible
--              con cierto estado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = interp e p

--8. satisf. Función que resuelve si una proposición es satisfacible.
satisf :: Prop -> Bool
satisf p = satisfAux (sinRepetidos(subconj(vars(p)))) (p)

--Aux8. satisfAux. Función auxiliar que dado todos los posibles estados de una proposicion
--              y la proposición dicha, regresa true si para algun estado la interpretación es verdadera
satisfAux :: [Estado] -> Prop -> Bool
satisfAux [] _ = False
satisfAux (x:xs) f 
    | interp x f = True
    | otherwise = satisfAux xs f 

--9. insatisfen. Función que resuelve si una proposición es insatisfacible
--                  con cierto estado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = vNeg(satisfen e p)

--10. contrad. Función que dice si una proposición es una contradicción.
contrad :: Prop -> Bool
contrad p = contradAux (sinRepetidos(subconj(vars(p)))) (p)

--Aux9. contradAux. Función auxiliar que dado todos los posibles estados de una proposicion
--              y la proposición dicha, regresa true si para todos los estados la interpretación es falsa
contradAux :: [Estado] -> Prop -> Bool
contradAux [] _ = True
contradAux (x:xs) f 
    | vNeg(interp x f) = contradAux xs f
    | otherwise = False

--11. equiv. Función que devuelve True si dos proposiciones son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv (PImpl p1 p2) p3 = equals (elimImpl(PImpl p1 p2)) (p3)
equiv p1 (PImpl p2 p3) = equals (elimImpl(PImpl p2 p3)) (p1)
equiv (PEquiv p1 p2) (p3) = if equals(elimEquiv((PEquiv p1 p2)))(p3) == True
                                then True
                                else if equals(elimImpl(elimEquiv((PEquiv p1 p2))))(p3) == True
                                    then True
                                    else False
equiv p1 (PEquiv p2 p3) = equals (elimImpl(elimEquiv(PEquiv p2 p3))) (p1)
equiv p1 p2 = equals p1 p2

--Aux10. equals. Función auxiliar que dadas dos proposiciones regresa true si son totalmente iguales
--              o son conmutativas.
equals :: Prop -> Prop -> Bool
equals (PVar p) (PVar q) = p == q
equals (PNeg (PNeg p1)) (p2) = equals (p1) (p2)
equals (p1) (PNeg (PNeg p2)) = equals (p1) (p2)
equals (PNeg p1) p2 
        | equals p1 p2 = False
        | otherwise = True
equals  p1 (PNeg p2)
        | equals p1 p2 = False
        | otherwise = True
equals (POr (PVar p1) (PVar p2)) (POr (PVar q1) (PVar q2))        
        | let variables = vars (POr (PVar p1) (PVar p2)) in (contains q1 variables == True) && (contains q2 variables == True) = True        
        | otherwise = False
equals (POr p1 p2) (POr q1 q2)
        |((equals p1 q1) == True) && ((equals p2 q2) == True) = True
        |otherwise = False
equals (PAnd (PVar p1) (PVar p2)) (PAnd (PVar q1) (PVar q2))        
        | let variables = vars (PAnd (PVar p1) (PVar p2)) in (contains q1 variables == True) && (contains q2 variables == True) = True        
        | otherwise = False
equals (PAnd p1 p2) (PAnd q1 q2)
        | ((equals p1 q1) == True) && ((equals p2 q2) == True) = True
        |otherwise = False
equals (PImpl p1 p2) (PImpl q1 q2)
        | ((equals p1 q1) == True) && ((equals p2 q2) == True) = True
        |otherwise = False
equals (PEquiv p1 p2) (PEquiv q1 q2)
        | ((equals p1 q1) == True) && ((equals p2 q2) == True) = True
        |otherwise = False 
equals p q = False

--12. elimEquiv. Función que elimina las equivalencias lógicas.
elimEquiv :: Prop -> Prop
elimEquiv (PVar x) = (PVar x)
elimEquiv (PNeg p) = (PNeg (elimEquiv p))
elimEquiv (POr p1 p2) = (POr (elimEquiv p1) (elimEquiv p2))
elimEquiv (PAnd p1 p2) = (PAnd (elimEquiv p1) (elimEquiv p2))
elimEquiv (PImpl p1 p2) = (PImpl (elimEquiv p1) (elimEquiv p2))
elimEquiv (PEquiv p1 p2) = (PAnd (PImpl (elimEquiv p1)(elimEquiv p2)) (PImpl (elimEquiv p2)(elimEquiv p1)))

--13. elimImpl. Función que elimina las implicaciones lógicas.
elimImpl :: Prop -> Prop
elimImpl (PVar x) = (PVar x)
elimImpl (PNeg p) = (PNeg (elimImpl p))
elimImpl (POr p1 p2) = (POr (elimImpl p1) (elimImpl p2))
elimImpl (PAnd p1 p2) = (PAnd (elimImpl p1) (elimImpl p2))
elimImpl (PImpl p1 p2) = (POr (PNeg (elimImpl p1)) (elimImpl p2))
elimImpl (PEquiv p1 p2) = (PEquiv (elimImpl p1) (elimImpl p2))

--14. deMorgan. Función que aplica las leyes de DeMorgan a una proposición.
deMorgan :: Prop -> Prop
deMorgan (PVar x) = (PVar x)
deMorgan (PNeg (PAnd p1 p2)) = (POr (PNeg (deMorgan p1) ) (PNeg (deMorgan p2)) )
deMorgan (PNeg (POr p1 p2)) = (PAnd (PNeg(deMorgan p1)) (PNeg (deMorgan p2)) )
deMorgan (PNeg p) = (PNeg (deMorgan p))
deMorgan (POr p1 p2) = (POr (deMorgan p1) (deMorgan p2))
deMorgan (PAnd p1 p2) = (PAnd (deMorgan p1) (deMorgan p2))
deMorgan (PImpl p1 p2) = (PImpl (deMorgan p1) (deMorgan p2))
deMorgan (PEquiv p1 p2) = (PEquiv (deMorgan p1) (deMorgan p2))