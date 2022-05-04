{--
--- Equipo Omeguita
--- Alvarado Camacho Andrea     318064343
--- Mondragón Segoviano Alfonso 115000957
--}

module Practica4 where

--Definición del tipo de datos para términos.
data Term = V Nombre | F Nombre [Term]

--Definición del tipo de datos para fórmulas.
data Form = NForm | TrueF | FalseF | Pr Nombre [Term] | Eq Term Term | 
            Neg Form | Conj Form Form | Disy Form Form | 
            Imp Form Form | Equi Form Form | All Nombre Form | 
            Ex Nombre Form

type Nombre = String

type Subst = [(Nombre,Term)]


--Instancia Show para Term.
instance Show Term where
  show (V x) = x
  show (F f t) = f ++ "(" ++ show t ++ ")"

--Instancia Show para Form.
instance Show Form where
  show NForm = ""
  show TrueF = "T"
  show FalseF = "F"
  show (Pr p t) = p ++ "(" ++ show t ++ ")"
  show (Eq t1 t2) = "(" ++ show t1 ++ "=" ++ show t2 ++ ")"
  show (Neg f) = "¬" ++ show f
  show (Conj f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
  show (Disy f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
  show (Equi f1 f2) = "(" ++ show f1 ++ " <--> " ++ show f2 ++ ")"
  show (All x f) = "Alle " ++ x ++ " (" ++ show f ++ ")" 
  show (Ex x f) = "Ein " ++ x ++ " (" ++ show f ++ ")"



--alcance. Función que devuelve el alcance de los cuantificadores de
--          una fórmula.
alcance :: Form -> [(Form, Form)]
alcance NForm = []
alcance TrueF = []
alcance FalseF = []
alcance (Eq t1 t2) = []
alcance (Pr n t) = []
alcance (All n f) = [(All n NForm, f)] ++ alcance f
alcance (Ex n f) = [(Ex n NForm, f)] ++ alcance f
alcance (Neg f) = alcance f
alcance (Conj f1 f2) = alcance f1 ++ alcance f2
alcance (Disy f1 f2) = alcance f1 ++ alcance f2
alcance (Imp f1 f2) = alcance f1 ++ alcance f2
alcance (Equi f1 f2) = alcance f1 ++ alcance f2

--bv. Función que devuelve las variables ligadas de una fórmula.
bv :: Form -> [Nombre]
bv NForm = []
bv TrueF = []
bv FalseF = []
bv (Eq t1 t2) = []
bv (Pr n t) = []
bv (All n f)
        | contains n (vars f) = [n] ++ bv f
        | otherwise = []
bv (Ex n f)
        | contains n (vars f) = [n] ++ bv f
        | otherwise = []
bv (Neg f) = bv f
bv (Conj f1 f2) = bv f1 ++ bv f2
bv (Disy f1 f2) = bv f1 ++ bv f2
bv (Imp f1 f2) = bv f1 ++ bv f2
bv (Equi f1 f2) = bv f1 ++ bv f2

--Aux1. contains. Función auxiliar para saber si un elemento pertenece o esta contenido en una lista
contains :: (Eq a) => a -> [a] -> Bool
contains a [] = False
contains a (x:xs)
  | a == x = True
  | otherwise = a `contains` xs

--Aux2. sinRepetidos. Función auxiliar para quitar elementos repetidos de una lista
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos [x] = [x]
sinRepetidos (x:xs) 
    | (x `contains` xs) = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

--Aux3. vars. Funcion auxiliar que devuelve la lista de variables de una formula sin repeticiones
vars :: Form -> [Nombre]
vars f = sinRepetidos(varsAux f)

--Aux3. varsAux. Función auxiliar que devuelve la lista de variables de una formula
varsAux :: Form -> [Nombre]
varsAux NForm = []
varsAux TrueF = []
varsAux FalseF = []
varsAux (Pr n t) = varsFun t
varsAux (Eq t1 t2) = varsTerm t1 ++ varsTerm t2
varsAux (Neg f) = vars f
varsAux (Conj f1 f2) = vars f1 ++ vars f2
varsAux (Disy f1 f2) = vars f1 ++ vars f2
varsAux (Imp f1 f2) = vars f1 ++ vars f2
varsAux (Equi f1 f2) = vars f1 ++ vars f2
varsAux (All n f) = vars f
varsAux (Ex n f) = vars f

--Aux4. varsTerm. Función que devuelve la lista de variables de terminos
varsTerm :: Term -> [Nombre]
varsTerm (V x) = [x]
varsTerm (F f v) = varsFun v

--Aux5. varsFun. Función que devuelve la lista de variables de funciones
varsFun :: [Term] -> [Nombre]
varsFun [] = []
varsFun [V v] = [v]
varsFun [F f v] = varsFun v
varsFun (x:xs) = varsFun [x] ++ varsFun xs

--fv. Función que devuelve las variables libres de una fórmula.
fv :: Form -> [Nombre]
fv NForm = []
fv TrueF = []
fv FalseF = []
fv (Eq t1 t2) = varsTerm t1 ++ varsTerm t2
fv (Pr n t) = varsFun t
fv (All n f) = quita n (vars f)
fv (Ex n f) = quita n (vars f)
fv (Neg f) = fv f
fv (Conj f1 f2) = fv f1 ++ fv f2
fv (Disy f1 f2) = fv f1 ++ fv f2
fv (Imp f1 f2) = fv f1 ++ fv f2
fv (Equi f1 f2) = fv f1 ++ fv f2

--Aux6. quita. Función auxiliar que devuelve una lista sin cierto elemento
quita :: (Eq a) => a -> [a] -> [a]
quita a [] = []
quita a (x:xs)
    | a == x = quita a xs
    | otherwise = [x] ++ quita a xs

--sustTerm. Función que realiza la sustitución de variables en un término.
sustTerm :: Term -> Subst -> Term
sustTerm t [] = error "Devolver el termino"
sustTerm (V x) (y:ys) = if (x == fst(y)) 
                          then snd(y)
                          else (V x)
sustTerm (F f []) s = (F f [])
sustTerm (F f (x:xs)) (y:ys) = (F f (sustTermFun (x:xs)(y:ys) ) )

--susTermFun. Función auxiliar para las funciones cuando se quiere hacer una sustitución
sustTermFun :: [Term] -> Subst -> [Term]
sustTermFun (x:xs) [] = x:xs
sustTermFun [(V x)] (y:ys) = if (x == fst(y)) 
                              then [snd(y)]
                              else sustTermFun ([(V x)])(ys)
sustTermFun [(F f [])] s = [(F f [])]
sustTermFun [(F f (x:xs))] (y:ys) = [sustTerm (F f (x:xs)) (y:ys)]-- (sustTermFun (x:xs) (y:ys))
sustTermFun (x:xs) (y:ys) = sustTermFun ([x])(y:ys) ++ sustTermFun(xs)(y:ys)

--sustForm. Función que realiza la sustitución de variables en una 
--          fórmula sin renombramientos.
sustForm :: Form -> Subst -> Form
sustForm (NForm) s = NForm
sustForm (TrueF) s = TrueF
sustForm (FalseF) s = FalseF
sustForm (Pr n (x:xs)) s = (Pr n (sustTermFun(x:xs)(s))) 
sustForm (Eq t1 t2) s = (Eq (sustTerm t1 s) (sustTerm t2 s))
sustForm (Neg f) s = (Neg (sustForm f s))
sustForm (Conj f1 f2) s = (Conj (sustForm f1 s)(sustForm f2 s))
sustForm (Disy f1 f2) s = (Disy (sustForm f1 s)(sustForm f2 s))
sustForm (Imp f1 f2) s = (Imp (sustForm f1 s)(sustForm f2 s))
sustForm (Equi f1 f2) s = (Equi (sustForm f1 s)(sustForm f2 s))
sustForm (All n f) s = (All n (sustForm f s))
sustForm (Ex n f) s = (Ex n (sustForm f s))


--alphaEq. Función que dice si dos fórmulas son alpha-equivalentes.
alphaEq :: Form -> Form -> Bool
alphaEq NForm NForm = True
alphaEq TrueF TrueF = True
alphaEq FalseF FalseF = True
alphaEq (Pr n1 t1) (Pr n2 t2)
        | equals (fv (Pr n1 t1)) (fv (Pr n2 t2)) = True
        | otherwise = False
alphaEq (Eq t1 t2) (Eq t3 t4)
        | equals (fv (Eq t1 t2)) (fv (Eq t3 t4)) = True
        | otherwise = False
alphaEq (Neg f1) (Neg f2)
        | equals (fv (Neg f1)) (fv (Neg f2)) = True
        | otherwise = False
alphaEq (Conj f1 f2) (Conj f3 f4)
        | equals (fv (Conj f1 f2)) (fv (Conj f3 f4)) = True
        | otherwise = False
alphaEq (Disy f1 f2) (Disy f3 f4)
        | equals (fv (Disy f1 f2)) (fv (Disy f3 f4)) = True
        | otherwise = False
alphaEq (Imp f1 f2) (Imp f3 f4)
        | equals (fv (Imp f1 f2)) (fv (Imp f3 f4)) = True
        | otherwise = False
alphaEq (Equi f1 f2) (Equi f3 f4)
        | equals (fv (Equi f1 f2)) (fv (Equi f3 f4)) = True
        | otherwise = False
alphaEq (All n1 f1) (All n2 f2)
        | equals (fv (All n1 f1)) (fv (All n2 f2)) = True
        | otherwise = False
alphaEq (Ex n1 f1) (Ex n2 f2)
        | equals (fv (Ex n1 f1)) (fv (Ex n2 f2)) = True
        | otherwise = False
alphaEq f1 f2 = False

--Aux7. equals. Función auxiliar que devuelve true si dos listas son iguales y false en caso contrario
equals :: (Eq a) => [a] -> [a] -> Bool
equals [] [] = True
equals a [] = False
equals [] b = False
equals a b = if (length a == length b) then 
                    equalsAux a b
                else False
--Aux8. equalsAux. Función auxiliar que devuelve true si dos listas son iguales sin importar su orden y false en caso contrario
equalsAux :: (Eq a) => [a] -> [a] -> Bool
equalsAux a [] = False
equalsAux [] b = True
equalsAux (a:as) b 
        | contains a b = equalsAux as b
        | otherwise = False

{-- Puntos Extra
renom :: Form -> Form
renomConj :: Form -> Form
sustFormAlpha :: Form -> Subst -> Form
--}