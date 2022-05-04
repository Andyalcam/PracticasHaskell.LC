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
--bv (All n f)
  --      | contains n (vars( snd alcance f)) = [n]
    --    | otherwise = []
--bv (Ex n f) = 

--Aux1. contains. Función auxiliar para saber si un elemento pertenece o esta contenido en una lista
contains :: (Eq a) => a -> [a] -> Bool
contains a [] = False
contains a (x:xs)
  | a == x = True
  | otherwise = a `contains` xs

--Aux2. vars. Función auxiliar que devuelve la lista de variables de una formula
vars :: Form -> [Nombre]
vars NForm = []
vars TrueF = []
vars FalseF = []
vars (Pr n t) = varsFun t
vars (Eq t1 t2) = varsTerm t1 ++ varsTerm t2
vars (Neg f) = vars f
vars (Conj f1 f2) = vars f1 ++ vars f2
vars (Disy f1 f2) = vars f1 ++ vars f2
vars (Imp f1 f2) = vars f1 ++ vars f2
vars (Equi f1 f2) = vars f1 ++ vars f2
vars (All n f) = vars f
vars (Ex n f) = vars f

--Aux3. varsTerm. Función que devuelve la lista de variables de terminos
varsTerm :: Term -> [Nombre]
varsTerm (V x) = [x]
varsTerm (F f v) = varsFun v

varsFun :: [Term] -> [Nombre]
varsFun [] = []
varsFun [V v] = [v]
varsFun [F f v] = varsFun v
varsFun (x:xs) = varsFun [x] ++ varsFun xs

--fv. Función que devuelve las variables libres de una fórmula.
fv :: Form -> [Nombre]
fv f = error "Sin implementar."

--sustTerm. Función que realiza la sustitución de variables en un término.
sustTerm :: Term -> Subst -> Term
sustTerm t s = error "Sin implementar."

--sustForm. Función que realiza la sustitución de variables en una 
--          fórmula sin renombramientos.
sustForm :: Form -> Subst -> Form
sustForm f s = error "Sin implementar."

--alphaEq. Función que dice si dos fórmulas son alpha-equivalentes.
alphaEq :: Form -> Form -> Bool
alphaEq f1 f2 = error "Sin implementar."

{-- Puntos Extra
renom :: Form -> Form
renomConj :: Form -> Form
sustFormAlpha :: Form -> Subst -> Form
--}