{--
--- Equipo Omeguita
--- Alvarado Camacho Andrea     318064343
--- Mondragón Segoviano Alfonso 115000957
--}

module Practica3 where

import Practica2


{----- Formas Normales -----}

-- 1. fnn. Función que devuelve la Forma Normal Negativa de una 
--         proposición.
fnn :: Prop -> Prop
fnn p = fnnAux(elimImpl(elimEquiv(p)))

fnnAux :: Prop -> Prop
fnnAux (PVar p) = (PVar p)
fnnAux (PNeg p) = negAux p
fnnAux (POr p1 p2) = POr (fnnAux p1) (fnnAux p2)
fnnAux (PAnd p1 p2) = PAnd (fnnAux p1) (fnnAux p2)

negAux :: Prop -> Prop
negAux (PVar p) = PNeg (PVar p)
negAux (PNeg p) = fnnAux p
negAux (POr p1 p2) = PAnd (negAux p1) (negAux p2)
negAux (PAnd p1 p2) = POr (negAux p1) (negAux p2)

-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una 
--         proposición.
fnc :: Prop -> Prop
fnc p = dis(fnn(p))

dis :: Prop -> Prop
dis (PVar x) = (PVar x)
dis (PNeg p1) = (PNeg (dis p1))
dis (POr p1 (PAnd p2 p3)) = (PAnd (POr (dis p1) (dis p2)) (POr (dis p1) (dis p3)))
dis (POr (PAnd p1 p2) p3) = (PAnd (POr (dis p1) (dis p3)) (POr (dis p2) (dis p3)))
dis (POr p1 p2) = (POr (dis p1) (dis p2))
dis (PAnd p1 p2) = (PAnd (dis p1) (dis p2))

{----- Algoritmo DPLL -----}

-- Definiciones de algunos conceptos.
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)


--clausulaAux :: Clausula -> Literal

-- 3. unit. Función que aplica la regla unitaria.
unit :: Solucion -> Solucion
unit (m, []) = error "Ingresa una Formula"
unit (m, f) = if (formulaUnit(f)) == True then unitAux m f
                                    else (m,f)

unitAux :: Modelo -> Formula -> Solucion
unitAux (m) (x:xs) = if (auxC(x)) == True
                            then ( m ++ [auxCF(x)],(xs))
                            else unitAux (m) (xs ++ [x])

auxCF :: Clausula -> Literal
auxCF (x:xs) = x

auxC :: Clausula -> Bool
auxC (x:xs) = varUnit x

formulaUnit :: Formula -> Bool
formulaUnit [] = False
formulaUnit [x] = lista x
formulaUnit (x:xs) = if (lista x) == True then True
                            else formulaUnit xs

varUnit :: Literal -> Bool
varUnit (PNeg(PVar p)) = True
varUnit (PVar p) = True
varUnit l = False

-- 4. elim. Función que aplica la regla de eliminación. 
elim :: Solucion -> Solucion
elim ([], f) = ([],f)
elim (m, f) = if (con2 m f) == True then elimAuxx m f else (m, f)

elimAuxx :: Modelo -> Formula -> Solucion
elimAuxx [] f = ([],f)
elimAuxx (m:ms) (x) = if (con m x) == True then elimAux ([m] ++ ms) (x) else elimAuxx (ms ++ [m]) (x)

elimAux :: Modelo -> Formula -> Solucion
elimAux [] f = ([],f)
elimAux (m:ms) (x:xs) = if (containsP m x) == True then (m:ms,xs) else elimAux (m:ms) (xs ++ [x])

containsP :: Literal -> Modelo -> Bool
containsP l [] = False
containsP l (x:xs)
  | equals l x = True
  | otherwise = l `containsP` xs

con2 :: Modelo -> Formula -> Bool
con2 [] f = False
con2 [x] f = con x f
con2 (x:xs) f = if (con x f) == True then True
                            else con2 xs f

con :: Literal -> Formula -> Bool
con l [] = False
con l (x:xs)
    | containsP l x = True
    | otherwise = l `con` xs

con3 :: Modelo -> Formula -> Bool
con3 [] f = False
con3 [x] f = con (PNeg x) f
con3 (x:xs) f = if (con (PNeg x) f) == True then True
                            else con3 xs f

-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red ([], f) = ([],f)
red (m, f) = if (con3 m f) == True then redAux m f else (m, f)

auxCola :: Clausula -> Formula
auxCola (x:xs) = [xs]

redAux :: Modelo -> Formula -> Solucion
redAux (m) (x:xs) = if (containsP (PNeg(auxCF x)) (m)) == True
                                then (m, (auxCola(x) ++ xs))
                                else redAux (m) (xs ++ [x])

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m,f) = [splitAux1(m,f)] ++ [splitAux2(m,f)]

splitAux1 :: Solucion -> Solucion
splitAux1 (m , (x:xs)) = (m ++ [deMorgan(auxCF(x))], (x:xs) )


splitAux2 :: Solucion -> Solucion
splitAux2 (m , (x:xs)) = (m ++ [deMorgan(PNeg(auxCF(x)))], (x:xs) )


-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
conflict ([],f) = False
conflict (m:ms,f) = if (con (PNeg(m)) f) == True then (conflictAux (m:ms, f))
                                else conflict (ms,f)

conflictAux :: Solucion -> Bool
conflictAux (m, []) = False
conflictAux ((m:ms), (x:xs)) = if ((lista x) && (con (PNeg(m)) (x:xs))) == True
                                then True
                                else conflictAux (m:ms, xs) 

lista :: [a] -> Bool
lista [] = False
lista [x] = True
lista (x:xs) = False

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, []) = True
success (m,f) = False

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = red(elim(unit(m,f)))



{-- Puntos Extra --}


--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = if (success (m,f)) == True
                    then (m,f)
                    else if (conflict (m,f)) == True
                        then (m,f)
                        else if (equalsF f (snd(unit(m,f)))) == False
                            then dpll (unit(m,f))
                            else if (equalsF f (snd(elim(m,f)))) == False
                                then dpll (elim(m,f))
                                else if (equalsF f (snd(red(m,f)))) == False
                                    then dpll (red(m,f))
                                    else (m,f)

equalsL :: Clausula -> Clausula -> Bool
equalsL [] [] = True
equalsL [] c = False
equalsL c [] = False
equalsL [c] [d] = equals c d
equalsL (c:cs) (d:ds) = if (equals c d) == False then False else equalsL cs ds

equalsF :: Formula -> Formula -> Bool
equalsF [] [] = True
equalsF [] f = False
equalsF f [] = False
equalsF [f] [g] = equalsL f g
equalsF (f:fs) (g:gs) = if (equalsL f g) == False then False else equalsF fs gs






