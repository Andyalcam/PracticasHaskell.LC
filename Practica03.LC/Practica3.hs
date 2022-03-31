{--
--
--
--}

module Practica3 where

import Practica2


{----- Formas Normales -----}

-- 1. fnn. Función que devuelve la Forma Normal Negativa de una 
--         proposición.
fnn :: Prop -> Prop
fnn p = fnnAux(elimImpl(elimEquiv(p)))

--Aux1. fnnAux. Funcion auxiliar de fnn donde tendremos una formula sin implicacion y doble implicacion
fnnAux :: Prop -> Prop
fnnAux (PNeg(PNeg p)) = fnnAux(p)
fnnAux (PNeg(PAnd p1 p2)) = fnnAux(POr (fnnAux(PNeg(p1))) (fnnAux(PNeg(p2))))
fnnAux (PNeg(POr p1 p2)) = fnnAux(PAnd (fnnAux(PNeg(p1))) (fnnAux(PNeg(p2))))
fnnAux (POr (p1)(p2)) = POr(fnnAux (p1))(fnnAux(p2))
fnnAux (PAnd (p1)(p2)) = PAnd(fnnAux (p1))(fnnAux(p2)) 
fnnAux p = p

-- fnn (PImpl (PAnd(PVar "p")(PImpl(PVar "q")(PVar "r"))) (PVar "s"))  =>  (("p" ^ ("q" -> "r")) -> "s")
-- fnn (PImpl(PAnd(PImpl(PVar "p")(PVar "r"))(PImpl(PVar "q")(PVar "r")))(PImpl(PAnd(PVar "p")(PVar "q"))(PVar "r")))  => ((("p" -> "r") ^ ("q" -> "r")) -> (("p" ^ "q") -> "r"))


-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una 
--         proposición.
fnc :: Prop -> Prop
fnc p = fncAux(fncAux(fncAux(elimImpl(elimEquiv(p)))))


--Aux1. fncAux. Funcion auxiliar de fnc donde tendremos una formula sin implicacion y doble implicacion

fncAux :: Prop -> Prop
fncAux (PNeg(PNeg p)) = fncAux(p)
fncAux (PNeg(PAnd p1 p2)) = fncAux(POr (PNeg(p1)) (PNeg(p2)))
fncAux (PNeg(POr p1 p2)) = fncAux(PAnd (PNeg(p1)) (PNeg(p2)))

fncAux (POr p1 (PAnd p2 p3)) = fncAux(PAnd (fncAux(POr(p1)(p2))) (fncAux(POr(p1)(p3))))
fncAux (POr (PAnd p1 p2) p3) = fncAux(PAnd (fncAux(POr(p1)(p3))) (fncAux(POr(p2)(p3))))

fncAux (POr p1 p2) = POr(fncAux(p1)) (fncAux(p2)) 

fncAux (PAnd (p1)(PAnd p2 p3)) = PAnd(fncAux(p1))(fncAux(PAnd p2 p3)) 
fncAux (PAnd (PAnd p1 p2)(p3)) = PAnd(fncAux(PAnd p1 p2))(fncAux p3)

fncAux (PAnd p1 p2) = PAnd(fncAux(p1)) (fncAux(p2)) 

fncAux (PVar p) = PVar p
fncAux p = p

-- POr (PImpl (PVar "p")(PVar "q")) (PImpl (PVar "q")(PVar "p")) -> (("p" -> "q") v ("q" -> "p")) 
-- PNeg(PAnd(PVar "p")(PImpl (PVar "q")(PVar "r"))) -> ¬("p" ^ ("q" -> "r"))
-- PAnd(PImpl(PImpl(PVar "q")(PVar "r"))(PVar "q"))(PImpl (PVar "r")(PVar "q")) -> ((("q" -> "r") -> "q") ^ ("r" -> "q"))
-- PImpl(PEquiv (PVar "p")(PVar "q"))(PVar "r") -> (("p" <--> "q") -> "r")



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
unit (m, f) = unitAux m f

unitAux :: Modelo -> Formula -> Solucion
unitAux [] (x:xs) = if auxC(x) == True
                            then ([auxCF(x)],(xs))
                            else unitAux [] (xs ++ [x])

auxCF :: Clausula -> Literal
auxCF (x:xs) = x

auxC :: Clausula -> Bool
auxC (x:xs) = varUnit x

varUnit :: Literal -> Bool
varUnit (PNeg(PVar p)) = True
varUnit (PVar p) = True
varUnit l = False

-- 4. elim. Función que aplica la regla de eliminación. 
elim :: Solucion -> Solucion
elim (m, f) = error "Sin implementar."

-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red (m, f) = error "Sin implementar."

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = error "Sin implementar."

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
conflict (m, f) = error "Sin implementar."

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) = error "Sin implementar."

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = error "Sin implementar."



{-- Puntos Extra --}

{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}