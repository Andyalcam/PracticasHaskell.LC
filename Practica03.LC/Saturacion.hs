module Saturacion where

import Practica2
import Practica3

saturacion :: Clausula -> Clausula -> Bool
saturacion [] _ = False
saturacion (x:xs) [] 
        | containsProp (sat (x:xs) []) PTrue = True
        | otherwise = saturacion (x:xs) (sat (x:xs) [])

saturacion (x:xs) (y:ys)
        | containsProp (sat (x:xs) (y:ys)) PTrue = True
        | otherwise = saturacion ((x:xs)++(y:ys)) (sat (x:xs) (y:ys))

sat :: Clausula -> Clausula -> Clausula
sat [] _ = []
sat (x:xs) [] = eliminaPFalse(aux x (xs) ++ sat xs [])
sat (x:xs) (y:ys) = eliminaPFalse(aux x (y:ys) ++ sat xs (y:ys))

eliminaPFalse :: Clausula -> Clausula
eliminaPFalse [] = []
eliminaPFalse (x:xs)
        | (equals (x) (PFalse)) = eliminaPFalse xs 
        | otherwise = x : eliminaPFalse xs


containsProp :: Clausula -> Literal -> Bool
containsProp [] _ = False
containsProp (x:xs) l
        | equals x l = True
        | otherwise = xs `containsProp` l

satAux :: Literal -> Literal -> Literal

satAux (POr (PNeg(p1)) (p2)) (POr (p3) (p4)) = if (equals p1 p3)
                                                    then POr(p2)(p4)
                                                    else if (equals p1 p4)
                                                            then POr(p2)(p3)
                                                            else if isNeg(p2) && (equals (deMorgan(PNeg(p2))) p3)
                                                                    then POr((PNeg(p1)))(p4)
                                                                    else if isNeg(p2) && (equals (deMorgan(PNeg(p2))) p4)
                                                                        then POr((PNeg(p1)))(p3)
                                                                        else PFalse

satAux (POr (p1) (p2)) (POr (PNeg(p3)) (p4)) = if (equals p1 p3)
                                                then POr(p2)(p4)
                                                else if (equals p2 p3)
                                                        then POr(p1)(p4)
                                                        else if isNeg(p4) && (equals p1 (deMorgan(PNeg(p4))))
                                                            then POr(p2)(PNeg(p3))
                                                            else if isNeg(p4) && (equals p2 (deMorgan(PNeg(p4))))
                                                                    then POr (p1)(PNeg(p3))
                                                                    else PFalse                        

satAux (POr (PNeg(p1)) (p2)) (p3) = if (equals p1 p3)
                                        then p2
                                        else if isNeg(p2) && (equals (deMorgan(PNeg(p2))) p3)
                                                then (PNeg(p1))
                                                else PFalse


satAux (POr (p1)(p2)) (PNeg(p3)) = if (equals p1 p3)
                                        then p2
                                        else if (equals p2 p3)
                                            then p1
                                            else PFalse

                                    
satAux (PNeg(p1)) (POr (p2) (p3))  = if (equals p1 p2)
                                        then p3 
                                        else if (equals p1 p3)
                                                then p2
                                                else PFalse

satAux (p1) (POr (p2) (p3)) = if isNeg(p2) && (equals p1 (deMorgan(PNeg(p2))))
                                then p3
                                else if isNeg(p3) && (equals p1 (deMorgan(PNeg(p3))))
                                        then p2 
                                        else PFalse

satAux (PNeg(p1)) (p2) 
        | (equals p1 p2) = PTrue
        | otherwise = PFalse

satAux (p1) (PNeg(p2))
        | (equals p1 p2) = PTrue
        | otherwise = PFalse                            

satAux p1 p2 = PFalse

aux :: Literal -> Clausula -> Clausula
aux x [] = []
aux x (y:ys) = [satAux x y] ++ aux x ys


isNeg :: Literal -> Bool
isNeg (PNeg p) = True 
isNeg p = False



-- ghci> saturacion ([POr(PVar "p")(PVar "q"),POr(PNeg(PVar "q"))(POr(PVar "r")(PVar "s")),POr(PNeg(PVar "p"))(PVar "s"),POr(PNeg(PVar "s"))(PVar "t"),POr(PNeg(PVar "r"))(PVar "p"), PNeg(PVar "t")]) ([]) 