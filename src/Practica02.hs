module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables f = sinRepetidos (aux f)
  where 
    aux (Var p) = [p]
    aux (Cons _) = []
    aux (Not p) = aux p
    aux (And p q) = aux p ++ aux q
    aux (Or p q) = aux p ++ aux q
    aux (Impl p q) = aux p ++ aux q
    aux (Syss p q) = aux p ++ aux q


-- Función auxiliar propia para eliminar duplicados
sinRepetidos [] = []
sinRepetidos (x:xs)
  | x `elem` xs = sinRepetidos xs
  | otherwise = x : sinRepetidos xs


--Ejercicio 2

--Función auxiliar para checar si un elemento existe en la lista.
pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) _   = b
interpretacion (Var p) i    = pertenece p i
interpretacion (Not p) i    = not (interpretacion p i)
interpretacion (And p q) i  = interpretacion p i && interpretacion q i
interpretacion (Or p q) i   = interpretacion p i || interpretacion q i
interpretacion (Impl p q) i = not (interpretacion p i) || interpretacion q i
interpretacion (Syss p q) i = interpretacion p i == interpretacion q i


--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles f = conjPotencia (variables f)
 

--Ejercicio 4

modelos :: Prop -> [Estado]
modelos f = [i | i <- estadosPosibles f, interpretacion f i]

--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f g = estadosIguales f g estados
  where 
    varsTotales = unirSinRepetir (variables f) (variables g)
    estados = conjPotencia varsTotales


-- Funciones auxiliares
 
estadosIguales :: Prop -> Prop -> [Estado] -> Bool
estadosIguales _ _ [] = True
estadosIguales f g (e:es) = 
    if interpretacion f e == interpretacion g e
    then estadosIguales f g es 
    else False

unirSinRepetir :: [String] -> [String] -> [String]
unirSinRepetir [] ys = ys
unirSinRepetir (x:xs) ys =
    if x `elem` ys
    then unirSinRepetir xs ys
    else x : unirSinRepetir xs ys 




--Ejercicio 6 

-- Función auxiliar que checa que los elementos cumplan el predicado
todosVerdaderos :: [Bool] -> Bool
todosVerdaderos [] = True
todosVerdaderos (x:xs) = x && todosVerdaderos xs

tautologia :: Prop -> Bool
tautologia f = todosVerdaderos [interpretacion f i | i <- estadosPosibles f]

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion f = null (modelos f)

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined


--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs
