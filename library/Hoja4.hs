module Hoja4 where

data Dia = Lu | Ma | Mi | Ju | Vi | Sa | Do deriving (Eq, Ord, Show)

instance Enum Dia where
    fromEnum Lu = 1
    fromEnum Ma = 2
    fromEnum Mi = 3
    fromEnum Ju = 4
    fromEnum Vi = 5
    fromEnum Sa = 6
    fromEnum Do = 7
    toEnum 1 = Lu
    toEnum 2 = Ma
    toEnum 3 = Mi
    toEnum 4 = Ju
    toEnum 5 = Vi
    toEnum 6 = Sa
    toEnum 7 = Do
-- Clases de tipos Eq (==), Ord(<, >, <= ,>=) se definen en terminnos de (< e ==)

-- Tipos numericos Int (Bounded), Integer, Float, Double
-- Num -->  + - * negate abs signum fromInteger :: Integer -> a

--Integral --> quot rem div (divison doble), mod ,quotRem a -> a ->(a,a), divMod

-- Otras clases y tipo Enum, Show para tipos con elementos "mostrables", Read para tipos con elementos "leibles", Bounded

type Direccion = (Persona, Dir, Ciudad)
type Nombre = String
type Apellido = String
type Ciudad = String

data Persona = Per Nombre Apellido
data Dir = Calle String Int | Casa String

instance Show Persona where
    show (Per nombre apellido ) = nombre ++ " " ++ apellido

instance Show Dir where
        show (Calle s i) = s ++ " " ++ show i
        show (Casa s) = s 

instance Eq Persona where 
    (Per n1 a1) == (Per n2 a2) = a1 == a2 && n1 == n2
    
instance Ord Persona where
    (Per n1 a1) <= (Per n2 a2) = a1 < a2 || (a1 == a2 && n1 <= n2)

dirJon = (Per "Jon" "Legorburu", Calle "Str" 2, "Galdkao") 

dirMiren = (Per "Jon" "Legorburu", Casa "casa", "Galdkao")

escribir :: [Direccion] -> IO()
escribir xs = putStr (concat (map (\(p,d,c) -> show p ++ "\n" ++ show d ++ "\n" ++ c ++ "\n\n") xs))

diagonal :: Int -> Char -> [String]
diagonal n c = [replicate i '-' ++ [c] ++ replicate (n-i-1) '-' | i <- [0..n-1]]

-- Ejercicio 2
data ArGen a = N a [ArGen a]
ag1 = N 25 [N 35 [], N 45 [], N 55 [] ]
ag2 = N 20 [ N 12 [], ag1 , N 36 [N 52 []]]


preorder :: Ord a => ArGen a -> [a]
preorder (N raiz l) =  raiz: concatMap preorder l

postorder :: Ord a => ArGen a -> [a]
postorder (N raiz l) =  concatMap postorder l ++ [raiz]

esta' :: Eq a => a -> ArGen a -> Bool
esta' a (N raiz subArboles) = or( (a == raiz): map (esta' a) subArboles)

-- Ejercicio 3

data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b)

type ArtimeticExpr = Arbol Int String

exp1 = Nodo (Nodo (Hoja 9) "-" (Nodo (Hoja 10) "+" (Hoja 6))) "+" (Nodo (Hoja 3) "*" (Hoja 5))

instance (Show a, Show b) => Show (Arbol a b) where
    show = unlines . order
     
order :: (Show a, Show b) => Arbol a b -> [String]
order (Hoja a) = [show a]
order (Nodo ai b ad) = map (tab ++) (order ad)
                         ++ [show b] 
                      ++ map (tab ++) (order ai)
            where tab = "    "
                    
-- Ejercicio 4
data Arbin a = Hoja2 a | Unir (Arbin a) (Arbin a)

maparbin :: (a -> b) -> Arbin a -> Arbin b
maparbin f (Hoja2 x) = Hoja2 (f x)
maparbin f (Unir iz de) = Unir (maparbin f iz) (maparbin f de)

aplanar2 :: Arbin a -> [a]
aplanar2 (Hoja2 x) = [x]
aplanar2 (Unir iz de) = aplanar2 iz ++ aplanar2 de


--- Ejercicio Apuntes

data Arbus a = Vac | Nod (Arbus a) a (Arbus a)
a1 = Nod (Nod Vac 1 Vac) 3 (Nod Vac 5 Vac)
a2 = Nod (Nod Vac 7 Vac) 8 (Nod Vac 10 Vac)
a = Nod a1 6 a2

instance (Show a) => Show (Arbus a) where
    show Vac = ""
    show (Nod ai r ad) = show ai ++ " " ++ show r ++ " " ++ show ad ++ " \n"



aplanar :: Arbus a -> [a]
aplanar Vac = []
aplanar (Nod ai a ad) = aplanar ai ++ [a] ++ aplanar ad


estaOrd :: Ord a => Arbus a -> Bool 
estaOrd = ordenada . aplanar

ordenada:: Ord a => [a] -> Bool
ordenada (x:xs) 
    | null xs = True
    | x <= head xs = ordenada xs
    | otherwise = False

borrar :: Ord a => a -> Arbus a -> Arbus a
borrar x Vac = Vac
borrar x (Nod ai r ad) | x < r   = Nod (borrar x ai) r ad
                       | x == r  = une ai ad
                       | x > r   = Nod ai r (borrar x ad)

esta :: Ord a => a -> Arbus a -> Bool
esta x Vac = False
esta x (Nod ai r ad) | x < r = esta x ai
                     | x == r = True
                     | x > r = esta x ad

meter :: Ord a => a -> Arbus a -> Arbus a
meter x Vac = Nod Vac x Vac
meter x (Nod ai r ad) | x < r  = Nod (meter x ai) r ad
                      | x == r = Nod ai r ad
                      | x > r  = Nod ai r (meter x ad)
                      
une :: Arbus a -> Arbus a -> Arbus a
une Vac ad = ad
une ai ad = Nod ai' r ad 
    where (r, ai') = borrarMayor ai

borrarMayor :: Arbus a -> (a ,Arbus a)
borrarMayor (Nod ai r Vac) = (r, ai)
borrarMayor (Nod ai r ad)  = (r', Nod ai r ad')
    where (r', ad') = borrarMayor ad
          