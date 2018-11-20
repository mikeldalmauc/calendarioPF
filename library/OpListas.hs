module OpListas (quitarUno, eliminarDuplicados, ordenar) where

 quitarUno :: Eq a => a -> [a] -> [a]
 -- dados un elemento y una lista, devuelve la lista eliminando un repeticion de este
 quitarUno _ [] = []
 quitarUno a (x:xs)  |  x == a = xs
                     | otherwise = x:quitarUno a xs

 eliminarDuplicados :: Eq a =>  [a] -> [a]
 -- dada una lista, elimina todos los elementos duplicados
 eliminarDuplicados [] = []
 eliminarDuplicados (x:xs) =  x : eliminarDuplicados (filter (x/=) xs) 

 ordenar :: Ord a => [a] -> [a]
 -- dada una lista, la ordena de menor a mayor
 ordenar [] = []
 ordenar (x:xs) = ordenar menores ++ [x] ++ ordenar mayores
    where 
        menores = filter (< x) xs
        mayores = filter (>= x) xs