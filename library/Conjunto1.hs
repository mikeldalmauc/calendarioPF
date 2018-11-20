module Conjunto1 (Conj, vacio, simple, miembro, union, 
                    inter, dif, card, subConj, hacerConj, mapConj, filterConj, foldConj) where 
    
    import OpListas (quitarUno)
    
    newtype Conj a = Co [a]
                        
    instance (Eq a) => Eq (Conj a) where 
        x == y = 
            let 
                (Co dx) = x `dif` y
                (Co dy) = y `dif` x
            in
                dx == dy
                
    instance (Show a) => Show (Conj a) where
        show (Co []) = "{}"
        show (Co x) = "{" ++ concatMap (\a -> show a ++ ",") (init x) ++ show (last x) ++ "}"

    vacio :: Conj a
    -- Devuelve el conjunto vacío de la clase Conj
    vacio = Co []

    simple :: a -> Conj a 
    -- Devuelve el conjunto formado por un solo elemento
    simple a = Co [a]
    
    miembro :: Eq a => a -> Conj a -> Bool
    -- Determina si el elemento a pertenece al conjunto x
    miembro _ (Co [])   = False
    miembro a (Co (x:s))    |  x == a   = True
                            | otherwise  = miembro a (Co s)

    union, inter, dif :: Eq a =>  Conj a -> Conj a -> Conj a
    -- union : Dados dos conjuntos devuelve su unión
    union (Co x) (Co y) = Co (x ++ y)
    
    -- inter : Dados dos conjuntos devuelve su interseccion
    inter x y = x `dif` (x `dif` y)
    
    -- dif x y : devuelve el conjunto x menos el conjunto y
    dif x (Co []) = x
    dif (Co x) (Co (y:s)) = dif (Co (quitarUno y x)) (Co s) 
                    
    card :: Conj a -> Int  
    -- Indica la cardinalidad del conjunto
    card (Co l) = length l
    
    subConj :: Eq a => Conj a -> Conj a -> Bool
    -- subConj x y decide si x es subconjunto de y
    subConj x y = dif x y == vacio 
    
    hacerConj :: [a] -> Conj a
    -- Crea un conjunto a partir de una lista
    hacerConj = Co
    
    mapConj :: (a -> b) -> Conj a -> Conj b
    -- Aplica la función f a cada elemento del conjunto
    mapConj f (Co x) = Co (map f x)
    
    filterConj :: (a->Bool) -> Conj a -> Conj a
    -- Devuelve el conjunto resultado de filtrar segun el predicado p
    filterConj p (Co x) = Co (filter p x)
    
    foldConj :: (a -> b -> b) -> b -> Conj a -> b
    -- Devuelve el valor de aplicar foldr al conjunto
    foldConj f b (Co x) = foldr f b x