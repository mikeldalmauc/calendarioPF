 module PruebaConj where

   import Conjunto2   -- luego lo cambiaremos por Conjunto2

   c1,c2,c3,c4,c5,c6,c7,c8 :: Conj Char

-- -- Los resultados que se imprimen han de ser conjuntos ( decir 
-- --  colecci�n de elementos no repetidos y sin importar el orden)

   c1 = hacerConj ['a','b','c','a']       -- {'a','b','c'}
   c2 = hacerConj ['d','e','a','c']       -- {'d','e','a','c'}
   c3 = c1 `union` c2                       -- {'a','b','c','d','e'}
   c4 = inter c1 c2                       -- {'a','c'}
   c5 = dif c1 c2                         -- {'b'}
   c6 = dif c2 c1                         -- {'d','e'}
   c7 = simple 'y'                        -- {'y'}
   c8 = inter vacio c5                    -- {}

   b1,b2,b3,b4 :: Bool
   b1 = miembro 'a' c1                    -- True
   b2 = miembro 'd' c5                    -- False
   b3 = subConj c4 c3                     -- True
   b4 = subConj c6 vacio                  -- False

   n1, n2 :: Int
   n1 = card c1                           -- 3
   n2 = card (inter c5 c6)                -- 0
   ------------------------------------------------------------------

   e1,e2,e3,e4,e5,e6 :: Conj Int

   e1 = hacerConj [3,2,7,1,4,7,2]              -- {3,2,7,1,4}
   e2 = hacerConj [1,2,4,6,7]                  -- {1,2,4,6,7}

   e3 = mapConj (*2) e1                        -- {6,4,14,2,8}
   e4 = mapConj f e1
         where f x = if x<3 then x+1 else x
                                               -- {3,7,2,4}
   
   e5 = filterConj par (e1 `union` e2)             
             where par x = mod x 2 == 0      -- {2,4,6}

   e6 = filterConj (>5) (e1 `union` e2)          -- {7,6}

   sumaConj :: Conj Int -> Int
   sumaConj = foldConj (+) 0

   s1 = sumaConj e1                            -- 17
   s2 = sumaConj (hacerConj [3,3,4])           -- 7

-- --------------------------------------------------------------------
