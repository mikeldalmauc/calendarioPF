
-- Nombre(s): 


--  SUPERMERCADO         Programacion Funcional            curso 2018/19  --


module Supermercado where
-- ( -- lista de exportacion con 
--                       -- los tipos y funciones adecuadas
--                      )
                      

--  Tipos de datos  ------------------------------------------------

type Codigo = Int         --   Codigo de un producto
type Nombre = String      --   Nombre de un producto
type Precio = Float       --   Precio de un producto (2 decimales)

-- tipo Producto (escrito con sintaxis de registro):

data Producto = Prod {codigo ::Codigo, nombre ::Nombre, precio ::Precio}

instance Show Producto where
    show Prod {..} = show codigo ++ " " ++ nombre ++ " " ++ show precio 

newtype BaseDatos = BD [Producto]

-- los productos de la base de datos deben estar en orden creciente por codigo


--  Base de datos inicial para pruebas  ---------------------

miBD :: BaseDatos
miBD =  BD [Prod 1111 "Chupa Chups" 0.21,             
            Prod 1112 "Chupa Chups (bolsa gigante)" 1.33,
            Prod 1234 "Tio Pepe, 1lt" 5.40,
            Prod 3814 "Cacahuetes" 0.56,
            Prod 4719 "Fritos de maiz" 1.21,
            Prod 5643 "Dodotis" 10.10]                            
-------------------------------------------------------------  


-- Funciones basicas sobre productos:



-- Funciones principales sobre la BaseDatos

-- Un ejemplo es:

eliminar :: Codigo -> BaseDatos -> BaseDatos
-- eliminar cod bd = la base de datos obtenida al eliminar
--                   el producto de codigo cod de bd.
--                   Si no hay tal codigo en bd da error
eliminar cod  (BD xs) = 
        case posicion cod xs of 
            Nothing -> error ("No existe ninún producto con el código: " ++ show cod)
            Just index  -> 
                let 
                    (as, _:bs) = splitAt index xs
                in 
                    BD (as ++ bs)

insertar :: Producto -> BaseDatos -> BaseDatos
insertar Prod{..} (BD xs) = 

    -- if codigo == undefined then 
    --     -- let 
    --     --     codigo = (1+) . 
    --     -- in
    --         BD (xs ++ [cambiarCodigo Prod{..} (codigo (last xs))]) 
    -- else 
        case posicion codigo xs of 
            Nothing -> BD xs -- TODO hace esto para que se inserte en una posicion concreta
            Just index  -> 
                let 
                    (as, _:bs) = splitAt index xs
                in 
                    BD (as ++ Prod{..}:bs)

-- Esta a lo mejor hay que cambiar/borrar
cambiarCodigo :: Producto -> Codigo -> Producto
cambiarCodigo Prod{..} cod = Prod{codigo = cod,..}

-- Funciones auxiliares sobre [Producto]:

posicion :: Codigo -> [Producto] -> Maybe Int
-- posicion codigo [Producto] = Si existe, devuelve la posición en la lista
-- del producto con dicho código
posicion _ [] = Nothing
posicion n xs 
    | codigo e == n = Just index
    | codigo e < n = (+index) . (+1) <$> posicion n bs
    | otherwise = posicion n as
    where index = length xs `quot` 2
          (as, e:bs) = splitAt index xs 
    
-- insertarOrdendo :: Codigo -> 
--  Visualizacion de la Base de Datos -----------------------

imprimir :: BaseDatos -> IO()
imprimir (BD l) = do
                    putStr "Cabecera \n"
                    (putStr . concat . map (\p -> show p ++"\n")) l
                    
                -- Visualizaci�n de la Base de Datos ordenada por Nombre de productos:

-- imprimirPorNombre :: BaseDatos -> IO()





