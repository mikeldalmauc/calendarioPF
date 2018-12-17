
-- Nombre(s): Mikel Dalmau y Julen Fernandino


--  SUPERMERCADO         Programacion Funcional            curso 2018/19  --
module Supermercado (BaseDatos (..), Producto (..), Codigo, Precio, defaultBD, eliminar, insertar, cambiarNombre, cambiarPrecio, consultarNombre, consultarPrecio, imprimir, imprimirPorNombre, imprimirPorPrecio)  where

import Data.List (sortOn)

--  Tipos de datos  ------------------------------------------------

type Codigo = Int         --   Codigo de un producto
type Nombre = String      --   Nombre de un producto
type Precio = Float       --   Precio de un producto (2 decimales)

-- tipo Producto (escrito con sintaxis de registro):

data Producto = Prod {codigo ::Codigo, nombre ::Nombre, precio ::Precio}

instance Show Producto where
    show Prod{..} = show codigo ++ lineaBlancos numBlancos ++ nombre ++ " " ++ lineaPuntos numPuntos ++ " " ++ show precio
        where
            numBlancos  = anchuraImprimirCod - length (show codigo)
            numPuntos   = anchuraImprimirNombre - length nombre

newtype BaseDatos = BD [Producto]

--  Base de datos para pruebas  ---------------------

defaultBD :: IO BaseDatos
defaultBD = return (BD [Prod 1111 "Chupa Chups" 0.21,             
            Prod 1112 "Chupa Chups (bolsa gigante)" 1.33,
            Prod 1234 "Tio Pepe, 1lt" 5.40,
            Prod 3814 "Cacahuetes" 0.56,
            Prod 4719 "Fritos de maiz" 1.21,
            Prod 5643 "Dodotis" 10.10] )
                          
--------------------------------------------------------------------------------------
-- Variables definidas para imprimir en el terminal de manera estructurada
--------------------------------------------------------------------------------------

anchuraImprimirCod :: Int
-- Variable que alberga la anchura de la columna donde se imprime el código
anchuraImprimirCod = 8

anchuraImprimirNombre :: Int
-- Variable que alberga la anchura de la columna donde se imprime el nombre
anchuraImprimirNombre = 40

-- anchuraImprimirPrecio :: Int
-- -- Variable que alberga la anchura de la columna donde se imprime el precio
-- anchuraImprimirPrecio = 8

--------------------------------------------------------------------------------------
-- Funciones principales sobre la BaseDatos
--------------------------------------------------------------------------------------

eliminar :: Codigo -> BaseDatos -> BaseDatos
-- eliminar cod bd = la base de datos obtenida al eliminar
--                   el producto de codigo cod de bd.
--                   Si no hay tal codigo en bd da error
eliminar cod  (BD xs) = 
        case buscar cod xs of 
            (Just index,_) -> 
                let 
                    (as, _:bs) = splitAt index xs
                in 
                    BD (as ++ bs)    
            (_,_) -> error ("No existe ningun producto con el codigo: " ++ show cod ++ "#")


insertar :: Producto -> BaseDatos -> BaseDatos
-- insertar Prod{..} (BD xs)    = la base de datos obtenida al insertar
--                              el producto en la Base de datos bd.
--                              Si el código es negativo, se le asignara
--                              un código nuevo (el código más alto + 1).
insertar Prod{..} (BD xs) = 
    if codigo < 0 then 
        BD (xs ++ [Prod{codigo = proximoCodigo xs, precio = pre,..}]) 
    else 
        case buscar codigo xs of 
            (Nothing,_)     -> BD (insertarOrdenado Prod{precio = pre,..} xs)
            (Just index,_)  -> 
                let 
                    (as, _:bs) = splitAt index xs
                in 
                    BD (as ++ Prod{precio = pre,..}:bs)
    where pre = validatePrice precio

--------------------------------------------------------------------------------------
-- Funciones basicas sobre productos:
--------------------------------------------------------------------------------------

cambiarNombre :: Nombre -> Codigo -> BaseDatos -> BaseDatos
-- cambiarNombre nom cod bd = la base de datos obtenida al cambiar
--                          el nombre por nom del producto de código cod 
--                          en la Base de datos xs.
--                          Si no existe el producto da error
cambiarNombre nom cod (BD xs) =
    case buscar cod xs of
        (_,Just Prod{..}) -> insertar Prod{nombre=nom,..} (BD xs) 
        (_,_)     -> error ("No existe ningun producto con el codigo: " ++ show cod ++ "#")

cambiarPrecio :: Precio -> Codigo -> BaseDatos -> BaseDatos
-- cambiarPrecio pre cod bd = la base de datos obtenida al cambiar
--                          el precio por pre del producto de código cod 
--                          en la Base de datos bd.
--                          Si no existe el producto da error
cambiarPrecio pre cod (BD xs) =
    case buscar cod xs of
        (_,Just Prod{..}) -> insertar Prod{precio=pre,..} (BD xs) 
        (_,_)     -> error ("No existe ningun producto con el codigo: " ++ show cod ++ "#")
        
consultarNombre :: Codigo -> BaseDatos -> Nombre
-- consultarNombre cod bd = el nombre del producto de código cod en la base de datos bd
--                          Si no existe el producto da error
consultarNombre cod (BD xs) =
    case buscar cod xs of
        (_,Just p) -> nombre p
        (_,_)     -> error ("No existe ningun producto con el codigo: " ++ show cod ++ "#")

consultarPrecio :: Codigo -> BaseDatos -> Precio
-- consultarPrecio cod bd = el precio del producto de código cod en la base de datos bd
--                          Si no existe el producto da error
consultarPrecio cod (BD xs) =
    case buscar cod xs of
        (_,Just p) -> precio p
        (_,_)     -> error ("No existe ningun producto con el codigo: " ++ show cod ++ "#")

--------------------------------------------------------------------------------------
-- Funciones auxiliares sobre [Producto]:
--------------------------------------------------------------------------------------

proximoCodigo :: [Producto] -> Codigo
-- devuelve un código de producto resultante de sumarle 1 al
-- código del ultimo producto de la lista.
proximoCodigo = (+1) . codigo . last

buscar :: Codigo -> [Producto] -> (Maybe Int, Maybe Producto)
-- buscar codigo [Producto] = Si existe, devuelve la posición en la lista
-- del producto con dicho código
buscar _ []       = (Nothing,Nothing)
buscar n xs 
    | codigo e == n = (Just index, Just e)
    | codigo e < n  = case buscar n bs of
                        (Just i, p) -> (Just (i+index+1),p)
                        (Nothing,_) -> (Nothing, Nothing)
                        
    | otherwise     = buscar n as
    where 
        index       = length xs `quot` 2
        (as, e:bs)  = splitAt index xs 
    
insertarOrdenado :: Producto -> [Producto] -> [Producto]
-- insertarOrdenado prod (x:xs) = lista de productos obtenida al insertar
--                              el producto prod en la lista (x:xs) de manera ordenada
insertarOrdenado prod []        = [prod]
insertarOrdenado prod (x:xs)    
    | codigo prod <= codigo x   = prod : x : xs
    | otherwise                 = x : insertarOrdenado prod xs


ordenarPorNombre :: [Producto] -> [Producto]
-- Dada una lista de productos, la ordena de menor a mayor dependiendo del nombre del producto
ordenarPorNombre [] = []
ordenarPorNombre (x:xs)  = ordenarPorNombre menor ++ [x] ++ ordenarPorNombre mayor
    where 
        menor   = [lis | lis <- xs, nombre x > nombre lis]
        mayor   = [lis | lis <- xs, nombre x <= nombre lis]

ordenarPorPrecio :: [Producto] -> [Producto]
ordenarPorPrecio = sortOn precio

--------------------------------------------------------------------------------------
--  Visualizaciones de la Base de Datos
--------------------------------------------------------------------------------------

-- Visualización de la Base de Datos ordenada por Codigo de productos:
imprimir :: BaseDatos -> IO()
imprimir (BD l) = do
    putStrLn ("\n" ++ s1 ++ lineaBlancos numBlancosCod ++ s2 ++ lineaBlancos numBlancos ++ s3)
    (putStr . concatMap (\p -> show p ++"\n")) l
    putStrLn "** Fin de la Base de datos **"
    where 
        s1              = "Código"
        s2              = "Nombre producto"
        s3              = "Precio"
        numBlancos      = anchuraImprimirNombre - length s2
        numBlancosCod   = anchuraImprimirCod - length s1
        
imprimirPorNombre :: BaseDatos -> IO()
-- Visualización de la Base de Datos ordenada por Nombre de productos:
imprimirPorNombre (BD l) = imprimir (BD (ordenarPorNombre l))

imprimirPorPrecio :: BaseDatos -> IO()
-- Visualización de la Base de Datos ordenada por precio de productos:
imprimirPorPrecio (BD l) = imprimir (BD (ordenarPorPrecio l))

lineaPuntos :: Int -> String
-- devuelve un string con n número de puntos
lineaPuntos n = replicate n '.'

lineaBlancos :: Int -> String
-- devuelve un string con n número de espacios blancos
lineaBlancos n = replicate n ' '

validatePrice :: Float -> Float
-- Redondea un precio a 2 decimales
validatePrice p = truncate' p 2

truncate' ::  Float -> Int -> Float
-- Redondea un valor a número de decimales definidos
truncate' x n = fromIntegral (ceiling (x * t)) / t
    where t = 10^n