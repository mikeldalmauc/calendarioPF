
-- Nombre(s): Mikel Dalmau y Julen Fernandino


--  SUPERMERCADO         Programacion Funcional            curso 2018/19  --

-- TODO: REVISAR SI LOS EXPORTADO ES ADECUADO
-- lista de exportacion con los tipos y funciones adecuadas

-- (BaseDatos, Producto, eliminar, insertar, cambiarNombre, cambiarPrecio, imprimir, imprimirPorNombre) 
module Supermercado where
                      

--  Tipos de datos  ------------------------------------------------

type Codigo = Int         --   Codigo de un producto
type Nombre = String      --   Nombre de un producto
type Precio = Float       --   Precio de un producto (2 decimales)

-- tipo Producto (escrito con sintaxis de registro):

data Producto = Prod {codigo ::Codigo, nombre ::Nombre, precio ::Precio}

instance Show Producto where
    show Prod {..} = show codigo ++ lineaBlancos numBlancos ++ nombre ++ " " ++ lineaPuntos numPuntos ++ " " ++ show precio
        where
            numBlancos  = anchuraImprimirCod - (length (show codigo))
            numPuntos   = anchuraImprimir - (length (show nombre)) - (length (show precio))

data BaseDatos = BD [Producto]

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



-- Variables definidas para imprimir en el terminal de manera estructurada -----------------------

anchuraImprimirCod :: Int
-- Variable que alberga la anchura de la columna donde se imprime el código
anchuraImprimirCod = 8

anchuraImprimir :: Int
-- Variable que alberga la anchura de la columna donde se imprime el resto de la información
anchuraImprimir = 40

------------------------------------------------------------- ------------------------------------  


-- Funciones basicas sobre productos:



-- Funciones principales sobre la BaseDatos

-- Un ejemplo es:

eliminar :: Codigo -> BaseDatos -> BaseDatos
-- eliminar cod bd = la base de datos obtenida al eliminar
--                   el producto de codigo cod de bd.
--                   Si no hay tal codigo en bd da error
eliminar cod  (BD xs) = 
        case posicion cod xs of 
            Nothing     -> error ("No existe ninún producto con el código: " ++ show cod)
            Just index  -> 
                let 
                    (as, _:bs) = splitAt index xs
                in 
                    BD (as ++ bs)    


insertar :: Producto -> BaseDatos -> BaseDatos
-- insertar Prod{..} (BD xs)    = la base de datos obtenida al insertar
--                              el producto en la Base de datos bd.
--                              Si el código es negativo, se le asignara
--                              un código nuevo (el código más alto + 1).
insertar Prod{..} (BD xs) = 
    if codigo < 0 then 
        BD (xs ++ [Prod{codigo = proximoCodigo xs,..}]) 
    else 
        case posicion codigo xs of 
            Nothing     -> BD (insertarOrdenado Prod{..} xs)
            Just index  -> 
                let 
                    (as, _:bs) = splitAt index xs
                in 
                    BD (as ++ Prod{..}:bs)


cambiarNombre :: Nombre -> Codigo -> BaseDatos -> BaseDatos
-- cambiarNombre nom cod xs = la base de datos obtenida al cambiar
--                          el nombre por nom del producto de código cod 
--                          en la Base de datos xs.
--                          Si no hay tal codigo en xs da error
cambiarNombre nom cod (BD xs) =
    case posicion cod xs of
        Nothing     -> error ("No existe ningún producto con el código: " ++ show cod)
        Just index  -> 
            let 
                (as, x:bs) = splitAt index xs
                pre = precio x
            in 
                BD (as ++ (Prod cod nom pre):bs)

cambiarPrecio :: Precio -> Codigo -> BaseDatos -> BaseDatos
-- cambiarPrecio pre cod xs = la base de datos obtenida al cambiar
--                          el precio por pre del producto de código cod 
--                          en la Base de datos xs.
--                          Si no hay tal codigo en xs da error
cambiarPrecio pre cod (BD xs) =
    case posicion cod xs of
        Nothing     -> error ("No existe ningún producto con el código: " ++ show cod)
        Just index  -> 
            let 
                (as, x:bs) = splitAt index xs
                nom = nombre x
            in 
                BD (as ++ (Prod cod nom pre):bs)

--------------------------------------------------------------------------------------


-- Funciones auxiliares sobre [Producto]:

proximoCodigo :: [Producto] -> Codigo
-- devuelve un código de producto resultante de sumarle 1 al
-- código del ultimo producto de la lista.
proximoCodigo = (+1) . codigo . last

posicion :: Codigo -> [Producto] -> Maybe Int
-- posicion codigo [Producto] = Si existe, devuelve la posición en la lista
-- del producto con dicho código
posicion _ []       = Nothing
posicion n xs 
    | codigo e == n = Just index
    | codigo e < n  = (+index) . (+1) <$> posicion n bs
    | otherwise     = posicion n as
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


--  Visualizacion de la Base de Datos -----------------------

imprimir :: BaseDatos -> IO()
-- Visualización de la Base de Datos ordenada por Codigo de productos:
imprimir (BD l) = do
    putStrLn ("\n" ++ s1 ++ lineaBlancos numBlancosCod ++ s2 ++ lineaBlancos numBlancos ++ s3)
    (putStr . concatMap (\p -> show p ++"\n")) l
    putStrLn ("** Fin de la Base de datos **")
        where 
            s1              = "Código"
            s2              = "Nombre producto"
            s3              = "Precio"
            numBlancos      = anchuraImprimir - length s2 - length s3
            numBlancosCod   = anchuraImprimirCod - length s1

imprimirPorNombre :: BaseDatos -> IO()
-- Visualización de la Base de Datos ordenada por Nombre de productos:
imprimirPorNombre (BD l) = imprimir (BD (ordenarPorNombre l))


lineaPuntos :: Int -> String
-- devuelve un string con n número de puntos
lineaPuntos n = replicate n '.'

lineaBlancos :: Int -> String
-- devuelve un string con n número de espacios blancos
lineaBlancos n = replicate n ' '




