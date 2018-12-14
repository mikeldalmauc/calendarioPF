------------------------------------------------------------------------
-- Nombre(s) : Mikel Dalmau y Julen Fernandino
-- ¿Tratas los errores referentes a la clave del producto? 
------------------------------------------------------------------------


-- SUPERMERCADO INTERACTIVO   Programacion Funcional  curso 2018/19 ----


module SuperInteractivo where

-- Importación de modulos:

import Supermercado

-- Otros tipos de datos

type Archivo = String
type Orden = String


--  Salvaguarda y recuperacion de la Base de datos   -------------------

-- Funcion que guarda la Base de datos de un supermercado en un archivo.
-- Se espera que el archivo está en el directorio desde el que hemos arrancado
-- guardaBD :: BaseDatos -> Archivo -> IO ()



-- Funcion que recupera la BD de un archivo.
-- Se espera que el archivo est� en el directorio desde el cual hemos arrancado,
-- y que su contenido haya sido formado por guardaBD.
-- recuperaBD :: Archivo -> IO BaseDatos



--     Interaccion con el supermercado  ----------------------------------------

-- Proceso de inicializacion:
-- Espera que la Base de Datos del supermercado se encuentre 
-- en el archivo "productos.txt".
-- Muestra el menu de opciones, recupera la Base de Datos, e inicia una sesion.

{-
supermercado :: IO ()
supermercado = do
               putStrLn menu
               bd <- recuperaBD "productos.txt"
               sesionCon $! bd      -- probar tambien con sesionCon bd
-}
supermercado :: IO ()
supermercado = do
               putStrLn menu
               bd <- miBD
               sesionCon $! bd      -- probar tambien con sesionCon bd

-- Texto del menu de ordenes.
-- Aparece cuando se activa la Base de Datos (mediante supermercado)
-- y tambien al ejecutar la orden "ayuda".

menu :: String
menu = "Supermercado interactivo \n" ++
       "Ordenes disponibles: \n" ++
       "  ayuda: muestra esta ayuda \n" ++
       "  conPre: consulta el precio de un producto \n" ++
       "  conNom: consulta el nombre de un producto \n" ++
       "  camPre: cambia el precio de un producto \n" ++
       "  camNom: cambia el nombre de un producto \n" ++
       "  metPro: mete un nuevo producto \n" ++
       "  eliPro: elimina un producto \n" ++
       "  mosBD: muestra el contenido de la Base de datos  \n" ++
       "  mosBDnombre: muestra la BD ordenada por Nombre \n" ++
       "  fin: termina la sesion, guardando la Base de datos."


-- Proceso principal:
-- Depende de una Base de Datos dada como par�metro.
sesionCon :: BaseDatos -> IO()
sesionCon bd = do
        comando <- getLine
        bd' <- ejecutaCon bd comando
        sesionCon bd'


-- Proceso auxiliar:
-- Depende de una Base de datos y una orden dados como par�metros.
ejecutaCon :: BaseDatos -> Orden -> IO BaseDatos
ejecutaCon bd comando =
        case comando of
            "ayuda" ->  do
                            putStrLn menu
                            return bd
            "conPre" -> do 
                            putStr "Codigo?"
                            cod <- read <$> getLine
                            print (consultarPrecio cod bd)
                            return bd
            "conNom" -> do 
                            putStr "Codigo?"
                            cod <- read <$> getLine
                            print (consultarNombre cod bd)
                            return bd
            "camPre" -> do 
                            putStr "Codigo?"
                            cod <- read <$> getLine
                            putStrLn ("Precio Actual: " ++ show (consultarPrecio cod bd))
                            putStr "Nuevo Precio?"
                            precio <- read <$> getLine
                            return $! cambiarPrecio precio cod bd
            "camNom" -> do 
                            putStr "Codigo?"
                            cod <- read <$> getLine
                            putStrLn ("Nombre Actual: " ++ show (consultarNombre cod bd))
                            putStr "Nuevo Nombre?"
                            nombre <- getLine -- TODO corregir esto para que lea bien los strings
                            return $! cambiarNombre nombre cod bd
    
            "fin"   -> fail "Sesion finalizada" -- Todo cambiar esto
            _ -> do
                    putStrLn "El comando introducido no exite, prueba 'ayuda' para ver los comando disponibles."
                    return bd