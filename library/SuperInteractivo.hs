------------------------------------------------------------------------
-- Nombre(s) : Mikel Dalmau y Julen Fernandino
-- ¿Tratas los errores referentes a la clave del producto? 
------------------------------------------------------------------------


-- SUPERMERCADO INTERACTIVO   Programacion Funcional  curso 2018/19 ----


module SuperInteractivo where

-- Importación de modulos:

import Data.List (find,minimumBy)
import Data.Ord (comparing)
import Supermercado

-- Otros tipos de datos

type Archivo = String
type Orden = String


--  Salvaguarda y recuperacion de la Base de datos   -------------------

-- Funcion que guarda la Base de datos de un supermercado en un archivo.
-- Se espera que el archivo está en el directorio desde el que hemos arrancado
guardaBD :: BaseDatos -> Archivo -> IO ()
guardaBD bd archivo = putStrLn "\nBase de datos guarda."

-- Funcion que recupera la BD de un archivo.
-- Se espera que el archivo está en el directorio desde el cual hemos arrancado,
-- y que su contenido haya sido formado por guardaBD.
recuperaBD :: Archivo -> IO BaseDatos
recuperaBD archivo = do                         -- ES LA UNICA MANERA QUE HE CONSEGUIDO QUE NO ME SALTARAN ERRORES.
    file <- lines <$> readFile archivo
    let bd = parseEntry [] file
    return (BD bd)


parseEntry :: [Producto] -> [String] -> [Producto]
parseEntry bd (a:b:c:x) = parseEntry (bd ++ [Prod (read a :: Codigo) b (read c :: Precio)]) x
parseEntry bd _        = bd


--     Interaccion con el supermercado  ----------------------------------------

-- Proceso de inicializacion:
-- Espera que la Base de Datos del supermercado se encuentre 
-- en el archivo "productos.txt".
-- Muestra el menu de opciones, recupera la Base de Datos, e inicia una sesion.


supermercado :: IO ()
supermercado = do
               putStrLn menu
               bd <- recuperaBD "C:/Users/Mikel/Google Drive/Trayectoria Profesional/Grado en Ing Informática/Cuarto/PF/practicaCalendario-PF/calendar/library/productos.txt"
               sesionCon $! bd      -- probar tambien con sesionCon bd

data Comando = Com {nombre ::String, descripcion ::String, funcion :: BaseDatos -> IO (Maybe BaseDatos)}

instance Show Comando where
        show Com{..} = nombre ++": " ++ descripcion

comandos :: [Comando]
comandos = [ Com "ayuda" "muestra esta ayuda" ayuda,
        Com "conPre" "consulta el precio de un producto" conPre,
        Com "conNom" "consulta el nombre de un producto" conNom,
        Com "camPre" "cambia el precio de un producto" camPre,
        Com "camNom" "cambia el nombre de un producto" camNom,
        Com "metPro" "mete un nuevo producto" metPro,
        Com "eliPro" "elimina un producto" eliPro,
        Com "mosBD" "muestra el contenido de la Base de datos" mosBD,
        Com "mosBDnombre" "muestra la BD ordenada por Nombre" mosBDnombre,
        Com "mosBDprecio" "muestra la BD ordenada por Precio" mosBDprecio,
        Com "fin" "termina la sesion, guardando la Base de datos" fin]


menu :: String
menu = concatMap (\c -> show c ++"\n") comandos
        
ayuda :: BaseDatos -> IO (Maybe BaseDatos)
ayuda bd =  do
        putStrLn menu
        return (Just bd)

conPre :: BaseDatos -> IO (Maybe BaseDatos)
conPre bd = do 
        cod <- leerCodigoExistente bd
        print (consultarPrecio cod bd)
        return (Just bd)

conNom :: BaseDatos -> IO (Maybe BaseDatos)
conNom bd = do 
        cod <- leerCodigoExistente bd
        print (consultarNombre cod bd)
        return (Just bd)

camPre :: BaseDatos -> IO (Maybe BaseDatos)
camPre bd = do 
        cod <- leerCodigoExistente bd
        putStrLn ("Precio Actual: " ++ show (consultarPrecio cod bd))
        putStr "Nuevo Precio? "
        precio <- read <$> getLine
        return (Just (cambiarPrecio precio cod bd))

camNom :: BaseDatos -> IO (Maybe BaseDatos)
camNom bd = do 
        cod <- leerCodigoExistente bd
        putStrLn ("Nombre Actual: " ++ show (consultarNombre cod bd))
        putStr "Nuevo Nombre? "
        nombre <- getLine
        return (Just (cambiarNombre nombre cod bd))

metPro :: BaseDatos -> IO (Maybe BaseDatos)
metPro bd = do
        cod <- leerCodigoLibre bd
        putStr "Nombre? "
        nom <- getLine
        putStr "Precio? "
        pre <- read <$> getLine
        return (Just (insertar (Prod cod nom pre) bd))

eliPro :: BaseDatos -> IO (Maybe BaseDatos)
eliPro bd = do
        cod <- leerCodigoExistente bd
        return (Just (eliminar cod bd))

mosBD :: BaseDatos -> IO (Maybe BaseDatos)
mosBD bd = do
        putStrLn "Base de datos actual:"
        imprimir bd
        return (Just bd)

mosBDnombre :: BaseDatos -> IO (Maybe BaseDatos)
mosBDnombre bd = do
        putStrLn "Base de datos actual:"
        imprimirPorNombre bd
        return (Just bd)

mosBDprecio :: BaseDatos -> IO (Maybe BaseDatos)
mosBDprecio bd = do
        putStrLn "Base de datos actual:"
        imprimirPorPrecio bd
        return (Just bd)

fin :: BaseDatos -> IO (Maybe BaseDatos)
fin bd = return Nothing


-- Proceso principal:
-- Depende de una Base de Datos dada como par�metro.
sesionCon :: BaseDatos -> IO()
sesionCon bd = do
        putStr "\nsupermercado> "
        comando <- getLine
        bd' <- ejecutaCon bd comando
        case bd' of
            Just bd''   -> sesionCon bd''
            Nothing     -> guardaBD bd "prod.txt" -- TODO: usar el nombre del archivo adecuado
            

ejecutaCon :: BaseDatos -> Orden -> IO (Maybe BaseDatos)
ejecutaCon bd orden = case find (\Com{..} -> nombre == orden) comandos of
                        Just Com{..} -> funcion bd
                        Nothing -> do
                                putStrLn (sugerirComando orden ++ "El comando introducido no exite, prueba 'ayuda' para ver los comando disponibles.")
                                return (Just bd)


leerCodigoLibre :: BaseDatos -> IO Codigo
leerCodigoLibre bd = do
    putStr "Codigo? "
    cod <- read <$> getLine
    if estaCodigo cod bd
        then do 
            putStrLn "El codigo esta repetido, introduzca otro." 
            leerCodigoLibre bd
        else return cod

leerCodigoExistente :: BaseDatos -> IO Codigo
leerCodigoExistente bd = do
    putStr "Codigo? "
    cod <- read <$> getLine
    if not (estaCodigo cod bd)
        then do 
            putStrLn "El codigo no existe, introduzca otro." 
            leerCodigoExistente bd
        else return cod


-----------------------------------------------------------------------------------------------------------------------
--- Funciones auxiliares
-----------------------------------------------------------------------------------------------------------------------

sugerirComando :: String -> String
sugerirComando s = elegirMinimo(zip (map (\Com{..} -> hammingDistance s nombre) comandos) comandos)

elegirMinimo :: [(Int, Comando)] -> String
elegirMinimo l = if val > 3 then "" else "Tal vez quisiste decir " ++ SuperInteractivo.nombre com ++ "\n"
        where (val, com) = minimumBy (comparing fst) l

hammingDistance :: Eq a => [a] -> [a] -> Int
-- Defina la diferencia entre dos strings
hammingDistance (x:xs) (y:ys) | x /= y = tl + 1
                              | otherwise = tl
    where tl = hammingDistance xs ys
hammingDistance x [] = length x
hammingDistance [] y = length y
hammingDistance [] [] = 0