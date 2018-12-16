------------------------------------------------------------------------
-- Nombre(s) : Mikel Dalmau y Julen Fernandino
-- ¿Tratas los errores referentes a la clave del producto? 
------------------------------------------------------------------------


-- SUPERMERCADO INTERACTIVO   Programacion Funcional  curso 2018/19 ----


module SuperInteractivo where

-- Importación de modulos:

import Data.List (find,minimumBy)
import Data.Ord (comparing)
import Text.Read
import Prelude hiding (catch)
import Control.Exception
import Supermercado

-- Otros tipos de datos

type Archivo = String
type Orden = String

pathBD :: FilePath
pathBD = "C:/Users/Mikel/Google Drive/Trayectoria Profesional/Grado en Ing Informática/Cuarto/PF/practicaCalendario-PF/calendar/library/productos.txt"

-----------------------------------------------------------------------------------------------------------------------
--  Salvaguarda y recuperacion de la Base de datos  
-----------------------------------------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------------------------------------
--- Interaccion con el supermercado
-----------------------------------------------------------------------------------------------------------------------

-- Proceso de inicializacion:
-- Espera que la Base de Datos del supermercado se encuentre 
-- en el archivo "productos.txt".
-- Muestra el menu de opciones, recupera la Base de Datos, e inicia una sesion.
supermercado :: IO ()
supermercado = do
               putStrLn bienvenida
               putStrLn menu
               bd <- recuperaBD pathBD
               sesionCon $! bd

-----------------------------------------------------------------------------------------------------------------------
--- Proceso principal
-----------------------------------------------------------------------------------------------------------------------

sesionCon :: BaseDatos -> IO()
-- El proceso principal se llama recursivamente, y consiste en la lectura y ejecución de un comando
-- en cada llamada. Con cada ejecución de un comando se obtiene una base de datos actualizada y se utiliza
-- para la siguiente llamada. En caso contrario, se guarda la base de datos y se termina sesión.
sesionCon bd =  do
                putStr "\nsupermercado> "
                comando <- getLine
                res <- ejecutaCon bd comando
                case res of
                    Just bd'   -> sesionCon bd'
                    Nothing    -> guardaBD bd pathBD
            

ejecutaCon :: BaseDatos -> Orden -> IO (Maybe BaseDatos)
-- ejecutaCon bd orden = devuelve la base de datos resultante de ejecutar el comando indicado por 'orden' 
-- en caso de no existir se devuelve la base de datos original.
--      
--      Esta función captura cualquier excepción posible y la trata mostrando su mensaje y conservando la base de 
--      datos original.
-- 
ejecutaCon bd orden = catch 
                        (case find (\Com{..} -> nombre == orden) comandos of
                                Just Com{..} -> if nombre == "fin" then 
                                                    return Nothing 
                                                else return <$> funcion bd
                                Nothing ->  do
                                            putStrLn (sugerirComando orden ++ "El comando introducido no exite, prueba 'ayuda' para ver los comando disponibles.")
                                            return (Just bd) 
                        ) handler
                    where
                        handler :: SomeException -> IO (Maybe BaseDatos)
                        handler ex = do
                                    putStrLn (takeWhile (/= '#') (show ex))
                                    return (Just bd)

-----------------------------------------------------------------------------------------------------------------------
--- Comandos del usuario
-----------------------------------------------------------------------------------------------------------------------

data Comando = Com {nombre ::String, descripcion ::String, funcion :: BaseDatos -> IO BaseDatos}

comandos :: [Comando]
comandos = [    Com "ayuda" "muestra esta ayuda" ayuda,
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

ayuda :: BaseDatos -> IO BaseDatos
ayuda bd =  do
            putStrLn menu
            return bd

conPre :: BaseDatos -> IO BaseDatos
conPre bd = do
            putStr "Codigo? "
            cod <- leerNumero
            print (consultarPrecio cod bd)
            return bd

conNom :: BaseDatos -> IO BaseDatos
conNom bd = do
            putStr "Codigo? "
            cod <- leerNumero
            print (consultarNombre cod bd)
            return bd

camPre :: BaseDatos -> IO BaseDatos
camPre bd = do 
            putStr "Codigo? "
            cod <- leerNumero
            putStrLn (("Precio Actual: " ++ ) `seq` show (consultarPrecio cod bd))
            putStr "Nuevo Precio? "
            pre <- leerFloat
            return (cambiarPrecio pre cod bd)

camNom :: BaseDatos -> IO BaseDatos
camNom bd = do 
            putStr "Codigo? "
            cod <- leerNumero
            putStrLn (("Nombre Actual: " ++) `seq` show (consultarNombre cod bd))
            putStr "Nuevo Nombre? "
            nombre <- getLine
            return (cambiarNombre nombre cod bd)

metPro :: BaseDatos -> IO BaseDatos
metPro bd = do 
            putStr "Codigo? "
            cod <- leerNumero
            putStr "Nombre? "
            nom <- getLine
            putStr "Precio? "
            pre <- leerFloat
            return (insertar (Prod cod nom pre) bd)

eliPro :: BaseDatos -> IO BaseDatos
eliPro bd =  do 
            putStr "Codigo? "
            cod <- leerNumero
            return (eliminar cod bd)

mosBD :: BaseDatos -> IO BaseDatos
mosBD bd = do
        putStrLn "Base de datos actual:"
        imprimir bd
        return bd

mosBDnombre :: BaseDatos -> IO BaseDatos
mosBDnombre bd = do
        putStrLn "Base de datos actual:"
        imprimirPorNombre bd
        return bd

mosBDprecio :: BaseDatos -> IO BaseDatos
mosBDprecio bd = do
        putStrLn "Base de datos actual:"
        imprimirPorPrecio bd
        return bd

fin :: BaseDatos -> IO BaseDatos
fin = return 

-----------------------------------------------------------------------------------------------------------------------
--- Funciones auxiliares
-----------------------------------------------------------------------------------------------------------------------

menu :: String
menu = "\n Comandos Disponibles:\n" ++ concatMap (\Com{..} -> nombre ++": " ++ descripcion ++"\n") comandos
        
sugerirComando :: String -> String
-- Sugiere un comando parecido al introducido en caso de que este no esté entre los comandos disponibles
sugerirComando s = elegirMinimo(zip (map (\Com{..} -> hammingDistance s nombre) comandos) comandos)

elegirMinimo :: [(Int, Comando)] -> String
-- elegirMinimo (a,b) = nombre del comando de menor a o el string vacío si no existe a <= 3
elegirMinimo l = if val > 3 then "" else "Tal vez quisiste decir " ++ SuperInteractivo.nombre com ++ "\n"
        where (val, com) = minimumBy (comparing fst) l

hammingDistance :: Eq a => [a] -> [a] -> Int
-- Defina la diferencia entre dos strings
hammingDistance (x:xs) (y:ys) | x /= y = tl + 1
                              | otherwise = tl
    where tl = hammingDistance xs ys
hammingDistance x [] = length x
hammingDistance [] y = length y

leerNumero :: IO Int
-- Lee un entero de la consolo o lanza un error si no es un número
leerNumero = do 
                cod <- readMaybe <$> getLine
                case cod of
                    Nothing -> error "El valor introducido tiene que ser un numero valido.#"
                    Just val -> return val

leerFloat :: IO Float
-- Lee un float de la consolo o lanza un error si no es un número
leerFloat = do 
                cod <- readMaybe <$> getLine
                case cod of
                    Nothing -> error "El valor introducido tiene que ser un numero valido.#"
                    Just val -> return val

bienvenida :: String
bienvenida =  unlines [" _______   __                                                    __        __           "
                ,"|       \\ |  \\                                                  |  \\      |  \\          "
               ,"| $$$$$$$\\ \\$$  ______   _______  __     __   ______   _______   \\$$  ____| $$  ______  "
                ,"| $$__/ $$|  \\ /      \\ |       \\|  \\   /  \\ /      \\ |       \\ |  \\ /      $$ /     \\ "
                ,"| $$    $$| $$|  $$$$$$\\| $$$$$$$\\\\$$\\ /  $$|  $$$$$$\\| $$$$$$$\\| $$|  $$$$$$$|  $$$$$$\\"
                ,"| $$$$$$$\\| $$| $$    $$| $$  | $$ \\$$\\  $$ | $$    $$| $$  | $$| $$| $$  | $$| $$  | $$"
                ,"| $$__/ $$| $$| $$$$$$$$| $$  | $$  \\$$ $$  | $$$$$$$$| $$  | $$| $$| $$__| $$| $$__/ $$"
                ,"| $$    $$| $$ \\$$     \\| $$  | $$   \\$$$    \\$$     \\| $$  | $$| $$ \\$$    $$ \\$$    $$"
                ," \\$$$$$$$  \\$$  \\$$$$$$$ \\$$   \\$$    \\$      \\$$$$$$$ \\$$   \\$$ \\$$  \\$$$$$$$  \\$$$$$$ "]