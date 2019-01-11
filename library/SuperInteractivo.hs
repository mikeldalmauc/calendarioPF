------------------------------------------------------------------------
-- Nombre(s) : Mikel Dalmau y Julen Fernandino

-- SUPERMERCADO INTERACTIVO   Programacion Funcional  curso 2018/19 ----

------------------------------------------------------------------------

module SuperInteractivo where

-- Importación de modulos:

import Data.List (find,minimumBy,sortOn)
import Data.Ord (comparing)
import Text.Read
import Control.Exception
import Supermercado

-- Otros tipos de datos

type Archivo = String
type Orden = String

data Modelo = Model {bd ::BaseDatos, pathBD ::FilePath , cerrar::Bool}

initModel :: IO Modelo
-- Inicializa el modelo de datos con la base de datos y el path
initModel = do
            path <- bucarRuta
            bd <- recuperaBD path
            return $! Model bd path False

-----------------------------------------------------------------------------------------------------------------------
--  Salvaguarda y recuperacion de la Base de datos  
-----------------------------------------------------------------------------------------------------------------------

-- Funcion que guarda la Base de datos de un supermercado en un archivo.
-- Se espera que el archivo esté en el directorio desde el que hemos arrancado
guardaBD :: BaseDatos -> Archivo -> IO ()
guardaBD (BD xs) archivo = do
            writeFile archivo (unlines (concatMap (\Prod{..} -> [show codigo, nombre, show precio]) (sortOn codigo xs)))
            putStrLn "\nBase de datos guardada."
            
-- Funcion que recupera la BD de un archivo.
-- Se espera que el archivo está en el directorio desde el cual hemos arrancado,
-- y que su contenido haya sido formado por guardaBD.
recuperaBD :: Archivo -> IO BaseDatos
recuperaBD archivo = catch (do                         
                            file <- lines <$> readFile archivo
                            let bd = parseEntry [] file
                            return (BD bd)) handler
            where
                handler :: SomeException -> IO BaseDatos
                handler ex = do
                                putStrLn "Ha ocurrido un error al leer la base de datos"
                                print ex
                                putStrLn "\n\n Se ha iniciado el programa con una base de datos de pruebas."
                                putStrLn "Para cambiar la ruta o leer la base de datos otra vez usar el comando config"
                                bd' <- defaultBD
                                return $! bd'

parseEntry :: [Producto] -> [String] -> [Producto]
-- Lee los valores de la base de datos y crea la lista de productos
parseEntry bd (a:b:c:xs) = parseEntry (bd ++ [Prod (read a :: Codigo) b (read c :: Precio)]) xs
parseEntry bd _        = bd

bucarRuta :: IO FilePath
bucarRuta = return "library.txt"


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
            m <- initModel
            sesionCon $! m

-----------------------------------------------------------------------------------------------------------------------
--- Proceso principal
-----------------------------------------------------------------------------------------------------------------------

sesionCon :: Modelo -> IO()
-- El proceso principal se llama recursivamente, y consiste en la lectura y ejecución de un comando
-- en cada llamada. Con cada ejecución de un comando se obtiene una base de datos actualizada y se utiliza
-- para la siguiente llamada. En caso contrario, se guarda la base de datos y se termina sesión.
sesionCon Model{..} =  do
                putStr "\nsupermercado> "
                comando <- getLine
                m <- ejecutaCon Model{..} comando
                case m of
                    Just Model{cerrar = True,..} ->  guardaBD bd pathBD 
                    Just Model{..} -> sesionCon Model{..}
                    Nothing -> sesionCon Model{..}

ejecutaCon :: Modelo -> Orden -> IO (Maybe Modelo)
-- ejecutaCon m orden = devuelve el modelo de datos resultante de ejecutar el comando indicado por 'orden' 
-- en caso de no existir se devuelve el modelo de datos original.
--      
--      Esta función captura cualquier excepción posible y la trata mostrando su mensaje y conservando el modelo de 
--      datos original.
-- 
ejecutaCon m orden = catch 
                        (case find (\Com{..} -> nombre == orden) comandos of
                                Just Com{..} -> return <$> funcion m
                                Nothing ->  do
                                            putStrLn (sugerirComando orden ++ "El comando introducido no exite, prueba 'ayuda' para ver los comando disponibles.")
                                            return (Just m)
                        ) handler
                    where
                        handler :: SomeException -> IO (Maybe Modelo)
                        handler ex = do
                                        putStrLn (takeWhile (/= '#') (show ex))
                                        return (Just m)

-----------------------------------------------------------------------------------------------------------------------
--- Comandos del usuario
-----------------------------------------------------------------------------------------------------------------------

data Comando = Com {nombre ::String, descripcion ::String, funcion :: Modelo -> IO Modelo}

comandos :: [Comando]
comandos = [Com "ayuda" "muestra esta ayuda" ayuda,
            Com "conPre" "consulta el precio de un producto" conPre,
            Com "conNom" "consulta el nombre de un producto" conNom,
            Com "camPre" "cambia el precio de un producto" camPre,
            Com "camNom" "cambia el nombre de un producto" camNom,
            Com "metPro" "mete un nuevo producto" metPro,
            Com "eliPro" "elimina un producto" eliPro,
            Com "mosBD" "muestra el contenido de la Base de datos" mosBD,
            Com "mosBDnombre" "muestra la BD ordenada por Nombre" mosBDnombre,
            Com "mosBDprecio" "muestra la BD ordenada por Precio" mosBDprecio,
            Com "config" "muestra el menu de configuracion" config,
            Com "fin" "termina la sesion, guardando la Base de datos" fin]

ayuda :: Modelo -> IO Modelo
ayuda m =  do
            putStrLn menu
            return m

conPre :: Modelo -> IO Modelo
conPre Model{..} = do
            putStr "Codigo? "
            cod <- leerNumero
            print (consultarPrecio cod bd)
            return Model{..}

conNom :: Modelo -> IO Modelo
conNom Model{..} = do
            putStr "Codigo? "
            cod <- leerNumero
            print (consultarNombre cod bd)
            return Model{..}

camPre :: Modelo -> IO Modelo
camPre Model{..} = do 
            putStr "Codigo? "
            cod <- leerNumero
            putStrLn (("Precio Actual: " ++ ) `seq` show (consultarPrecio cod bd))
            putStr "Nuevo Precio? "
            pre <- leerFloat
            return Model{bd = cambiarPrecio pre cod bd, ..}

camNom :: Modelo -> IO Modelo
camNom Model{..} = do 
            putStr "Codigo? "
            cod <- leerNumero
            putStrLn (("Nombre Actual: " ++) `seq` show (consultarNombre cod bd))
            putStr "Nuevo Nombre? "
            nombre <- getLine
            return Model{bd = cambiarNombre nombre cod bd, ..}

metPro :: Modelo -> IO Modelo
metPro Model{..} = do 
            putStr "Codigo? "
            cod <- leerNumero
            putStr "Nombre? "
            nom <- getLine
            putStr "Precio? "
            pre <- leerFloat
            return Model{bd = insertar (Prod cod nom pre) bd, ..}

eliPro :: Modelo -> IO Modelo
eliPro Model{..} = do 
            cod <- leerCodigoExistente bd
            return Model{bd = eliminar cod bd, ..}

mosBD :: Modelo -> IO Modelo
mosBD Model{..} =  do
            putStrLn "Base de datos actual:"
            imprimir bd
            return Model{..}

mosBDnombre :: Modelo -> IO Modelo
mosBDnombre Model{..} = do
            putStrLn "Base de datos actual:"
            imprimirPorNombre bd
            return Model{..}

mosBDprecio :: Modelo -> IO Modelo
mosBDprecio Model{..} = do
            putStrLn "Base de datos actual:"
            imprimirPorPrecio bd
            return Model{..}

config :: Modelo -> IO Modelo
-- Abre un pequeño menu para otro tipo de acciones
config Model{..} = do 
            putStrLn "Menu de configuración"
            putStrLn "1 - Leer Base de Datos"
            putStrLn "2 - Escribir Base Datos"
            putStrLn "3 - Cambiar Ruta de la Base Datos"
            putStrLn "_ - Volver"
            opt <- getLine
            case opt of 
                "1" ->  do
                        bd' <- recuperaBD pathBD
                        return Model{bd = bd',..}
                "2" ->  do
                        guardaBD bd pathBD 
                        return Model{..}
                "3" ->  do 
                        putStrLn ("Ruta actual de la Base de Datos : " ++ pathBD)
                        putStrLn "Nueva Ruta : "
                        pathBD' <- getLine
                        bd' <- recuperaBD pathBD' 
                        return Model{bd = bd', pathBD = pathBD',..}
                _  -> return Model{..}

fin :: Modelo -> IO Modelo
fin Model{..} = return Model{cerrar = True,..}

-----------------------------------------------------------------------------------------------------------------------
--- Funciones auxiliares
-----------------------------------------------------------------------------------------------------------------------

menu :: String
menu = "\n Comandos Disponibles:\n" ++ concatMap (\Com{..} -> nombre ++": " ++ descripcion ++"\n") comandos
        
sugerirComando :: String -> String
-- Sugiere un comando parecido al introducido en caso de que este no esté entre los comandos disponibles
sugerirComando s = elegirMinimo(zip (map (\Com{..} -> hammingDistance s nombre) comandos) comandos)

elegirMinimo :: [(Int, Comando)] -> String
-- elegirMinimo (a,b) = nombre del comando de menor a o el string vacío si no existe a <= 2
elegirMinimo l = if val > 2 then "" else "Tal vez quisiste decir " ++ SuperInteractivo.nombre com ++ "\n"
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


leerCodigoExistente :: BaseDatos -> IO Codigo
leerCodigoExistente bd = do
        putStr "Codigo? "
        cod <- leerNumero
        if not (estaCodigo cod bd)
                then do 
                    putStrLn "El codigo no existe, introduzca otro." 
                    leerCodigoExistente bd
                else do 
                    return cod


bienvenida :: String
bienvenida =  unlines [ " _______   __                                                    __        __           "
                        ,"|       \\ |  \\                                                  |  \\      |  \\          "
                        ,"| $$$$$$$\\ \\$$  ______   _______  __     __   ______   _______   \\$$  ____| $$  ______  "
                        ,"| $$__/ $$|  \\ /      \\ |       \\|  \\   /  \\ /      \\ |       \\ |  \\ /      $$ /     \\ "
                        ,"| $$    $$| $$|  $$$$$$\\| $$$$$$$\\\\$$\\ /  $$|  $$$$$$\\| $$$$$$$\\| $$|  $$$$$$$|  $$$$$$\\"
                        ,"| $$$$$$$\\| $$| $$    $$| $$  | $$ \\$$\\  $$ | $$    $$| $$  | $$| $$| $$  | $$| $$  | $$"
                        ,"| $$__/ $$| $$| $$$$$$$$| $$  | $$  \\$$ $$  | $$$$$$$$| $$  | $$| $$| $$__| $$| $$__/ $$"
                        ,"| $$    $$| $$ \\$$     \\| $$  | $$   \\$$$    \\$$     \\| $$  | $$| $$ \\$$    $$ \\$$    $$"
                        ," \\$$$$$$$  \\$$  \\$$$$$$$ \\$$   \\$$    \\$      \\$$$$$$$ \\$$   \\$$ \\$$  \\$$$$$$$  \\$$$$$$ "]