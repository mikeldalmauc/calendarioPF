--------------------------------------------------------------------
--  PRACTICA: CALENDARIO        --        PF  2018-19

--  Nombre(s):   Mikel Dalmau y Julen Fernandino
---------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n 
-- donde c = columnas (3 ó 4) y  n = año cuyo calendario deseamos imprimir

module Calendarios where
    
    import qualified Data.List

    type Dibujo = [Linea]  -- cada dibujo es una lista de líneas (de igual longitud)
    type Linea = String   -- cada línea es una lista de caracteres
    type Year = Int
    type Month = Int
    type Columna = Int       -- será 3 o 4
    type Lang = String    -- "en","es","eu","ca" u otros
    
    -- Para imprimir un dibujo en pantalla:
    printDibujo :: Dibujo -> IO()
    printDibujo dib = do
                       putStr "\n"
                       (putStr . concat . map (++"\n")) dib
    
    -- Para imprimir el calendario de un año con un número de columnas y un idioma ("en","es","eu" y "ca")
    printCalendario :: Columna -> Year -> Lang -> IO()
    printCalendario c a lang = printDibujo (calendario c lang a)
    
    
    -- Funcion principal:
    
    calendario :: Columna -> Lang -> Year -> Dibujo   
    --el dibujo de un calendario en c columnas de un año dado y un idioma ("en","es","eu" y "ca")
    calendario c lang y =  bloque c (map (dibujomes lang) (meses y lang))
    
    --------------------------------------------------------------------
    --  Define las siguientes funciones sobre dibujos:
    --------------------------------------------------------------------
    
    dibEsCorrecto :: Dibujo -> Bool   
    -- comprueba que las lineas de un dibujo tienen igual longitud
    dibEsCorrecto []        = True
    dibEsCorrecto (x:xs)    = null (dropWhile (== length x) (map length (x:xs))) 
    
    listaDibCorrectos :: [Dibujo] -> Bool 
    -- comprueba que todos los dibujos de la lista son correctos y 
    -- tienen todos las mismas dimensiones    
    listaDibCorrectos []    = True
    listaDibCorrectos s     = all dibEsCorrecto s && todosIguales (map alto s) && todosIguales (map ancho s)

    todosIguales :: Eq a => [a] -> Bool
    -- Comprueba si todos los elementos de una lista son iguales y nos devuelve verdadero o falso.
    todosIguales [] = True
    todosIguales s  = all (== head s) (tail s)
    
    alto :: Dibujo -> Int 
    -- Devuelve la altura de un dibujo correcto
    alto [] = 0
    alto s 
        | dibEsCorrecto s   = length s
        | otherwise         = error "El dibujo no es correcto"
    
    
    ancho :: Dibujo -> Int
    -- Devuelve la anchura de un dibujo correcto
    ancho [] = 0
    ancho s
        | dibEsCorrecto s   = length (head s)
        | otherwise         = error "El dibujo no es correcto"
        
    
    sobre :: Dibujo -> Dibujo -> Dibujo 
    -- Precondicion: los dibujos tienen la misma anchura
    -- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2
    sobre [] _ = error "d1 es 'null'"
    sobre _ [] = error "d2 es 'null'"
    sobre d1 d2 
        | ancho d1 == ancho d2  = d1 ++ d2
        | otherwise                 = error "No tienen la misma anchura"

    
    alLado :: Dibujo -> Dibujo -> Dibujo   
    -- Precondicion: los dibujos tienen la misma altura
    -- alLado d1 d2 pone d1 a la izquierda de d2
    alLado [] _ = error "d1 es 'null'"
    alLado _ [] = error "d2 es 'null'"
    alLado d1 d2
        | alto d1 == alto d2    = zipWith (++) d1 d2
        | otherwise                 = error "No tienen la misma altura"
    
    apilar :: [Dibujo] -> Dibujo
    -- Precondicion: los dibujos de la lista (no vacia) tienen la misma anchura
    -- apila todos los dibujos de una lista (el primero de la lista queda en la cima de la pila)
    apilar [] = []
    apilar (x:xs)
        | todosIguales (map ancho (x:xs))   = foldl sobre x xs
        | otherwise                         = error "No tienen la misma anchura"
    
    
    extender :: [Dibujo] -> Dibujo
    -- Precondicion: los dibujos de la lista (no vacia) tienen la misma altura
    -- extiende todos los dibujos de una lista (el primero de la lista queda el más a la izquierda)
    extender [] = []
    extender (x:xs)
        | null xs                           = x
        | todosIguales (map alto (x:xs))    = foldl alLado x xs
        | otherwise                         = error "No tienen la misma altura"
    
    
    dibBlanco :: (Int,Int) -> Dibujo
    -- dibBlanco (al,an) devuelve el dibujo de caracteres blancos de altura al y anchura an
    -- Precondicion: al>0 && an>0
    dibBlanco (al, an)
        | al>0 && an>0  = replicate al (blancos an)
        | otherwise     = error "Altura y anchura <= 0"
    
    
    bloque :: Int -> [Dibujo] -> Dibujo
    -- bloque n dibs es el dibujo formado al agrupar de n en n los dibujos de la lista dibs,
    -- extender cada sublista y luego apilarlas
    bloque n dibs
        | length dibs >= n  = extender (take n dibs) ++ bloque n (drop n dibs)
        | otherwise         = extender dibs
    
    
    -- otras funciones auxiliares sobre dibujos que se necesiten:
    
    -------------------------------------------------------------------------------
    --  Define constantes y funciones necesarias para calcular y dibujar los meses 
    -------------------------------------------------------------------------------
    
    
    meses :: Year -> Lang -> [(String, Year, Int, Int)]
    -- meses n devuelve una lista de 12 elementos con los datos relevantes de cada uno de
    -- los meses del año n: (nombre del mes, n, primer día del mes, longitud del mes)
    meses n lang = 
        let 
            nombreMeses = nombresMeses lang
            year = replicate 12 n
            pDias = pdias (ene1 n) 1 n
            lmeses = [lmes i n | i<-[1..12]] 
        in 
            Data.List.zip4 nombreMeses year pDias lmeses
    
            
    dibujomes ::Lang -> (String, Year, Int, Int) -> Dibujo
    -- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25 formado 
    -- por el titulo y la tabla del mes de nombre nm y año a.
    dibujomes lang (nm,a,pd,lm) = 
        let 
            titulo = nm ++ " " ++ show a
            lineaVacia = ""
            semana = diasSemana lang
            dias = chopDias (concatMap celdaDia [2-pd..lm])
            dibujo = [titulo, lineaVacia, semana] ++ dias ++ replicate (7-length dias) lineaVacia   -- Añadir a la lista todas las filas necesarias
        in            
            map (appendBlancosHasta 25) dibujo       -- Añadir blancos por la derecha hasta 25 a todos


    ene1 :: Year -> Int
    -- ene1 a devuelve el dia de la semana del 1 de enero del año a 
    --  siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo
    ene1 a = mod (a + div (a-1) 4 - div (a-1) 100 + div (a-1) 400) 7


    pdias :: Int -> Month -> Year -> [Int]
    -- pdias c m a    devuelve una lista con 12 dias que son los días de la semana 
    --                en que comienza cada mes del año a
    --                siendo 1 = lunes, 2 = martes, ..., 6= sabado, 7=domingo
    --  
    --   Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]
    -- c es el día de comienzo del mes 
    -- m es el numero del mes en cuestion
    -- a es el año
    pdias c m a 
            | m > 12 = []
            | c == 0 = 7: pdias c' (m+1) a
            | otherwise = c: pdias c' (m+1) a
            where c' = mod (c + lmes m a) 7
    

    lmes :: Month -> Year -> Int
    -- Dado un mes y un año, dice cuantos dias tiene ese mes
    lmes m a 
        | m `elem` [4,6,9,11] = 30
        | m == 2 && (mod a 4 == 0 && (mod a 100 /= 0 || mod a 400 == 0)) = 29
        | m == 2 = 28 
        | otherwise = 31
            
    -- otras funciones que se necesiten:
    
    appendBlancosHasta :: Int -> String -> String
    -- añade hasta n espacios blancos tras lel siguiente string
    appendBlancosHasta i s 
        | length s >= i = s
        | otherwise = s ++ blancos (i-length s)


    prependBlancosHasta :: Int -> String -> String
    -- añade hasta n espacios blancos delante del siguiente string
    prependBlancosHasta i s 
        | length s >= i = s
        | otherwise = blancos (i - length s) ++ s
    

    blancos :: Int -> String
    -- devuelve un string de blancos, con un número n que le demos como parámetro.
    blancos n = replicate n ' '


    celdaDia :: Int -> String
    -- Dado un número mayor que 0, devuelve su representación de String en una 
    -- celda de longintud mínima de 3 añadiendo hasta 1 blanco por delante y el resto por detrás.
    -- En caso de no ser un número valido, devuelve 3 espacios.
    celdaDia n 
        | n <= 0    = blancos 3
        | otherwise = appendBlancosHasta 3 (prependBlancosHasta 2 (show n))


    chopDias :: String -> [String]
    -- Corta el String de días en bloques de 7 dias creando una lista de strings de longitud 7*3 = 21
    -- como máximo
    chopDias s =
        case splitAt 21 s of
            (_, []) -> [s]
            (a,b)   ->  a:chopDias b
           

    nombresMeses :: Lang -> [String]
    -- devuleve una lista con los nombres de los meses.
    -- la lista dependerá del idioma s que le pasemos como párametro, o "es" por defecto.
    nombresMeses s = 
        case s of
            "es" -> ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"]
            "eu" -> ["Urtarrila","Otsaila","Martxoa","Apirila","Maiatza","Ekaina","Uzataila","Abuztua","Iraila","Urria","Azaroa","Abendua"]
            "en" -> ["January","February","March","April","May","June","July","August","September","October","November","December"]
            "ca" -> ["Gener","Febrer","Març","Abril","Maig","Juny","Juliol","Agost","Septembre","Octubre","Novembre","Decembre"]
            _ ->  nombresMeses "es"
    

    diasSemana :: String -> String
    -- devuleve una lista con los nombres de los días.
    -- la lista dependerá del idioma s que le pasemos como párametro, o "es" por defecto.
    diasSemana s =
        case s of
            "es" -> "Lu Ma Mi Ju Vi Sa Do"
            "eu" -> "Al As Az Og Or Lr Ig"
            "en" -> "Mo Tu We Th Fr Sa Su"
            "ca" -> "Dl Dm Dc Dj Dv Ds Dg"
            _ ->  diasSemana "es"
    