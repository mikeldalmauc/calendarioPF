--------------------------------------------------------------------
--  PRACTICA: CALENDARIO        --        PF  2018-19

--  Nombre(s):   
---------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n 
-- donde c = columnas (3 ó 4) y  n = año cuyo calendario deseamos imprimir

module Calendarios where

    type Dibujo = [Linea]  -- cada dibujo es una lista de líneas (de igual longitud)
    type Linea = String   -- cada línea es una lista de caracteres
    type Year = Int
    type Columna = Int       -- será 3 o 4
    
    -- Para imprimir un dibujo en pantalla:
    printDibujo :: Dibujo -> IO()
    printDibujo dib = do
                       putStr "\n"
                       (putStr . concat . map (++"\n")) dib
    
    -- Para imprimir el calendario de un año con un número de columnas:
    -- printCalendario :: Columna ->  Year -> IO()
    -- printCalendario c a = printDibujo (calendario c a)
    
    
    -- Funcion principal:
    
    -- calendario :: Columna -> Year -> Dibujo   
    -- el dibujo de un calendario en c columnas de un año dado
    -- calendario c  =  bloque c . map dibujomes . meses
    
    --------------------------------------------------------------------
    --  Define las siguientes funciones sobre dibujos:
    --------------------------------------------------------------------
    
    -- dibEsCorrecto :: Dibujo -> Bool   
    -- comprueba que las lineas de un dibujo tienen igual longitud
    
    
    -- listaDibCorrectos ::[Dibujo] -> Bool 
    -- comprueba que todos los dibujos de la lista son correctos y 
    -- tienen todos las mismas dimensiones
    
    
    -- alto :: Dibujo -> Int   
    -- Devuelve la altura de un dibujo correcto
    
    
    -- ancho :: Dibujo -> Int  
    -- Devuelve la anchura de un dibujo correcto
    
    
    -- sobre :: Dibujo -> Dibujo -> Dibujo 
    -- Precondicion: los dibujos tienen la misma anchura
    -- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2
    
    
    -- alLado :: Dibujo -> Dibujo -> Dibujo   
    -- Precondicion: los dibujos tienen la misma altura
    -- alLado d1 d2 pone d1 a la izquierda de d2
    
    -- apilar :: [Dibujo] -> Dibujo
    -- Precondicion: los dibujos de la lista (no vacia) tienen la misma anchura
    -- apila todos los dibujos de una lista (el primero de la lista queda en la cima de la pila)
    
    
    -- extender :: [Dibujo] -> Dibujo
    -- Precondicion: los dibujos de la lista (no vacia) tienen la misma altura
    -- extiende todos los dibujos de una lista (el primero de la lista queda el más a la izquierda)
    
    
    -- dibBlanco :: (Int,Int) -> Dibujo
    -- dibBlanco (al,an) devuelve el dibujo de caracteres blancos de altura al y anchura an
    -- Precondicion: al>0 && an>0
    
    
    -- bloque :: Int -> [Dibujo] -> Dibujo
    -- bloque n dibs es el dibujo formado al agrupar de n en n los dibujos de la lista dibs,
    -- extender cada sublista y luego apilarlas
    
    
    -- otras funciones auxiliares sobre dibujos que se necesiten:
    
    
    
    -------------------------------------------------------------------------------
    --  Define constantes y funciones necesarias para calcular y dibujar los meses 
    -------------------------------------------------------------------------------
    
    -- meses ::  Year -> [(String, Year, Int, Int)]
    -- meses n devuelve una lista de 12 elementos con los datos relevantes de cada uno de
    -- los meses del año n: (nombre del mes, n, primer día del mes, longitud del mes)
    
    
    -- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25 formado 
    -- por el titulo y la tabla del mes de nombre nm y año a. 
    -- Necesita como parámetros el primer dia pd y la longitud del mes lm
    dibujomes ::(String, Year, Int, Int) -> Dibujo
    dibujomes (nm,a,pd,lm) = 
        let 
            titulo = nm ++ " " ++ show a ++ "\n\n"  -- Titulo, Tan ancho como 25
            semana = diasSemana "es" ++"\n"         -- Dias de la semana en Idioma elegido
            dias =                                  -- Dias del mes
            dibujo = [titulo, semana, dias]
        in            
            map (sumarBlancosHasta 25) dibujo       -- Añadir blancos por la derecha hasta 25 a todos

    ene1 :: Year -> Int
    ene1 a = mod (a + div (a-1) 4 - div (a-1) 100 + div (a-1) 400) 7
    -- ene1 a devuelve el dia de la semana del 1 de enero del año a 
    --        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo
     
    
    -- pdias :: Int -> [Int]
    -- pdias a    devuelve una lista con 12 dias que son los días de la semana 
    --            en que comienza cada mes del año a
    --            siendo 1 = lunes, 2 = martes, ..., 6= sabado, 7=domingo
    -- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]
    
    
    
    -- otras funciones que se necesiten:
    
    -- añade hasta n espacios blancos tras lel siguiente string
    sumarBlancosHasta :: Int -> String -> String
    sumarBlancosHasta i s 
        | length s >= i = s
        | otherwise = s ++ blancos (length s - i)

    blancos :: Int -> String
    blancos n = [' '|i<-[1..n]]
    
    
    -- TODO Nombres de mes, segun numero de mes e idioma, crear tipo ennumerado
    --mes :: Int -> String -> String
    --mes n s 
        
    diasSemana :: String -> String
    diasSemana s =
        case s of
            "es" -> "Lu Ma Mi Ju Vi Sa Do"
            "eu" -> "Al As Az Og Or Lr Ig"
            "en" -> "Mo Tu We Th Fr Sa Su"
            _ ->  diasSemana "es"


    