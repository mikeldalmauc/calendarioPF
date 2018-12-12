module CalendarioInteractivo where

    import Calendarios (printCalendario)
    
    main :: IO ()
    main = do
        putStr "CALENDARIO"
        loop

    loop :: IO()
    loop = 
        do
            putStr "\n Give me a year (a>0) \n a = "
            a <- leerEnt
            putStr "\n Give me the number of cuolumns \n c = "
            c <- leerEnt
            putStr "\n Give me the language (\"es\", ... ) \n l = "
            l <- leerString
            printCalendario c a l
            loop

    leerEnt :: IO Int
    leerEnt = do 
        e <- getLine
        return (read e)
    
    leerString :: IO String
    leerString = do 
        e <- getLine
        return (read e)
    