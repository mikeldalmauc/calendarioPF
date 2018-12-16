module EjerciciosMikel where


reinas :: Int -> [[Int]]
reinas 0 = [[]]
reinas m = [p ++ [n] | p <- reinas (m-1), n <- [1..8], seguro p n]

seguro :: [Int] -> Int -> Bool
seguro p n = and [not (mata (i,j)(k,n)) | (i,j) <- zip [1..h] p ]
    where 
        h = length p
        k = h + 1

mata :: (Int, Int) -> (Int, Int) -> Bool
mata (i,j) (i', j') = (j == j') || (i == i') || (i+j == i'+j') || (i-j == i'-j')


