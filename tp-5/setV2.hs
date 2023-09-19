module Set 
    (Set,emptyS, addS, belongS, sizeS, removeS, unionS, setToList)
where
    data Set a = Set [a]

    emptyS :: Set a
    emptyS = (Set [])

    addS :: Eq a => a -> Set a -> Set a
    addS x (Set ys) = (Set (x:ys)) --PASA DE LINEAL A CONSTANTE PORQUE NO CHEQUEA REPS

    belongS :: Eq a => a -> Set a -> Bool
    belongS x (Set ls)= pertenece x ls

    sizeS :: Eq a => Set a -> Int
    sizeS (Set ls) = longitud(listaSinReps ls) --PASA DE LINEAL A CUADRATICA

    removeS :: Eq a => a -> Set a -> Set a
    removeS k (Set ls) = (Set (remove k ls))

    unionS :: Eq a => Set a -> Set a -> Set a
    unionS (Set xs) (Set ys) = (Set (xs ++ ys)) --PASA DE LINEAL A CONSTANTE

    setToList :: Eq a => Set a -> [a]
    setToList (Set xs) = (Set (listaSinReps xs)) --PASA DE LINEAL A CUADRATICA

    pertenece :: Eq a => a -> [a] -> Bool
    pertenece k [] = False
    pertenece k (n:ns) = (n==k) || (pertenece k ns)

    longitud :: [a] -> Int
    longitud [] = 0
    longitud (n : ns) = 1 + longitud ns

    remove :: Eq a => a -> [a] -> [a]
    remove k (x:xs) = if(k == x)
                        then xs
                        else x: (remove k xs)

    listaSinReps :: Eq a => [a] -> [a]
    listaSinReps [] = []
    listaSinReps (x:xs) = if((cantidadDeVecesQueApareceEn x xs) >= 2)
                            then listaSinReps xs
                            else x : (listaSinReps xs)

    cantidadDeVecesQueApareceEn :: Eq a => a -> [a] -> [a]
    cantidadDeVecesQueApareceEn _ [] = False
    cantidadDeVecesQueApareceEn k (x:xs) = if (k == x) 
                                                then 1 + cantidadDeVecesQueApareceEn k xs
                                                else cantidadDeVecesQueApareceEn k xs

    unoSiCeroSiNo :: Bool -> Int
    unoSiCeroSiNo True = 1
    unoSiCeroSiNo False = 0

