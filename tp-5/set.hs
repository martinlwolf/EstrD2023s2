module Set 
    (Set,emptyS, addS, belongS, sizeS, removeS, unionS, setToList)
where
    data Set a = Set [a]

    emptyS :: Set a
    emptyS = (Set [])

    addS :: Eq a => a -> Set a -> Set a
    addS x (Set ys) = if(pertenece x ys)
                        then (Set ys)
                        else (Set (x:ys))

    belongS :: Eq a => a -> Set a -> Bool
    belongS x (Set ls)= pertenece x ls

    sizeS :: Eq a => Set a -> Int
    sizeS (Set ls) = longitud ls

    removeS :: Eq a => a -> Set a -> Set a
    removeS k (Set ls) = (Set (remove k ls))

    unionS :: Eq a => Set a -> Set a -> Set a
    unionS (Set (x:xs)) (Set ys) = addS x (unionS (Set xs) (Set ys))

    setToList :: Eq a => Set a -> [a]
    setToList (Set xs) = xs

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