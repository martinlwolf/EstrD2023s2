module PriorityQueueV2
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where
    data PriorityQueue a = PQ [a]

{- INV REP: los elementos de la pq deben ser ordenables
-}

--O(1)
    emptyPQ :: PriorityQueue a
    emptyPQ = PQ []

--O(1)
    isEmptyPQ :: PriorityQueue a -> Bool
    isEmptyPQ (PQ xs) = null xs

--O(n)
    insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
    insertPQ x (PQ ys) = (PQ (sort (x:ys)))

--O(1)
--PRECOND: La queue no debe ser vacia
    findMinPQ :: Ord a => PriorityQueue a -> a
    findMinPQ (PQ (x:xs)) = x

--O(1)
--PRECOND: La queue no debe ser vacia
    deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
    deleteMinPQ (PQ (x:xs)) = (PQ xs)

    elMinimo :: Ord a => [a] -> a
    elMinimo [] = error "la lista no debe estar vacia"
    elMinimo [x] = x
    elMinimo (n:ns) = if (n < elMinimo ns)
                        then n
                        else elMinimo ns
                        
    laListaSinElElemento :: Eq a => [a] -> a -> [a]
    laListaSinElElemento [] k = []
    laListaSinElElemento (x:xs) k = if (k == x)
                                    then laListaSinElElemento xs k
                                    else x : (laListaSinElElemento xs k)

    sort :: Ord a => [a] -> [a]
    sort [] = []  -- La lista vacía ya está ordenada.
    sort (x:xs) = 
        let menores = sort [a | a <- xs, a <= x]
            mayores = sort [a | a <- xs, a > x]
        in menores ++ [x] ++ mayores
