import PriorityQueueV2

--EJERCICIO 2 PQ
--1)
heapSort :: Ord a => [a] -> [a]
heapSort xs = pQToList (insertarTodosEn xs emptyPQ)

--O(n^2)
insertarTodosEn :: [a] -> PriorityQueue a -> PriorityQueue a
insertarTodosEn [] pq = pq
insertarTodosEn (x:xs) pq= insertPQ x pq : (insertarTodosEn xs pq)

--O(n^2)
pQToList :: PriorityQueue a -> [a]
pQToList pq = if(isEmptyPQ pq)
                then []
                else findMinPQ pq : pQToList (deleteMinPQ)

--EJERCICIO 3 MAP
--O(n^2)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = lookUpDeTodas (keys map) map

--O(n^2)
lookUpDeTodas :: [a] -> Map k v-> [Maybe v]
lookUpDeTodas [] = []
lookUpDeTodas (k: ks) map = (lookupM k map) :  (lookUpDeTodas ks (deleteM k map))

--O(n^2)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k:ks) map = if (isNothing(lookupM k map)) 
                            then False
                            else todasAsociadas ks (deleteM k map)

--O(n^2)
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

--O(n^2)
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = valoresYSusKeysDe (keys map) map

--O(n^2)
valoresYSusKeysDe :: Eq k => [k] -> Map k v -> [(k, v)]
valoresYSusKeysDe [] = []
valoresYSusKeysDe (k:ks) map = (k, (lookupM k map)) : (valoresYSusKeysDe ks (deleteM k map))

--O(n^2)
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v) : xs) = case lookupM k (agruparEq xs) of
                          Just ys -> assocM k (v:ys) (agruparEq xs)
                          Nothing -> assocM k [v] (agruparEq xs)

multisetToList :: Multiset a -> [(a,Int)]
multisetToList (MS map) = mapToList map

emptyM :: Multiset a
emptyM = MS emptyM

addMS :: Ord a => a -> Multiset a -> Multiset a
addMS x (MS map) = case lookupM x mp of
                    Just n -> MS (assocM x (n+1) map)
                    Nothing -> MS (assocM x 1 map)

ocurrences :: Ord a => a -> Multiset a -> Int
ocurrences x (MS map) = case lookupM x mp of
                    Just n -> n
                    Nothing -> 0

ocurrencias :: Ord a => [a] -> [(a,Int)]
ocurrencias cs = multisetToList(listToMS cs)

listToMS :: Ord a => [a] -> Multiset a
listToMS [] = emptyMS
listToMS (x:xs) = addMS x (listToMS xs)

