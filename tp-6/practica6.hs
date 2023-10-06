import PriorityQueueV2
import Map

--EJERCICIO 2 PQ
--1)
heapSort :: Ord a => [a] -> [a]
heapSort xs = pQToList (insertarTodosEn xs emptyPQ)

--O(n^2)
insertarTodosEn :: Ord a =>  [a] -> PriorityQueue a -> PriorityQueue a
insertarTodosEn [] pq = pq
insertarTodosEn (x:xs) pq= (insertarTodosEn xs (insertPQ x pq))

--O(n^2)
pQToList ::  Ord a => PriorityQueue a -> [a]
pQToList pq = if(isEmptyPQ pq)
                then []
                else findMinPQ pq : pQToList (deleteMinPQ pq)

--EJERCICIO 3 MAP
--O(n^2)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = lookUpDeTodas (keys map) map

--O(n^2)
lookUpDeTodas :: [a] -> Map k v-> [Maybe v]
lookUpDeTodas [] _ = []
lookUpDeTodas (k: ks) map = case lookupM k map of
                            Just x -> (lookupM k map) : (lookUpDeTodas ks (deleteM k map))
                            Nothing -> (lookUpDeTodas ks (deleteM k map))

--O(n^2)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k:ks) map = case (lookupM k map) of
                            Nothing -> False
                            Just x  -> todasAsociadas ks (deleteM k map)

--O(n^2)
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

--O(n^2)
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = valoresYSusKeysDe (keys map) map

--O(n^2)
valoresYSusKeysDe :: Eq k => [k] -> Map k v -> [(k, v)]
valoresYSusKeysDe [] _ = []
valoresYSusKeysDe (k:ks) map = (k, (lookupM k map)) : (valoresYSusKeysDe ks (deleteM k map))

--O(n^2)
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v) : xs) = case lookupM k (agruparEq xs) of
                          Just ys -> assocM k (v:ys) (agruparEq xs)
                          Nothing -> assocM k [v] (agruparEq xs)

--O(n^2)
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] map =
incrementar (k:ks) map = case lookupM k map of
                          Just n -> assocM k (n+1) (incrementar ks (deleteM k map))
                          Nothing -> (incrementar ks map)

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = assocDeTodosLosDe_ConKeys_A map1 (keys map1) map2

assocDeTodosLosDe_ConKeys_A :: Eq k => Map k v -> [a] -> Map k v -> -> Map k v
assocDeTodosLosDe_ConKeys_A map1 [] map2 = 
assocDeTodosLosDe_ConKeys_A map1 (k:ks) map2 = case (lookupM k map1) of
                                                Just x -> assocM k (lookupM k map1) (assocDeTodosLosDe_ConKeys_A (deleteM k map1) ks map2)
                                                Nothing -> (assocDeTodosLosDe_ConKeys_A (deleteM k map1) ks map2)

--EJERCICIO 5 MAP
indexar :: [a] -> Map Int a
indexar xs = indexarAPartirDe xs 0

--O(n^2)
indexarAPartirDe :: [a] -> Int -> Map Int a
indexarAPartirDe [] _ = emptyM
indexarAPartirDe (x:xs) n = assocM n x (indexarAPartirDe xs (n+1))

--O(n^2)
ocurrencias :: String -> Map Char Int
ocurrencias [] = emptyM
ocurrencias (x:xs) = assocM x (cantidadDeVecesQueEstaEn x xs) (ocurrencias xs)

--O(n)
cantidadDeVecesQueEstaEn :: Eq a => a -> [a] -> Int
cantidadDeVecesQueEstaEn _ [] = 0
cantidadDeVecesQueEstaEn x (y:ys) = if (x == y)
                                        then 1 + cantidadDeVecesQueEstaEn x ys
                                        else cantidadDeVecesQueEstaEn x ys 


ocurrencias :: Ord a => [a] -> [(a,Int)]
ocurrencias cs = multisetToList(listToMS cs)

listToMS :: Ord a => [a] -> Multiset a
listToMS [] = emptyMS
listToMS (x:xs) = addMS x (listToMS xs)

