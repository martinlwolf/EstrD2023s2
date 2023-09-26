import Set
--PUNTO 1

head' :: [a] -> a
head' (x:xs) = x
--CONSTANTE

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
--CONSTANTE

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
--LINEAL

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--LINEAL

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
--LINEAL

pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 n [] = False
pertenece2 n (x:xs) = n == x || pertenece2 n xs
--LINEAL

sinRepetidos2 :: Eq a => [a] -> [a]
sinRepetidos2 [] = []
sinRepetidos2 (x:xs) = if pertenece x xs
                        then sinRepetidos2 xs
                        else x : sinRepetidos2 xs
--CUADRATICO

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
--LINEAL

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

--LINEAL

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
--LINEAL

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
--LINEAL

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
--CUADRATICA

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
--LINEAL

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                        else x : sacar n xs
--LINEAL

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =    let m = minimo xs in m : ordenar (sacar m xs)
--CUADRATICA

--PUNTO 2

--SET
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) set = if(pertenece x (setToList set))
                                    then x : losQuePertenecen xs set
                                    else losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos ls = setToList(setConElementos ls)

setConElementos :: Eq a => [a] -> Set a
setConElementos [] = emptyS
setConElementos (x:xs) = addS x (setConElementos xs)

{--unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT set tree1 tree2) =  unionS (unirTodos tree1) (unirTodos tree2)-}


pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (n:ns) = (n==k) || (pertenece k ns)

{--QUEUE
lengthQ :: Queue a -> Int
lengthQ queue = length (elementosDeLaQueue queue)

elementosDeLaQueue :: Queue a -> [a]
elementosDeLaQueue queue = if(isEmptyQ queue)
                            then []
                            else firstQ queue : (elementosDeLaQueue queue)

queueToList :: Queue a -> [a]
queueToList queue = if(isEmptyQ queue)
                            then []
                            else firstQ queue : (elementosDeLaQueue queue)

unionQ :: Queue a -> Queue a -> Queue a
unionQ queue1 queue2 = if(isEmptyQ queue2)
                        then queue1
                        else unionQ (enQueue (firstQ queue2) queue1) (deQueue queue2)

--STACK
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar stack = if(isEmptyS)
                    then []
                    else top stack : (desapilar stack)

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 k stack = push k stack
insertarEnPos n k stack = push (top st) (insertarEnPos (n-1) k (pop stack))--}
