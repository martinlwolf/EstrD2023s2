--PUNTO 1
-- 1)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n : ns) = n + sumatoria ns

-- 2)
longitud :: [a] -> Int
longitud [] = 0
longitud (n : ns) = 1 + longitud ns

-- 3)
-- PRECOND: La lista no debe ser vacia
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n : ns) = (n+1) : sucesores ns

-- 4)
-- PRECOND: La lista no debe ser vacia
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (n : ns) = if (n)
                        then conjuncion ns
                        else False

conjuncionPM :: [Bool] -> Bool
conjuncionPM [] = True
conjuncionPM (True : ns) = conjuncionPM ns
conjuncionPM _ = False

-- 5)
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (False:ns) = disyuncion ns
disyuncion _ = True

{-- 6)
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (n:ns) = if null ns
                    then aplanar n
                    else n

aplanarPM :: [[a]] -> [a]
aplanarPM [] = []
aplanarPM (n:[]) = aplanarPM n-}

--7)
pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (n:ns) = if (n==k)
                        then True
                        else pertenece k ns

pertenecePM :: Eq a => a -> [a] -> Bool
pertenecePM k [] = False
pertenecePM k (n:ns) = k == n
pertenecePM k (_:ns) = pertenecePM k ns

--8)
apariciones :: Eq a => a -> [a] -> Int
apariciones k [] = 0
apariciones k (n:ns) = unoSiCeroSiNo (k == n) + apariciones k ns

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo True = 1
unoSiCeroSiNo False = 0

--9)
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA  k [] = []
losMenoresA k (n:ns) = if (n<k)
                        then n : losMenoresA k ns
                        else losMenoresA k ns

--10)
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA k [] = [] 
lasDeLongitudMayorA k (n:ns) = if ((longitud n) > k)
                                then n : lasDeLongitudMayorA k ns
                                else lasDeLongitudMayorA k ns

{--11)
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] =
agregarAlFinal (n:ns) k = agregarAlFinal ns k-}

--12)
agregar :: [a] -> [a] -> [a]
agregar [] [] = []
agregar [] l = l
agregar l [] = l
agregar (x:xs) l = x : agregar xs l

{--13)
reversa :: [a] -> [a]
reversa [] = []
reversa (n:ns) = -}

--14)
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x:xs) (y:ys) = maximoEntre x y : zipMaximos xs ys

maximoEntre :: Int -> Int -> Int
maximoEntre x y = if (x>y)
                    then x
                    else y

--PUNTO 2
--1)
-- PRECOND: el numero debe ser positivo
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--2)
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--3)
repetir :: Int -> a -> [a]
repetir 0 k = []
repetir n k = k : repetir (n-1) k

--4)
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros k (n:ns) = n : losPrimeros (k-1) ns

--5)
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 l = l
sinLosPrimeros _ [] = []
sinLosPrimeros k (n:ns) = sinLosPrimeros (k-1) ns

--PUNTO 3
--1)
data Persona = P String Int
        deriving Show

edad :: Persona -> Int
edad (P n e) = e
--a)
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA k (n:ns) = if ((edad n) > k)
                        then n : mayoresA k ns
                        else mayoresA k ns

--b)
promedioEdad :: [Persona] -> Int
promedioEdad [] = 0
promedioEdad personas = div (sumatoria (listaDeEdades personas)) (longitud personas)

listaDeEdades :: [Persona] -> [Int]
listaDeEdades [] = []
listaDeEdades (n:ns) = (edad n) : listaDeEdades ns

--c)
elMasViejo :: [Persona] -> Persona
elMasViejo [] = P "default" 0
elMasViejo (n:ns) = if ((edad n) > (edad (elMasViejo ns)))
                        then n
                        else elMasViejo ns