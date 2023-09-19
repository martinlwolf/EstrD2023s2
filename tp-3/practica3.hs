--PUNTO 1)
--1)
data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia
--a)
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas col1 (Bolita col2 celda) = unoSi(sonMismoColor col1 col2) + nroBolitas col1 celda 

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True
sonMismoColor Rojo Rojo = True
sonMismoColor _ _ = False

--b)
poner :: Color -> Celda -> Celda
poner col celda = (Bolita col celda)

--c)
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar col (Bolita col2 celda) =  if (sonMismoColor col col2)
                                    then celda
                                    else Bolita col2 (sacar col celda)


sacarBolitaSiEsDeColor :: Color -> Celda -> Celda
sacarBolitaSiEsDeColor col (Bolita col2 celda) = if (sonMismoColor col col2)
                                                    then celda
                                                    else (Bolita col2 celda)

--d)
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n col celda = (poner col (ponerN (n-1) col celda)) 

--2)
data Objeto = Cacharro | Tesoro
        deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
        deriving Show

cam = Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Tesoro] (Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Tesoro] (Fin))))))

--a)
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre obj camino) = tieneTesoro obj || hayTesoro camino 
hayTesoro (Nada camino) = hayTesoro camino

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (n:ns) = esTesoro n || tieneTesoro ns

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--b)
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objs camino) = if (tieneTesoro objs)
                                            then 0
                                            else 1 + pasosHastaTesoro camino
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino

--c)
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 Fin = False 
hayTesoroEn 0 (Cofre objs camino) = tieneTesoro objs
hayTesoroEn 0 (Nada camino) = False
hayTesoroEn n (Cofre objs camino) = hayTesoroEn (n-1) camino
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino

caminoSinElPrimero :: Camino -> Camino
caminoSinElPrimero Fin = Fin
caminoSinElPrimero (Cofre objs camino) = camino
caminoSinElPrimero (Nada camino) = camino

--d)
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Cofre objs camino) = alMenosNTesoros (restaEspecial n (cuantosSonTesoro objs)) camino
alMenosNTesoros n (Nada camino)    = alMenosNTesoros n camino 

restaEspecial :: Int -> Int -> Int
restaEspecial n n2 = if(n < n2)
                        then 0
                        else n - n2

--e)
--PRECOND: el primer numero debe ser menor o igual al segundo
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 n camino = cantTesorosHasta n camino
cantTesorosEntre n1 n2 Fin = 0 
cantTesorosEntre n1 n2 (Cofre _ camino) = cantTesorosEntre ((n1)-1) ((n2)-1) camino
cantTesorosEntre n1 n2 (Nada camino) = cantTesorosEntre ((n1)-1) ((n2)-1) camino

cantTesorosHasta :: Int -> Camino -> Int
cantTesorosHasta 0 camino = cantidadDeTesorosDelCamino camino
cantTesorosHasta n Fin = 0
cantTesorosHasta n (Cofre objs camino) = cuantosSonTesoro objs + cantTesorosHasta (n-1) (camino)
cantTesorosHasta n (Nada camino)       = cantTesorosHasta (n-1) (camino)

cuantosSonTesoro :: [Objeto] -> Int
cuantosSonTesoro [] = 0 
cuantosSonTesoro (o:os) = if(esTesoro o)
                                then 1 + cuantosSonTesoro os
                                else cuantosSonTesoro os

cantidadDeTesorosDelCamino :: Camino -> Int
cantidadDeTesorosDelCamino Fin = 0
cantidadDeTesorosDelCamino (Cofre objs camino) = cuantosSonTesoro objs
cantidadDeTesorosDelCamino (Nada camino) = 0

--PUNTO 2
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
            deriving Show
t1 = NodeT (2::Int) (NodeT (4::Int) (NodeT (6::Int) EmptyT EmptyT) EmptyT) (NodeT (2::Int) EmptyT (NodeT (4::Int) EmptyT EmptyT))
t2 = NodeT (2::Int) EmptyT (NodeT (4::Int) EmptyT EmptyT)
--1)
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n tree1 tree2) = n + (sumarT tree1) + (sumarT tree2)

--2)
sizeT :: Tree a -> Int
sizeT EmptyT = 1
sizeT (NodeT a tree1 tree2) = 1 + (sizeT tree1) + (sizeT tree2)

--3)
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n tree1 tree2) = (NodeT (2*n) (mapDobleT tree1) (mapDobleT tree2))

--4)
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a EmptyT = False
perteneceT a (NodeT b tree1 tree2) = (a == b) || (perteneceT a tree1) || (perteneceT a tree2)

--5)
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a EmptyT = 0
aparicionesT a (NodeT b tree1 tree2) = unoSi(a == b) + (aparicionesT a tree1) + (aparicionesT a tree2)

--6)
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT a tree1 tree2) = if((isEmpty tree1) && (isEmpty tree2))
                                then [a]
                                else  ((leaves tree1) ++ (leaves tree2))--}

isEmpty :: Tree a -> Bool
isEmpty  EmptyT = True
isEmpty (NodeT _ _ _) = False

--7)
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT a tree1 tree2) = 1 + (max (heightT tree1) (heightT tree2))

--8)
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT a tree1 tree2) = NodeT a (mirrorT tree2) (mirrorT tree1)

--9)
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT a tree1 tree2) = (toList tree1) ++ [a] ++ (toList tree2)

--10)
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT a tree1 tree2) = [a]
levelN n  (NodeT a tree1 tree2) = (levelN (n-1) tree1) ++ (levelN (n-1) tree2)

--11)
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT a tree1 tree2) = [a] : (juntarListasDeListas (listPerLevel tree1) (listPerLevel tree2))

juntarListasDeListas :: [[a]] -> [[a]] -> [[a]]
juntarListasDeListas [] lss = lss
juntarListasDeListas lss [] = lss
juntarListasDeListas (xs:xss) (ys:yss) = (xs ++ ys) : juntarListasDeListas xss yss 

--12)
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT a tree1 tree2) = toList(laMasLargaDe tree1 tree2)

laMasLargaDe :: Tree a -> Tree a -> Tree a
laMasLargaDe EmptyT tr = tr
laMasLargaDe tr EmptyT = tr
laMasLargaDe tree1 tree2 = if ((heightT tree1) > (heightT tree2))
                                then tree1
                                else tree2

--13)
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT a tree1 tree2) = [a] : ((agregarACadaLista a ((todosLosCaminos tree1))++ (agregarACadaLista a(todosLosCaminos tree2))))

agregarACadaLista :: a -> [[a]] -> [[a]]
agregarACadaLista x [] = []
agregarACadaLista x (ls:lss) = (x : ls) : (agregarACadaLista x lss)


--PUNTO 2.2
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
                deriving Show
--1)
eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum exp1 exp2) = (eval exp1) + (eval exp2)
eval (Prod exp1 exp2) = (eval exp1) * (eval exp2)
eval (Neg exp) = (-1) * (eval exp)

--2)
simplificar :: ExpA -> ExpA
simplificar (Valor n) = (Valor n)
simplificar (Sum exp1 exp2) = simplSuma (simplificar exp1) (simplificar exp2)
simplificar (Prod exp1 exp2) = simplProd (simplificar exp1) (simplificar exp2)
simplificar (Neg exp) = (simplNeg (simplificar exp))

simplSuma :: ExpA -> ExpA -> ExpA
simplSuma (Valor 0) exp2 = exp2
simplSuma exp1 (Valor 0) = exp1
simplSuma exp1 exp2 = Sum (exp1) (exp2)

simplProd :: ExpA -> ExpA -> ExpA
simplProd (Valor 0) exp2 = (Valor 0)
simplProd exp1 (Valor 0) = (Valor 0)
simplProd (Valor 1) exp2 = exp2
simplProd exp1 (Valor 1) = exp1
simplProd exp1 exp2 = Prod (exp1) (exp2)

simplNeg :: ExpA -> ExpA
simplNeg (Neg exp) = exp
simplNeg exp = (Neg exp)

esNeg :: ExpA -> Bool
esNeg (Neg _) = True
esNeg _ = False
