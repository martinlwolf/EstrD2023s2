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
sonMismoColor Rojo Rojo = False

--b)
poner :: Color -> Celda -> Celda
poner col celda = (Bolita col celda)

--c)
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar col celda = sacarBolitaSiEsDeColor col celda

sacarBolitaSiEsDeColor :: Color -> Celda -> Celda
sacarBolitaSiEsDeColor col (Bolita col2 celda) = if (sonMismoColor col col2)
                                                    then celda
                                                    else (Bolita col2 celda)

--d)
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n col celda = (Bolita col (ponerN (n-1) col celda)) 

--2)
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

--a)
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre obj camino) = if(tieneTesoro obj)
                                then True
                                else hayTesoro camino 
hayTesoro (Nada camino) = hayTesoro camino

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (n:ns) = if(esTesoro n)
                        then True
                        else tieneTesoro ns

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
hayTesoroEn n camino = hayTesoroEn (n-1) (caminoSinElPrimero camino)
hayTesoroEn 0 Fin = False 
hayTesoroEn 0 (Cofre objs camino) = tieneTesoro objs
hayTesoroEn 0 (Nada camino) = False

caminoSinElPrimero :: Camino -> Camino
caminoSinElPrimero Fin = Fin
caminoSinElPrimero (Cofre objs camino) = camino
caminoSinElPrimero (Nada camino) = camino

--d)
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros n Fin = False
alMenosNTesoros n (Cofre objs camino) = if(unoSi(tieneTesoro objs) + ) 
alMenosNTesoros n (Nada camino)    = 