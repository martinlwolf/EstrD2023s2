--PUNTO 1
data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int

--1)
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ pizza) = 1 + (cantidadDeCapas pizza)

--2)
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i : is) = (Capa i (armarPizza is))

--3)
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pizza) = if (esJamon ing)
                                then sacarJamon pizza
                                else (Capa ing (sacarJamon pizza))

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True
esJamon _ = False

--4)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing pizza) = if ((esSalsa ing) || (esQueso ing))
                                            then tieneSoloSalsaYQueso pizza
                                            else False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

--5)
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing pizza) = if (esAceituna ing)
                                        then (Capa (aceitunasDuplicadas ing) (duplicarAceitunas pizza))
                                        else (Capa ing (duplicarAceitunas pizza))

--PRECOND: el ingrediente debe ser una aceituna
aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas n) = (Aceitunas (n*2))
aceitunasDuplicadas _ = error "no es una aceituna"

--6)
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p : ps) = ((cantidadDeCapas p), p) : (cantCapasPorPizza ps)

--PUNTO 2
data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
--1)
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = tieneTesoro cofre 
hayTesoro (Bifurcacion cofre map1 map2) = if (tieneTesoro cofre)
                                            then True
                                            else ((tieneTesoro map1) || (tieneTesoro map2))

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (n:ns) = if(esTesoro n)
                        then True
                        else tieneTesoro ns

--2)
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin cofre) = tieneTesoro cofre
hayTesoroEn [] (Bifurcacion cofre map1 map2) = tieneTesoro cofre
hayTesoroEn (d:ds) (Fin cofre) = False
hayTesoroEn (d:ds) (Bifurcacion cofre map1 map2) = if(esIzq d)
                                                    then hayTesoroEn ds map1
                                                    else hayTesoroEn ds map2

--3)
--PRECOND: Hay 1 tesoros
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Capa cofre map1 map2) = if(tieneTesoro cofre)
                                            then []
                                            else if (hayTesoro map1)
                                                then Izq : caminoAlTesoro map1
                                                else Der : caminoAlTesoro map2
