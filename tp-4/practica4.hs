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

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

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
hayTesoro (Fin cofre) = tieneTesoroElCofre cofre 
hayTesoro (Bifurcacion cofre map1 map2) = if (tieneTesoroElCofre cofre)
                                            then True
                                            else ((hayTesoro map1) || (hayTesoro map2))

tieneTesoroElCofre :: Cofre -> Bool
tieneTesoroElCofre (Cofre objs) = tieneTesoro objs

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (n:ns) = if(esTesoro n)
                        then True
                        else tieneTesoro ns

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2)
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin cofre) = tieneTesoroElCofre cofre
hayTesoroEn [] (Bifurcacion cofre map1 map2) = tieneTesoroElCofre cofre
hayTesoroEn (d:ds) (Fin cofre) = False
hayTesoroEn (d:ds) (Bifurcacion cofre map1 map2) = if(esIzq d)
                                                    then hayTesoroEn ds map1
                                                    else hayTesoroEn ds map2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

--3)
--PRECOND: Hay 1 tesoros
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Bifurcacion cofre map1 map2) = if(tieneTesoroElCofre cofre)
                                            then []
                                            else if (hayTesoro map1)
                                                then Izq : caminoAlTesoro map1
                                                else Der : caminoAlTesoro map2

--4)
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin cofre) = []
caminoDeLaRamaMasLarga (Bifurcacion cofre map1 map2) = if((altura map1) > (altura map2))
                                                    then (Izq : caminoDeLaRamaMasLarga map1)
                                                    else (Der : caminoDeLaRamaMasLarga map2)

altura :: Mapa -> Int
altura (Fin _) = 0
altura (Bifurcacion cofre mapa1 mapa2) = 1 + (altura mapa1) + (altura mapa2)

--5)
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre) = [tesorosDeCofre cofre]
tesorosPorNivel (Bifurcacion cofre map1 map2) = tesorosDeCofre cofre : (tesorosPorNivel map1 ++ tesorosPorNivel map2)

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre objs) = objs

--6)
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin cofre) = [[]]
todosLosCaminos (Bifurcacion cofre map1 map2) = (agregarACadaLista Izq (todosLosCaminos map1)) ++ (agregarACadaLista Der (todosLosCaminos map2))

agregarACadaLista :: a -> [[a]] -> [[a]]
agregarACadaLista x [] = [[x]]
agregarACadaLista x (ls:lss) = (x : ls) : (agregarACadaLista x lss)

--PUNTO 3
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)
--1)
sectores :: Nave -> [SectorId]
sectores (N treeSector) = sectorIdDeTodos treeSector

sectorIdDeTodos :: Tree Sector -> [SectorId]
sectorIdDeTodos EmptyT = []
sectorIdDeTodos (NodeT sector tree1 tree2) = (idDe sector) : ((sectorIdDeTodos tree1 ++ sectorIdDeTodos tree2))

idDe :: Sector -> SectorId
idDe (S id _ _) = id

--2)
poderDePropulsion :: Nave -> Int
poderDePropulsion (N treeSector) = poderDePropulsionEnSectores treeSector

poderDePropulsionEnSectores :: Tree Sector -> Int
poderDePropulsionEnSectores EmptyT = 0
poderDePropulsionEnSectores (NodeT sector tree1 tree2) = poderDePropulsionEnMotores (motoresDe (componentesDe sector)) + 
                                                                                    (poderDePropulsionEnSectores tree1) +
                                                                                    (poderDePropulsionEnSectores tree1)

poderDePropulsionEnMotores :: [Componente] -> Int
poderDePropulsionEnMotores [] = 0
poderDePropulsionEnMotores (m : ms) = (propulsion m) + (poderDePropulsionEnMotores ms)

componentesDe :: Sector -> [Componente]
componentesDe (S _ componentes _) = componentes

motoresDe :: [Componente] -> [Componente]
motoresDe [] = []
motoresDe (c : cs) = if(esMotor c)
                        then c : motoresDe cs
                        else motoresDe cs

esMotor :: Componente -> Bool
esMotor (Motor _) = True
esMotor _ = False

--PRECOND: El componente debe ser un motor
propulsion :: Componente -> Int
propulsion (Motor n) = n
propulsion _ = error "No es un motor"

--3)
barriles :: Nave -> [Barril]
barriles (N treeSector) = barrilesDeSectores treeSector

barrilesDeSectores :: Tree Sector -> [Barril]
barrilesDeSectores EmptyT = []
barrilesDeSectores (NodeT sector tree1 tree2) = barrilesDelSector sector ++ ((barrilesDeSectores tree1) ++ (barrilesDeSectores tree1))

barrilesDelSector :: Sector -> [Barril]
barrilesDelSector (S _ componentes _) = barrilesDeComponentes componentes

barrilesDeComponentes :: [Componente] -> [Barril]
barrilesDeComponentes [] = []
barrilesDeComponentes (c : cs) = if(esAlmacen c)
                                    then barrilesDe c
                                    else barrilesDeComponentes cs

--PRECOND: El componente debe ser un almacen
barrilesDe :: Componente -> [Barril]
barrilesDe (Almacen barriles) = barriles
barrilesDe _ = error "No es un almacen"

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _ = False

--4)
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector componentes id (N treeSector) = if(existeElSectorConId_En id treeSector)
                                                then (N (agregarComponentesAlSector componentes id treeSector))
                                                else (N treeSector)

existeElSectorConId_En :: SectorId -> Tree Sector -> Bool
existeElSectorConId_En _ EmptyT = False
existeElSectorConId_En id (NodeT sector tree1 tree2) = if ((idDe sector) == id)
                                                            then True
                                                            else (existeElSectorConId_En id tree1) || (existeElSectorConId_En id tree2) 

agregarComponentesAlSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarComponentesAlSector _ _ EmptyT = EmptyT
agregarComponentesAlSector comps id (NodeT sector tree1 tree2) = if((idDe sector) == id) 
                                                                    then (NodeT (componentesASector comps sector) tree1 tree2)
                                                                    else (NodeT sector (agregarComponentesAlSector comps id tree1) (agregarComponentesAlSector comps id tree2))

componentesASector :: [Componente] -> Sector -> Sector
componentesASector comps (S id comps2 trips) = (S id (comps ++ comps2) trips)

--5)

-- No supe como hacerla parcial
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA trip ids (N treeSector) = (N (asignarTripulanteASectores trip ids treeSector))

asignarTripulanteASectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteASectores _ _ EmptyT = EmptyT
asignarTripulanteASectores trip ids (NodeT sector tree1 tree2) = (NodeT (agregarTripulanteSiTieneIdCorrecto trip ids sector) (asignarTripulanteASectores trip ids tree1) 
                                                                                                                            (asignarTripulanteASectores trip ids tree2))

agregarTripulanteSiTieneIdCorrecto :: Tripulante -> [SectorId] -> Sector -> Sector
agregarTripulanteSiTieneIdCorrecto trip [] sector = sector
agregarTripulanteSiTieneIdCorrecto trip (i:is) sector = if(sonMismoId i (idDe sector))
                                                                    then(S i (componentesDe sector) (agregarTripulanteALista trip (tripulantesDe sector)))
                                                                    else agregarTripulanteSiTieneIdCorrecto trip is sector

sonMismoId :: SectorId -> SectorId -> Bool
sonMismoId sector1 sector2 = sector1 == sector2

tripulantesDe :: Sector -> [Tripulante]
tripulantesDe (S _ _ trips) = trips

agregarTripulanteALista :: Tripulante -> [Tripulante] -> [Tripulante]
agregarTripulanteALista trip trips = trip : trips

--6)
{--data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)--}
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados trip (N treeSector) = sectoresAsignadosEnArbol trip treeSector

sectoresAsignadosEnArbol :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosEnArbol _ EmptyT = []
sectoresAsignadosEnArbol trip (NodeT sector tree1 tree2) = agregarIdAListaSiApareceEn trip sector ((sectoresAsignadosEnArbol trip tree1) ++ 
                                                                                                    (sectoresAsignadosEnArbol trip tree1))
                                                                                                
agregarIdAListaSiApareceEn :: Tripulante -> Sector -> [SectorId] -> [SectorId] 
agregarIdAListaSiApareceEn trip (S id _ listaTrip) listaId= if(estaElTripulanteEn trip listaTrip)
                                                        then id : listaId
                                                        else listaId

estaElTripulanteEn :: Tripulante -> [Tripulante] -> Bool
estaElTripulanteEn trip [] = False
estaElTripulanteEn trip (t:ts) = (trip == t) || (estaElTripulanteEn trip ts)

--7)
tripulantes :: Nave -> [Tripulante]
tripulantes (N treeSector) =  tripulantesDeSectores treeSector

tripulantesDeSectores :: Tree Sector -> [Tripulante]
tripulantesDeSectores EmptyT = []
tripulantesDeSectores (NodeT sector tree1 tree2) = agregarLosTripulantesDelSector_QueNoEstenEnLaLista sector 
                                                        ((tripulantesDeSectores tree1) ++ (tripulantesDeSectores tree1))

agregarLosTripulantesDelSector_QueNoEstenEnLaLista :: Sector -> [Tripulante] -> [Tripulante]
agregarLosTripulantesDelSector_QueNoEstenEnLaLista (S _ _ trips1) trips2 = agregarLosTripulantesDe_QueNoEstenEn trips1 trips2

agregarLosTripulantesDe_QueNoEstenEn :: [Tripulante] -> [Tripulante] -> [Tripulante]
agregarLosTripulantesDe_QueNoEstenEn [] trips = trips
agregarLosTripulantesDe_QueNoEstenEn (t:ts) trips = if(noEstaEn t trips)
                                                        then agregarLosTripulantesDe_QueNoEstenEn ts (t:trips)
                                                        else agregarLosTripulantesDe_QueNoEstenEn ts trips

noEstaEn :: Tripulante -> [Tripulante] -> Bool
noEstaEn _ [] = True
noEstaEn trip (t:ts) = not(trip == t) && (noEstaEn trip ts)

--PUNTO 4
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo
--1)
exp1 = Explorador "n" ["t1","t2"] (Cria "a") (Cria "a")
exp2 = Explorador "n2" ["t1","t2"] (Cria "a") (Cria "a")
cazador = Cazador "n" ["p1","p2","p3"] exp1 exp2 (Cria "a")

--2)

buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = (cantidadAlimentoCazado lobo) > (cantidadDeCrias lobo)

cantidadAlimentoCazado :: Lobo -> Int
cantidadAlimentoCazado (Cria _) = 0
cantidadAlimentoCazado (Explorador _ _ lobo1 lobo2) = (cantidadAlimentoCazado lobo1) + (cantidadAlimentoCazado lobo2)
cantidadAlimentoCazado (Cazador _ presas lobo1 lobo2 lobo3) = (length presas) + (cantidadAlimentoCazado lobo1) + (cantidadAlimentoCazado lobo2)
                                                                                       + (cantidadAlimentoCazado lobo3) 

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cria _) = 0
cantidadDeCrias (Explorador _ _ lobo1 lobo2) = (cantidadDeCrias lobo1) + (cantidadDeCrias lobo2)
cantidadDeCrias (Cazador _ presas lobo1 lobo2 lobo3) = (length presas) + (cantidadDeCrias lobo1) + (cantidadDeCrias lobo2)
                                                                                       + (cantidadDeCrias lobo3)

--3)
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaDe lobo

elAlfaDe :: Lobo -> (Nombre,Int)
elAlfaDe (Cria n) = (n,0)
elAlfaDe (Explorador n territorios lobo1 lobo2) = elAlfaEntre (elAlfaEntre (elAlfaDe lobo1) (elAlfaDe lobo2)) (n,0)
elAlfaDe (Cazador n presas lobo1 lobo2 lobo3)   = elAlfaEntre (elAlfaEntre (elAlfaEntre (elAlfaDe lobo1) (elAlfaDe lobo2)) (elAlfaDe lobo3)) (n,(length presas))

elAlfaEntre :: (Nombre,Int) -> (Nombre,Int) -> (Nombre,Int)
elAlfaEntre (n1,int1) (n2,int2) = if (int1 > int2)
                                    then (n1,int1)
                                    else (n2,int2)

--4)
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M lobo) = losNombresDeLobosQueExploraron t lobo

losNombresDeLobosQueExploraron :: Territorio -> Lobo -> [Nombre]
losNombresDeLobosQueExploraron t (Cria _) = []
losNombresDeLobosQueExploraron t (Explorador n territorios lobo1 lobo2) = agregarNombreASiExploro n t territorios ((losNombresDeLobosQueExploraron t lobo1) ++
                                                                                                        (losNombresDeLobosQueExploraron t lobo2))
losNombresDeLobosQueExploraron t (Cazador n _ lobo1 lobo2 lobo3) = ((losNombresDeLobosQueExploraron t lobo1) ++
                                                                    (losNombresDeLobosQueExploraron t lobo2)) ++ (losNombresDeLobosQueExploraron t lobo3)

agregarNombreASiExploro :: Nombre -> Territorio -> [Territorio] -> [Nombre] -> [Nombre]
agregarNombreASiExploro nom t territorios nombres = if (pertenece t territorios)
                                                        then nom : nombres
                                                        else nombres
--5)
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = exploradoresPorTerritorioEnLobos lobo

exploradoresPorTerritorioEnLobos :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioEnLobos (Cria n) = []
exploradoresPorTerritorioEnLobos (Explorador n territorios lobo1 lobo2) = agregarExplorador n territorios
                                                                      (juntarTerritorio (exploradoresPorTerritorioEnLobos lobo1)
                                                                                      (exploradoresPorTerritorioEnLobos lobo2))
exploradoresPorTerritorioEnLobos (Cazador _ _ lobo1 lobo2 lobo3)   = juntarTerritorio( (exploradoresPorTerritorioEnLobos lobo3)
                                                                    (juntarTerritorio (exploradoresPorTerritorioEnLobos lobo1)
                                                                                      (exploradoresPorTerritorioEnLobos lobo2)))

agregarExplorador :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarExplorador n [] lss = lss
agregarExplorador n (t:ts) lss = agregarATerreno n t (agregarExplorador n ts lss)

agregarATerreno :: Nombre -> Territorio -> [(Territorio, [Nombre])]
agregarATerreno n t [] = [(t,n)]
agregarATerreno n t ((t2,ns) : tss) = if (t == t2)
                                        then (t, n:ns) : tss
                                        else (t, ns) : agregarATerreno n t tss
juntarTerritorio :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarTerritorio [] ls = ls
juntarTerritorio ls [] = ls
juntarTerritorio ((t,ns) : tss) tss2 =  agregarTerritorioConNombres (t,ns) (juntarTerritorio tss tss2)

agregarTerritorioConNombres :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarTerritorioConNombres tn [] = [tn]
agregarTerritorioConNombres (t, ns) ((t2,ns2):tss) = if (t==t2)
                                                      then (t,(appendSinReps ns ns2)) : tss
                                                      else (t2,ns2) : agregarTerritorioConNombres (t,ns) tss

appendSinReps :: Eq a => [a] -> [a] -> [a]
appendSinReps (x:xs) ys = if (pertenece x ys)
                                then (appendSinReps xs ys) 
                                else x : (appendSinReps xs ys) 
                                
pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (n:ns) = (n==k) || (pertenece k ns)
                            

--6)
{--type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo--}

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M lobo) = losLobosQueTienenDeSubordinadoA n lobo

losLobosQueTienenDeSubordinadoA :: Nombre -> Lobo -> [Nombre]
losLobosQueTienenDeSubordinadoA n (Cria _) = []
losLobosQueTienenDeSubordinadoA n (Explorador n_ _ lobo1 lobo2)         = []
losLobosQueTienenDeSubordinadoA n  (Cazador n2 _ lobo1 lobo2 lobo3)     = singularSi n (esCazadorConNombre n lobo1 ||
                                                                                        esCazadorConNombre n lobo2 ||
                                                                                        esCazadorConNombre n lobo3) ++  (losLobosQueTienenDeSubordinadoA n lobo1) ++
                                                                                                                    (losLobosQueTienenDeSubordinadoA n lobo2) ++
                                                                                                                    (losLobosQueTienenDeSubordinadoA n lobo3)

singularSi :: a -> Bool -> [a]
singularSi a True = [a]
singularSi a False = []

esCazadorConNombre :: Nombre -> Lobo -> Bool
esCazadorConNombre n (Cria _) = False
esCazadorConNombre n (Explorador _ _ _ _)    = False
esCazadorConNombre n (Cazador n2 _ _ _ _) = n == n2