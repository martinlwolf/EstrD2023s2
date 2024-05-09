module Nave (naveVacia, tripulantesDe, sectores, conMayorRango, conMasTripulantes, conRango, sectorDe, agregarTripulante)

where

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

{-INV REP:
    -Si hay un tripulante dentro de un set de un sector, el mismo debe estar en el heap de tripulantes
    -Un tripulante que este dentro de un set no puede estar en ningun otro set de otro sector
    -Si hay uno o mas sectores en el map, la tupla Sector Int debe tener el sector del map que tenga
    mas tripulantes y el entero debe ser el tamaño del set de tripulantes que tenga ese sector como valor en el map-}

naveVacia :: [Sector] -> Nave
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia [] = error "Lista vacia"
naveVacia secs = (MkN (asociarTodos secs emptyM) emptyH (s,0))

asociarTodos :: [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
asociarTodos [] map = map
asociarTodos (s:ss) map = assocM s emptyS (asociarTodos ss map)
--Costo O(S log S) siendo log S el costo de hacer assocM sobre el map de sectores y se multiplica por S por hacerlo por cada elemento
--de la lista de sectores


tripulantesDe :: Sector -> Nave -> Set Tripulante
--Propósito: Obtiene los tripulantes de un sector.
--Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe sec (MkN mapS hT tup) = lookupM sec mapS


sectores :: Nave -> [Sector]
--Propósito: Denota los sectores de la nave
--Costo: O(S) siendo S la cantidad de sectores.
sectores (MkN mapS hT tup) = domM mapS


conMayorRango :: Nave -> Tripulante
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
--Costo: O(1).
conMayorRango (MkN mapS hT tup) = findMin hT


conMasTripulantes :: Nave -> Sector
--Propósito: Denota el sector de la nave con más tripulantes.
--Costo: O(1).
conMasTripulantes (MkN mapS hT tup) = fst(tup)


conRango :: Rango -> Nave -> Set Tripulante
--Propósito: Denota el conjunto de tripulantes con dicho rango.
--Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango r (MkN mapS hT tup) = if (not(isEmptyH hT))
                            then if (rango (findMin hT) == r)
                                then addS (findMin hT) (conRango r (deleteMin hT))
                                else (conRango r (deleteMin hT))
                            else emptyH


sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectores trip (MkN mapS hT tup) = sectorDeTripulante trip (sectores mapS) mapS

sectorDeTripulante :: Tripulante -> [Sector] -> Map (Set Tripulante) -> Sector
sectorDeTripulante trip [] mapS= error "El tripulante no esta en la nave"
sectorDeTripulante trip (s:ss) mapS = if(belongs s (lookupM s mapS))
                                    then s
                                    else sectorDeTripulante trip ss mapS
--Costo O(S * (log S + log P)) siendo log P el costo de hacer belongs sobre el set de tripulantes, log S el costo de hacer lookupM 
--sobre el map de Sector y se multiplica por S por hacer las operaciones por cada elemento de la lista de sectores


agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
--Costo: No hay datos (justifique su elección).
agregarTripulante trip sec (MkN mapS hT tup) = case lookupM sec mapS of
                                            Just setT -> let newSet = (addS trip setT) in
                                                            (MkN (assocM sec newSet mapS) (insertH trip hT) (actualizarTupla sec newSet tup))
                                            Nothing -> error "El sector no esta"
--Costo O(log S + log T + log t) siendo log S el costo de hacer lookupM y assocM sobre el map de Sector, siendo log T el costo de hacer insertH
--sobre el heap de tripulantes y log t por hacer addS sobre el set de tripulantes correspondiente al sector dado

actualizarTupla :: Sector -> Set Tripulante -> (Sector, Int) -> (Sector, Int)
actualizarTupla secN setTN (secV, cant) = if (sizeS setTN > cant)
                                            then (secN, (sizeS setTN))
                                            else (secV, cant)
--Costo O(1)



tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
tripulantes nave = losTripulantesDe (sectores nave) nave

losTripulantesDe :: [Sector] -> Nave -> Set Tripulante
losTripulantesDe [] nave = emptyS
losTripulantesDe (s:ss) nave = unionS (tripulantesDe s nave) (losTripulantesDe ss nave)
--Costo (S * (T log t)) siendo T log t el costo de hacer la union sobre los sets de tripulantes, T es el set de tripulantes de tripulantesDe
-- y t es el set con el resto de tripulantes. 