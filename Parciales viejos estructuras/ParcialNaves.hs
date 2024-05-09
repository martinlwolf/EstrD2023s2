module Nave (construir, ingresarT, sectoresAsignados, datosDeSector, tripulantesN, agregarASector, asignarASector)

where

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{-INV REP:
    -Si hay un tripulante en el map Nombre Tripulante, el nombre dentro del valor debe ser exactamente el mismo que tiene como clave
    y viceversa
    -Todos los tripulantes que esten en el map deben ser exactamente los mismos que estan en el MaxHeap de tripulantes y viceversa
    -Los sectores que esten como valor en el primer map, deben estar asociados a un sector id igual al que tienen ellos
    -Si hay un sector en el primer map con tripulantes, el mismo debe aparecer asignado a todos los tripulantes
    que tenga en el segundo map y en el maxheap-}

construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S log S)
construir secs = (N (construirSectores secs) emptyM emptyH)

construirSectores :: [SectorId] -> Map SectorId Sector
construirSectores [] = emptyM
construirSectores (s:ss) = assocM s (crearS s) (construirSectores ss)
--Costo O(S log S) siendo log S el costo de hacer assocM sobre el map de sectores y se multiplica por la misma S por tener que hacer
--esta operacion por cada SectorId


ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(log T)
ingresarT n r (N mapS mapT mH) = let newT = (crearT n r) in
                                (N mapS (assocM n newT mapT) (insertH newT mH))
--Costo O(log T) siendo log T el costo de hacer assocM sobre el map de tripulantes


sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log T)
sectoresAsignados n (N mapS mapT mH) = case lookupM n mapT of
                                    Just trip -> sectoresT trip
                                    Nothing -> error "El tripulante no existe"


datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
datosDeSector sid (N mapS mapT mH) = case lookupM sid mapS of
                                Just sector -> (tripulantesS sector, componentesS sector)
                                Nothing -> error "El sector no existe"

            
tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(T log T)
tripulantesN (N mapS mapT mH) = if (not (isEmptyH mH))
                                then (maxH mH): tripulantesN (deleteMaxH mH)
                                else []


agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector comps sid (N mapS mapT mH) = (N (agregarComponentesA sid comp mapS) mapT mH) 

agregarComponentesA :: SectorId -> [Componente] -> Map SectorId Sector -> Map SectorId Sector
agregarComponentesA sid comps map = case lookupM c map of
                                Just sec -> assocM sid (agregarComponentes comps sec) map
                                Nothing -> error "No esta el sector"
--Costo O(C + log S) siendo C el costo de hacer agregarComponentes y log S el costo de hacer assocM sobre el total de sectores

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes [] sec = sec
agregarComponentes (c:cs) sec = agregarC c sec
--Costo O(C) siendo C el resultado de hacer agregarC de costo constante por cada componente de la lista


asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
asignarASector n sid (N mapS mapT mH) = case lookupM sid mapS of
                                        Just sec ->let newTrip = (asignarleSectorA sid n mapT) in
                                                (N (agregarTripulante newTrip sid mapS) (assocM n newTrip mapT) (reemplazarEnMaxHeap newTrip mH))
                                        Nothing -> error "El sector no esta"

asignarleSectorA :: SectorID -> Nombre -> (Map Nombre Tripulante) -> Tripulante
asignarleSectorA sid n mapT = case lookupM n mapT of
                            Just trip -> (asignarS sid trip)
                            Nothing -> error "El tripulante no esta"
--Costo O(log T + log S) siendo log T el costo de hacer lookupM y assocM sobre el map de tripulantes y log S por hacer asignarS sobre
-- los sectores del tripulante

agregarTripulante :: Tripulante -> SectorId -> (Map SectorId Sector) -> (Map SectorId Sector)
agregarTripulante trip sid mapS = assocM sid (agregarT trip sec) mapS
--Costo O(log S) por hacer assocM sobre el map de sectores

reemplazarEnMaxHeap :: Tripulante -> MaxHeap -> MaxHeap
reemplazarEnMaxHeap trip mH = if (trip == maxH mH)
                            then insertH trip (deleteMaxH mH)
                            else insertH (maxH mH) (reemplazarEnMaxHeap trip (deleteMaxH mH))
--Costo O(T log T) siendo log T el costo de hacer insertH y deleteMaxH sobre el maxheap de tripulantes y se multiplica por T por tener
-- que hacer cada operacion por cada elemento del maxHeap

-------------------------------------------------------------------------------------------------------------------------------------

sectores :: Nave -> Set SectorId
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados)
sectores nave = sectoresNoVacios (tripulantesN nave) nave

sectoresNoVacios :: [Tripulante] -> Nave -> Set SectorId
sectoresNoVacios [] nave = emptyS
sectoresNoVacios (t:ts) nave = unionS (sectoresAsignados (nombre t) nave) (sectoresNoVacios ts nave)
-- Costo O(T * (S log S + log T)) siendo S log S el costo de hacer unionS sobre sectoresAsignados y sectores no vacios, log T por hacer
-- sectoresAsignados sobre los tripulantes, y se multiplica por T por hacer estas operaciones por cada tripulante de la lista


sinSectoresAsignados :: Nave ->[Tripulante]
--Propósito: Devuelve los tripulantes que no poseen sectores asignados
sinSectoresAsignados nave = losSinSectores (tripulantesN nave)

losSinSectores :: [Tripulante] -> [Tripulante]
losSinSectores [] n = []
losSinSectores (t:ts) n = if(isEmptyS (sectoresT t))
                            then t : losSinSectores ts
                            else losSinSectores ts
-- Costo O(T log T) siendo T log T el costo de hacer tripulantesN. El costo T de losSinSectores se simplifica por existir un costo mayor

barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles nave = todosLosBarriles (setToList(sectoresAsignados nave)) nave

todosLosBarriles :: [SectorId] -> Nave -> [Barril]
todosLosBarriles [] nave = []
todosLosBarriles (s:ss) nave = let (sn, comps) = datosDeSector s nave in
                            losBarrilesDe comps : (todosLosBarriles ss nave)
--Costo O(S * (log s + C)), siendo log s el costo de hacer datosDeSector sobre el total de sectores de la nave, C por hacer losBarrilesDe
-- y se multiplica por S por hacer estas operaciones por cada elemento de la lista de SectorId

losBarrilesDe :: [Componente] -> [Barril]
losBarrilesDe [] = []
losBarrilesDe (c:cs) = barrilesDe c : (losBarrilesDe cs)
--Costo O(C) siendo C el total de componentes de la lista, el costo es por hacer una op constante (barrilesDe) por cada componente

barrilesDe :: Componente -> [Barril]
barrilesDe (Almacen barriles) = barriles
barrilesDe _ = []
--Costo O(1) el pattern matching es constante

