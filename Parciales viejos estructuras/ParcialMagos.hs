module EscuelaDeMagia(fundarEscuela, estaVacia, registrar, magos, hechizosDe, leFaltanAprender, egresarUno, enseñar)

where

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

{-INV REP: 
    -Si un mago del map y de la priority queue tiene un hechizo, el mismo debe estar en el set de hechizos
    -No puede haber 2 magos con el mismo nombre en la PQ ni en el map
    -En el map, el nombre que se usa como clave debe ser exactamente el mismo que tenga el mago que tiene como valor
    -Los magos que esten en el map deben ser los mismos que estan en la priority queue y viceversa-}

fundarEscuela :: EscuelaDeMagia
--Propósito: Devuelve una escuela vacía.
--Eficiencia: O(1)
fundarEscuela = (EDM emptyS emptyM emptyPQ)

estaVacia :: EscuelaDeMagia -> Bool
--Propósito: Indica si la escuela está vacía.
--Eficiencia: O(1)
estaVacia (EDM setH mapM pqM) = isEmptyPQ pqM

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
--Eficiencia: O(log M)
registrar nom (EDM setH mapM pqM) = let magoN = crearM nom in
                                    case lookupM nom mapM of
                                    Just _ -> (EDM setH mapM pqM)
                                    Nothing -> (EDM setH (assocM nom magoN mapM) (insertPQ magoN pqM))

magos :: EscuelaDeMagia -> [Nombre]
--Propósito: Devuelve los nombres de los magos registrados en la escuela.
--Eficiencia: O(M)
magos (EDM setH mapM pqM) = domM mapM

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
magos n (EDM setH mapM pqM) = case lookupM n mapM of   
                            Just mago -> hechizos mago
                            Nothing -> error "El mago no existe"

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
leFaltanAprender n (EDM setH mapM pqM) = case lookupM n mapM of   
                            Just mago -> sizeS setH - sizeS (hechizos mago)
                            Nothing -> error "El mago no existe"

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
egresarUno (EDM setH mapM pqM) = if(not (isEmptyPQ pqM))
                                then (maxPQ pqM, (EDM setH (deleteM (maxPQ pqM) mapM) (deleteMaxPQ pqM)))
                                else error "No hay magos"

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)
enseñar h n (EDM setH mapM pqM) = let newM = (aprender h mago) in
                                 case lookupM n mapM of
                                Just mago -> (EDM (addS h setH) (assocM n newM mapM) (reemplazarAMago newM pqM))
                                Nothing -> error "El mago no existe"

reemplazarAMago :: Mago -> PriorityQueue Mago -> PriorityQueue Mago
reemplazarAMago m pq = if (m == maxPQ pq)
                        then insertPQ m (deleteMaxPQ pq)
                        else insertPQ (maxPQ pq) (reemplazarAMago m (deleteMaxPQ pq))
--Cost O(M log M) por hacerse las operaciones insertPQ y deleteMaxPQ, ambas de orden log M sobre el total de magos, y multiplicandose por M por
--tener que hacerse ambas operaciones por cada mago en el peor caso

-----------------------------------------------------------------------------------------------------------------------
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos e = hechizosDeTodos (magos e) e
--Propósito: Retorna todos los hechizos aprendidos por los magos.
--Eficiencia: O(M ∗ (log M + H log H))

hechizosDeTodos :: [Mago] -> EscuelaDeMagia -> Set Hechizo
hechizosDeTodos [] _ = emptyS
hechizosDeTodos (m:ms) e = unionS (hechizosDe m e) (hechizosDeTodos ms e)
--Costo O(M ∗ (log M + H log H)) siendo log M el costo de hechizosDe que busca sobre la lista de magos, H log H es por unir los 2 conjuntos
--de hechizos, y se multiplica por M por tener que hacerse estas operaciones por cada mago en la escuela

hayUnExperto :: EscuelaDeMagia -> Bool
--Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--Eficiencia: O(log M)
hayUnExperto e = leFaltanAprender (nombre(first(egresarUno e))) e == 0

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
--Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
--magos.
--Eficiencia: O(M log M)
egresarExpertos e = if (hayUnExperto e)
                    then let (m, esc) = egresarUno e in
                            (m : fst(egresarExpertos esc), snd(egresarExpertos esc))
                    else ([], e)
--Costo O(M log M) siendo log M el costo de hacer hayUnExperto y egresarUno sobre el total de magos de la escuela y se multiplica
-- por M por tener que hacer estas operaciones por cada mago de la escuela en el peor caso(todo expertos)
                    
