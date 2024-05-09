module Organizador (Organizador, nuevo, agregarPrograma, agregarPrograma, agregarPrograma, programasDe, programaronJuntas)

where

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

{-INV.REP:  - Si existe un checksum en el primer map con un set de personas como valor, todas las personas de ese set deben estar como clave
            en el segundo map, teniendo dentro de su set de checksums al checksum que tienen como clave en el primer map.
            - Si existe una persona en el segundo map y tiene un set de checksums, todos los checksums de ese set deben estar en el primer
            map como clave, teniendo dentro de sus sets a esa persona.
            - Una misma persona no puede aparecer 2 o mas veces dentro de un mismo set para un mismo checksum en el 1er map, asi como tampoco
            un checksum puede aparecer 2 o mas veces dentro de un mismo set para una misma persona en el 2do map.-}

nuevo :: Organizador
--Propósito: Un organizador vacío.
--Eficiencia: O(1)
nuevo = (MkO emptyM emptyM)

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (MkO mapC mapP ) ch setP = case lookupM mapC ch of
                                    Just s -> error "El programa ya esta cargado"
                                    Nothing -> (MkO (assocM ch setP mapC) (agregarProgramaAPersonasEn (setToList setP) ch mapP))
--Costo O(p * (log P + log c) + log C) siendo log C el costo de recorrer el map de checksums con lookupM y assocM y simplificandose
--del setToList por ser las mismas personas que el p proveniente de la funcion agregarProgramaAPersonasEn.

agregarProgramaAPersonasEn :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
agregarProgramaAPersonasEn [] _ mapP = mapP
agregarProgramaAPersonasEn (p:ps) ch mapP = case lookupM p mapP of
                                        Just setCh -> assocM p (addS ch setCh) (agregarProgramaAPersonasEn ps ch mapP)
                                        Nothing -> assocM p (addS ch emptyS) (agregarProgramaAPersonasEn ps ch mapP)
--Costo O(p * (log P + log c)). Siendo log P el costo de recorrer el map de personas en lookupM y assocM, log c el costo de recorrer 
--el set de checksum con addS, y multiplicando todo por p por tener que realizarse por cada elemento de la lista de personas


todosLosProgramas :: Organizador -> [Checksum]
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MkO mapC _) = domM mapC


autoresDe :: Organizador -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MkO mapC mapP) ch = case lookupM ch mapC of
                            Just setP -> setP
                            Nothing -> error "El programa no esta"


programasDe :: Organizador -> Persona -> Set Checksum
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MkO mapC mapP) p = case lookupM p mapP of
                                Just setC -> setC
                                Nothing -> error "La persona no esta"


programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas.
--Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
programaronJuntas (MkO mapC mapP) p1 p2 = if(p1 == p2)
                                        then error "Las personas no pueden ser iguales"
                                        else notNull(intersection (lookupM p1 mapP) (lookupM p2 mapP))


nroProgramasDePersona :: Organizador -> Persona -> Int
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona (MkO _ mapP) p = sizeS (lookupM p mapP)

-----------------------------------------------------------------------------------------------------------------------------

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
--Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
--programaron juntas.
programasEnComun p1 p2 org = intersection (programasDe org p1) (programasDe org p2)
--Costo O(log P + p log p) siendo log P el costo de realizar programasDe sobre los programas de una persona y p log p el costo de
--la interseccion por encontrar los elementos comunes entre los 2 sets de programas


esUnGranHacker :: Organizador -> Persona -> Bool
--Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
esUnGranHacker org p = laPersonaParticipoEnTodosLosProgramasDe p (todosLosProgramas org) org

laPersonaParticipoEnTodosLosProgramasDe :: Persona -> [Checksum] -> Organizador -> Bool
laPersonaParticipoEnTodosLosProgramasDe _ [] _ = True
laPersonaParticipoEnTodosLosProgramasDe p (c:cs) org = belongs p (autoresDe org c) && (laPersonaParticipoEnTodosLosProgramasDe p cs org)

--Costo O(C *(log p + log c)) Siendo log p el costo de realizar belongs sobre un set de personas, log c proveniente de realizar autoresDe
-- teniendose que recorrer todos los checksums en el peor caso, y se multiplica por C por tener que realizar todas estas operaciones
-- por cada checksum de la lista. La suma con C se simplifica.

------------------------------------------------------------------------------------------------------------------------------

--data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) Maybe Checksum

-- INV AGREGADO: -Si no hay programas en el organizador, el campo de mayor programa debe ser nothing, y si hay al menos 1 programa no 
-- puede ser nothing en ningun caso

elMayorPrograma :: Organizador -> Maybe Checksum
--Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
--Nothing si no puede devolver un programa.
--Eficiencia: O(1) en peor caso.
elMayorPrograma (MkO _ _ mayor) = mayor

--Funcion a modificar

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (MkO mapC mapP may) ch setP = case lookupM mapC ch of
                                    Just s -> error "El programa ya esta cargado"
                                    Nothing -> (MkO (assocM ch setP mapC) (agregarProgramaAPersonasEn (setToList setP) ch mapP) 
                                                actualizarMayor mayor ch setP mapC)
--Costo O(p * (log P + log c) + log C) siendo log C el costo de recorrer el map de checksums con lookupM y assocM y simplificandose
--del setToList por ser las mismas personas que el p proveniente de la funcion agregarProgramaAPersonasEn.

actualizarMayor :: Maybe Checksum -> Checksum -> Set Persona -> Map Checksum (Set Persona) -> Maybe Checksum
actualizarMayor mch ch setP mapC = case mch of
                                Nothing -> Just ch
                                Just mayor -> if(sizeS (autoresDeConMap mapC mayor) > (sizeS setP))
                                                then Just mch
                                                else Just ch



autoresDeConMap :: Map Checksum (Set Persona) -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDeConMap mapP ch = case lookupM ch mapC of
                            Just setP -> setP
                            Nothing -> error "El programa no esta"






