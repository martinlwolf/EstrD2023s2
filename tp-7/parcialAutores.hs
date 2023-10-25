data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO map1 map2) checkn ps = let a = ... checkn (set2List ps) map2
                                              in MkO (assocM checkn ps map1) a
                                              
agregarPersonasConChecksumAMap :: [Persona] -> Checksum -> Map Persona (Set Checksum)
agregarPersonasConChecksumAMap [] _ map = map
agregarPersonasConChecksumAMap (p:ps) n map = case lookupM n map of
                                            Just 
                                            
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO cxp pxc) c ps = case lookupM c cxp of
                                            Just _ -> error "el checksum esta registrado"
                                            Nothing -> MkO (assocM c ps cxp) (registrarAutoria (setToList ps) c pxc)


-- O(p * (la cantidad de personas de la lista) 
-- log P + (por hacer lookupM y assocM sobre las claves de pxc)
-- log c por addS sobre maximo set de checksums) 
-- log C por lookupM y assocM sobre claves de cxp
-- e
--O(log C + p * (log P + log c))
registrarAutoria :: [Persona] -> Checksum -> Map Persona (Set Checksum)
registrarAutoria [] c pxc = pxc
registrarAutoria (p:ps) c pxc = case lookupM p pxc of
                                Just cs -> assocM p (addS c cs) (registrarAutoria ps c pxc)
                                Nothing -> assocM p (addS c emptyS) (registrarAutoria ps c pxc)
                                
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas o p1 p2 = if p1 == p2
                                        then error "son la misma persona"
                                        else 
                                            let pps1 = programasDe p1 o
                                                pps2 = programasDe p2 o
                                            in not(isEmptyS(intersection pps1 pps2))
                                            
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 o = intesection (programasDe p1 o) (programasDe p2 o)