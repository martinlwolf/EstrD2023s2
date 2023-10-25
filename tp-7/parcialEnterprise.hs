data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

{-INV REP: - el sector de la tupla debe estar en el primer map y ser el sector con mas tripulantes del mismo
        -los tripulantes que esten en el heap deben estar en algun set de algun sector del primer map
        - no puede haber un mismo tripulante que este en un set de un sector en algun otro sector o en el mismo set-}

naveVacia :: [Sector] -> Nave
naveVacia [] = error
naveVacia secs = (MkN (agregarSectoresVaciosEn secs emptyM) emptyH 