import Map
import Set

module Empresa (Empresa, consEmpresa)

where 
    
type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                        (Map CUIL Empleado)

{-
INV REP : 
    -en el segundo map cada clave esta asociada con dicha clave como cuil.
    - todo sectorId asociado a un empleado ya sea parte de un conjunto valor del primer map o valor del segundo map 
        tiene que estar en el primer map como clave.
    - todo sector id del primer map tiene que tener asociado un conjunto con dichos empleados que tiene dicho sector asociado
    - todo empleado que conforma algun set valor del primer map tiene que se valor del segundo map. 
    -
-}

consEmpresa :: Empresa
consEmpresa = ConsE (emptyM) (emptyM)

--Precondición: el CUIL es de un empleado de la empresa.
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL n (ConsE _ map2) = lookupM n map2

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector s (ConsE map1 _) = lookupM s map1

todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ map2) = keys map2

todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE map1 map2) = keys map1

agregarSector :: SectorId -> Empresa -> Empresa
agregarSector s (ConsE map1 map2) = (ConsE (assocM s (emptyS) map1) map2) 

{-agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sectores cuil (ConsE map1 map2) = (ConsE map1 map2)-}

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector id cuil (ConsE map1 map2) = (ConsE map1 (agregarSectorAEmpleadoConCuil map2))