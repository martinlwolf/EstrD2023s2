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

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sectores cuil (ConsE map1 map2) = let e = incorporarSectoresA sectores (consEmpleado cuil)
                                                   in (ConsE (agregarEmpleadoASectoresDe e sectores map1) (assocM cuil e map2))
--O(log s * (log S + log e) + s log s)

incorporarSectoresA :: [SectorId] -> Empleado -> Empleado
incorporarSectoresA [] emp = emp
incorporarSectoresA (s : ss) emp =  incorporarSectoresA ss (incorporarSector s emp)
--O(s log s)

agregarEmpleadoASectoresDe :: Empleado -> [SectorId] -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
agregarEmpleadoASectoresDe emp [] map = map
agregarEmpleadoASectoresDe emp (s:ss) map = agregarEmpleadoASectoresDe emp ss (agregarEmpleadoEnSector emp s map)
--O(log s * (log S + log e))

agregarEmpleadoEnSector :: Empleado -> SectorId -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
agregarEmpleadoEnSector emp sec map = case lookupM sec map of
                                    Just emps -> assocM sec (addS emp emps) (deleteM sec map)
                                    Nothing -> map
--O(log S + log e)

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector 

--EJERCICIO 5
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon secs cuils = agregarEmpleadosA cuils secs (agregarSectoresEn secs (consEmpresa))

agregarSectoresEn :: [SectorId] -> Empresa -> Empresa
agregarSectoresEn [] emp = emp
agregarSectoresEn (s:ss) emp = agregarSector s (agregarSectoresEn ss emp)
--O(s log s)

agregarEmpleadosA :: [CUIL] -> [SectorId] -> Empresa -> Empresa
agregarEmpleadosA [] emp secs = emp
agregarEmpleadosA (c:cs) emp = agregarEmpleado secs c (agregarEmpleadosA cs secs emp)

recorteDePersonal :: Empresa -> Empresa
recorteDePersonal emp = borrarEmpladosEn (losPrimeros (div (cantidadTotalDeEmpleados emp) 2) (todosLosCuil emp))  emp

cantidadTotalDeEmpleados :: Empresa -> Int
cantidadTotalDeEmpleados emp = size (todosLosCuil emp)

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros k (n:ns) = n : losPrimeros (k-1) ns

borrarEmpladosEn :: [CUIL] -> Empresa -> Empresa
borrarEmpladosEn [] emp = emp
borrarEmpladosEn (c:cs) emp = borrarEmpleado c (borrarEmpladosEn cs emp)

convertirEnComodin :: CUIL -> Empresa -> Empresa
convertirEnComodin cuil empr = agregarASectores (todosLosSectores) cuil empr

agregarASectores :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarASectores [] _ empr = empr
agregarASectores (s : ss) c empr = agregarASector s c (agregarASectores ss c empr)

esComodin :: CUIL -> Empresa -> Bool
esComodin c empr = (size(sectores (buscarPorCUIL c empr))) = (size(todosLosSectores empr))
