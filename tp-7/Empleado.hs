
module Empleado 
    (Empleado, consEmpleado, cuil, incorporarSector, sectores)

where

import BST

type SectorId = Int
type CUIL = Int

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Empleado = Emp Int (Tree SectorId)

{-INV REP: El arbol de sectores es un BST-}

consEmpleado :: CUIL -> Empleado
consEmpleado n = (Emp n EmptyT)
--O(1)

cuil :: Empleado -> CUIL
cuil (Emp n _) = n 
--O(1)

incorporarSector :: SectorId -> Empleado -> Empleado
incorporarSector s (Emp n sectores) = (Emp n (insertBST s sectores))
--O(log S)

sectores :: Empleado -> [SectorId]
sectores (Emp _ sectores) = sectores
--O(1)
