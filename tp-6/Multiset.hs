module Multiset
    (Multiset, emptyMS, addMS, ocurrencesMS
             , unionMS, intersectionMS, multiSet2List) 
  where

import Map

data Multiset a = MS (Map a Int)

unionMS      :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
  -- PROP.: une dos multisets (las ocurrencias de un elemento son las que aparecen en ambos)

intersectMS  :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
  -- PROP.: interseca dos multisets (las ocurrencias de un elemento son el mínimo entre ambos)

emptyM :: Multiset a
emptyM = MS emptyM

addMS :: Ord a => a -> Multiset a -> Multiset a
addMS x (MS map) = case lookupM x map of
                    Just n -> MS (assocM x (n+1) map)
                    Nothing -> MS (assocM x 1 map)

ocurrencesMS :: Ord a => a -> Multiset a -> Int
ocurrencesMS x (MS map) = case lookupM x mp of
                    Just n -> n
                    Nothing -> 0

multiSetToList :: Multiset a -> [(a,Int)]
multiSetToList (MS map) = mapToList map