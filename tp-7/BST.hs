module BST (BST, belongsBST, insertBST, deleteBST
               , splitMinBST, splitMaxBST) 
where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data BST a  = BST (Tree a)

{-
    Inv Rep: Los elementos del arbol de la izquierda son todos menores a la raiz, y los elementos
    del arbol derecho son mayores a la raiz. No se repiten los elementos-}

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _ EmptyT = False
belongsBST x (NodeT y tree1 tree2) = (x == y) || if (x < y)
                                                then belongsBST x tree1
                                                else belongsBST x tree2
                                            
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT = (NodeT x EmptyT EmptyT)
insertBST x (NodeT y tree1 tree2) = if (x == y)
                                        then (NodeT x tree1 tree2)
                                        else if (x < y)
                                                then (NodeT y (insertBST x tree1) tree2)
                                                else (NodeT y tree1 (insertBST x tree2))
                                                
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y tree1 tree2) = if(x == y) 
                                    then rearmarBST tree1 tree2
                                    else if  (x < y)
                                            then deleteBST x tree1 
                                            else deleteBST x tree2

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT td = EmptyT
rearmarBST ti td = (NodeT (fst(splitMaxBST ti)) (snd(splitMaxBST ti)) td)

{-USANDO LET IN
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT td = EmptyT
rearmarBST ti td = let (m, ti2) = splitMaxBST ti
                    in (NodeT m ti2 td-}

--PRECOND: El tree no debe estar vacio
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (NodeT x EmptyT td) = (x,td)
splitMinBST (NodeT x ti td) = let (y, ti2) = splitMinBST ti
                                in (y, NodeT x ti2 td)

--PRECOND: El tree no debe estar vacio
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST (NodeT x ti EmptyT) = (x,ti)
splitMaxBST (NodeT x ti td) = let (y, td2) = splitMaxBST td
                                in (y, NodeT x ti td2)

esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x ti td) = if(seCumpleBst x ti td)
                            then (esBST ti && esBST td)
                            else False

seCumpleBst:: Ord a => a -> Tree a -> Tree a -> Bool
seCumpleBst x ti EmptyT = (x > (root ti))
seCumpleBst x EmptyT td = (x < (root td))
seCumpleBst x ti td     = (x > (root ti)) && (x < (root td))
--ES CUADRATICA PORQUE HACE 2 CONSTANTES N*2 POR CADA ARBOL

--PRECOND: NO ES EMPTYT
root :: Tree a -> a 
root (NodeT x _ _) = x

elMaximoMenorA _ EmptyT          = Nothing
elMaximoMenorA x (NodeT y ti td) = if (x==y)
                                   then Just (maxBST ti)
                                   else if (x > y)
                                        then case elMaximoMenorA x td of
                                             Just v -> Just v 
                                             Nothing -> Just y
                                        else elMaximoMenorA x ti

elMinimoMayorA _ EmptyT          = Nothing
elMinimoMayorA x (NodeT y ti td) = if (x==y)
                                   then Just (minBST td)
                                   else if (x < y)
                                        then case elMinimoMayorA x ti of
                                             Just v -> Just v 
                                             Nothing -> Just y
                                        else elMinimoMayorA x td

balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT _ ti td) = if (((heightT ti) - (heightT td)) <= 1) || (((heightT ti) - (heightT td)) >= 1)
                                then balanceado ti && balanceado td
                                else False


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT a tree1 tree2) = 1 + (max (heightT tree1) (heightT tree2))

isEmpty :: Tree a -> Bool
isEmpty EmptyT = True
isEmpty _ = False

