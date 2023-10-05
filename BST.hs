module BST (Set, belongsBST, insertBST, deleteBST
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
insertBST x EmptyT = (Node x EmptyT EmptyT)
insertBST x (NodeT y tree1 tree2) = if (x == y)
                                        then (NodeT x tree1 tree2)
                                        else if (x < y)
                                                then (Node y (insertBST x) tree2)
                                                else (Node y tree1 (insertBST x))
                                                
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT =
deleteBST x (NodeT y tree1 tree2) = 
                                                

                                                
