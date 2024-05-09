module RAList (emptyRAL, isEmptyRAL, lengthRAL, get, minRAL, add, elems, remove, set, addAt)

where

data RAList a = MkR Int (Map Int a) (Heap a)

{-INV REP:  -El numero entero que esta en el campo del RAList debe ser siempre igual a la cantidad de claves que posea el map e igual a la 
                cantidad de elementos del heap, asi con el map y el heap respectivamente. El entero tampoco podra ser negativo en ningun caso
            -Los numeros del map que funcionan como claves deben ser positivos y secuenciales.
            -El map y el heap tienen exactamente los mismos elementos.
            -El campo de siguiente posicion debe ser mayor a todos los enteros que funcionan como claves del map y es un numero mas de la
            longitud de la lista
-}

emptyRAL :: RAList a
--Propósito: devuelve una lista vacía.
--Eficiencia: O(1).
emptyRAL = (MkR 0 emptyM emptyH)

isEmptyRAL :: RAList a -> Bool
--Propósito: indica si la lista está vacía.
--Eficiencia: O(1).
isEmptyRAL (MkR n _ _) = n == 0

lengthRAL :: RAList a -> Int
--Propósito: devuelve la cantidad de elementos.
--Eficiencia: O(1).
lengthRAL (MkR n map h) = n

get :: Int -> RAList a -> a
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
--Eficiencia: O(log N).
get (MkR n map h) = case lookupM n map of
                Nothing -> error "El indice no esta en el map"
                Just elem -> elem

minRAL :: Ord a => RAList a -> a
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(1).
minRAL (MkR n map h) = findMin h

add :: Ord a => a -> RAList a -> RAList a
--Propósito: agrega un elemento al final de la lista.
--Eficiencia: O(log N).
add e (MkR n map h) = (MkR (n+1) (assocM n e map) (insertH e h))

elems :: Ord a => RAList a -> [a]
--Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
--Eficiencia: O(N log N).
elems (MkR n map h) = valoresDeKeysEn (domM map) map

valoresDeKeysEn :: [int] -> Map Int a -> [a]
valoresDeKeysEn [] _ = []
valoresDeKeysEn (k:ks) map = case lookupM k map of
                        Just x -> x:(valoresDeKeysEn ks map)
                        Nothing -> valoresDeKeysEn ks map


remove :: Ord a => RAList a -> RAList a
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(N log N).
remove (MkR n map h) = if n != 0
                        then (MkR (n-1) (deleteM n map) (eliminarDeHeap (lookupM (n-1) map) h))
                        else error "La RAList esta vacia"

eliminarDeHeap :: Ord a => a -> Heap a -> Heap a
eliminarDeHeap x heap = if(findMin heap == x)
                        then(deleteMin h)
                        else(insertH (findMin heap) (eliminarDeHeap a (deleteMin heap)))
--Costo O(N log N), siendo log N el costo de hacer deleteMin e insertH sobre los elementos del heap y se multiplica por N por tener que
--realizarse estas operaciones en todo el heap en el peor caso

set :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
--Eficiencia: O(N log N).
set new xn (MkR n map h) = case lookupM new map of
                        Just xv -> (MkR n (assocM new x map) (reemplazarEnHeap xn xv h))
                        Nothing -> error "El elemento a reemplazar no esta"

reemplazarEnHeap :: Ord a => a -> a -> Heap a -> Heap a
reemplazarEnHeap xn xv heap = if(findMin heap == xv)
                            then(insertH xn (deleteMin h))
                            else(insertH (findMin heap) (reemplazarEnHeap a (deleteMin heap)))

addAt :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
--Eficiencia: O(N log N).
--Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
--también como argumento la máxima posición posible.
addAt new x (MkR n map h) = if n-1 >= new
                            then(MkR (n+1) (assocM n x (desplazarAPartirDeHasta (new) (n-1) map) (insertH x h)))
                            else error "El índice debe estar entre 0 y la longitud de la lista."

desplazarAPartirDeHasta :: Ord a => Int -> Int -> Map Int a -> Map Int a
desplazarAPartirDeHasta n d map = if n <= d
                                    then assocM (n+1) (lookupM n map) (desplazarAPartirDeHasta (n+1) d)
                                    else map