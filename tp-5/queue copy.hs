module Queue
    (emptyQ, isEmptyQ, enQueue, firstQ, deQueue)
where
    data Queue a = Queue [a]

emptyQ :: Queue a
emptyQ = (Queue [])

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue xs) = True
isEmptyQ (Queue []) = False

enQueue :: a -> Queue a -> Queue a
enQueue x (Queue xs) = (Queue (x:xs)) -- DE LINEAL A CONSTANTE

firstQ :: Queue a -> a
firstQ (Queue xs) = sacarDelFinal xs
firstQ (Queue []) = error "listaVacia" -- DE CONSTANTE A LINEAL

deQueue :: Queue a -> Queue a
deQueue (Queue (x:xs)) = (Queue xs)
deQueue (Queue []) = error "listaVacia"

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     k = [k]
agregarAlFinal (n:ns) k = n : agregarAlFinal ns k

sacarDelFinal :: [a] -> a
sacarDelFinal []     = error "listaVacia"
sacarDelFinal (n:ns) = if (null ns)
                            then n
                            else n : (sacarDelFinal ns k)