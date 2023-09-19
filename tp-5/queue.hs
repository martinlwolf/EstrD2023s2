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
enQueue x (Queue xs) = (Queue (agregarAlFinal x xs))

firstQ :: Queue a -> a
firstQ (Queue (x:xs)) = x
firstQ (Queue []) = error "listaVacia"

deQueue :: Queue a -> Queue a
deQueue (Queue (x:xs)) = (Queue xs)
deQueue (Queue []) = error "listaVacia"

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     k = [k]
agregarAlFinal (n:ns) k = n : agregarAlFinal ns k
