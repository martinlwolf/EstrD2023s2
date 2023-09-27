emptyQ :: Queue a
emptyQ = (QQ [] [])

isEmptyQ :: Queue a
isEmptyQ (QQ fs _) = null fs 

firstQ :: Queue a -> a
firstQ (QQ fs _) = head fs

enqueue :: a -> Queue a -> Queue a
enqueue x (QQ fs bs) = if (null fs)
                        then (QQ [x] [])
                        else (QQ fs (x:bs))

dequeue :: Queue a -> Queue a
dequeue (QQ fs bs) = if null (tail fs)
                        then (QQ (reverse bs) [])
                        else (QQ (tail fs) bs)