module StackV1
    (emptyS, isEmptyS, push, firstQ, deQueue)
where
    data Stack a = Stack [a]

emptyS :: Stack a
emptyS = (Stack [])

isEmptyS :: Stack a -> Bool
isEmptyS (Stack xs) = True
isEmptyS (Stack []) = False

push :: a -> Stack a -> Stack a
push x (Stack xs) = agregarAlFinal xs x

--PRECOND: EL STACK DEBE TENER AL MENOS UN ELEMENTO
top :: Stack a -> a
top (Stack xs) = sacarDelFinal xs

pop :: Stack a -> Stack a
pop (Stack xs) = laListaSinElFinal xs

lenS :: Stack a -> Int
lenS (Stack xs) = longitud xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     k = [k]
agregarAlFinal (n:ns) k = n : agregarAlFinal ns k

sacarDelFinal :: [a] -> a
sacarDelFinal []     = error "listaVacia"
sacarDelFinal (n:ns) = if (null ns)
                            then n
                            else n : (sacarDelFinal ns k)

laListaSinElFinal :: [a] -> [a]
laListaSinElFinal [] = []
laListaSinElFinal (x:xs) = if (null ns)
                            then [n]
                            else [n] ++ (laListaSinElFinal xs)

longitud :: [a] -> Int
longitud [] = 0
longitud (n : ns) = 1 + longitud ns