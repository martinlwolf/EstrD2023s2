-- PUNTO 2 
-- 1) a)
sucesor :: Int -> Int
sucesor a = a + 1

-- 1) b)
sumar :: Int -> Int -> Int
sumar a b = a + b

-- 1) c)
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = ((div a b), (mod a b))

-- 1) d)
maxDelPar :: (Int,Int) -> Int
maxDelPar (a,b) = if a > b
                    then a
                    else b

-- 2)
{-1 sucesor (sumar 3 (maxDelPar (divisionYResto 12 2)))
  2 sumar 6(maxDelPar (divisionYResto (sucesor 3) 1))
  3 sumar 7 (maxDelPar (divisionYResto (suma 5 5) (sucesor 2)))
  4 maxDelPar(divisionYResto (sucesor(sumar 10 9)) 2)-}

-- PUNTO 3
-- 1) 
data Dir = Norte | Este | Sur | Oeste
        deriving Show
-- a)
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este
-- b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False
-- c)
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "Error: Oeste no tiene siguiente"
--La precondicion deberia ser que la direccion ingresada no debe ser Oeste

-- 2)
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
        deriving Show

-- a)
primeroYUltimoDia :: (DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)
-- b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False
-- c)
diaANumero :: DiaDeSemana -> Int -- Convierte un dia de la semana en un numero segun el orden que tenga
diaANumero Lunes = 1
diaANumero Martes = 2
diaANumero Miercoles = 3
diaANumero Jueves = 4
diaANumero Viernes = 5
diaANumero Sabado = 6
diaANumero Domingo = 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = (diaANumero d1) > (diaANumero d2)

-- d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False

-- 3)
-- a)
negar :: Bool -> Bool
negar True = False
negar False = True
-- b)
implica :: Bool -> Bool -> Bool
implica True b = b 
implica _ _= True
-- c)
yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien _ _ = False
-- d)
oBien :: Bool -> Bool -> Bool
oBien False b =  b
oBien _ _ = True

--PUNTO 4
-- 1)
data Persona = P String Int
        deriving Show
personaDefault :: Persona --Para probar
personaDefault = P "nombre" 40
-- a)
nombre :: Persona -> String
nombre (P n e) = n
-- b)
edad :: Persona -> Int
edad (P n e) = e
-- c)
crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)
-- d)
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n2 (P n e) = P n2 e
-- e)
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2
-- f)
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
                        then p1
                        else p2

-- 2)
data TipoDePokemon = Agua | Fuego | Planta
        deriving Show
data Pokemon = Pok TipoDePokemon Int
        deriving Show
data Entrenador = Ent String Pokemon Pokemon
        deriving Show
--Para probar
pokDefault1 :: Pokemon
pokDefault1 = Pok Planta 40
pokDefault2 :: Pokemon
pokDefault2 = Pok Fuego 25
entDefault1 :: Entrenador
entDefault1 = Ent "ent1" pokDefault1 pokDefault2
-- a)
esTipoMasFuerteQue :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoMasFuerteQue Agua Fuego = True
esTipoMasFuerteQue Planta Agua = True
esTipoMasFuerteQue Fuego Planta = True
esTipoMasFuerteQue _ _ = False
superaA :: Pokemon -> Pokemon -> Bool
superaA (Pok t1 p1) (Pok t2 p2) = esTipoMasFuerteQue t1 t2
-- b)
unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

sonTiposIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonTiposIguales Agua Agua = True
sonTiposIguales Fuego Fuego = True
sonTiposIguales Planta Planta = True
sonTiposIguales _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (Ent nom pok1 pok2) = sumar (unoSiCeroSino(sonTiposIguales t (tipoPok pok1))) (unoSiCeroSino(sonTiposIguales t (tipoPok pok2)))

tipoPok :: Pokemon -> TipoDePokemon
tipoPok (Pok t p) = t
-- c)
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1,ent2) = pokemonesDeEntrenador ent1 ++ pokemonesDeEntrenador ent2

pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (Ent n1 pok1 pok2) = pok1:pok2:[]

--PUNTO 5
-- 1)
-- a)
loMismo :: a -> a
loMismo a = a 
-- b)
siempreSiete :: a -> Int
siempreSiete a = 7
-- c)
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)
-- 2) Son funciones polimorficas porque los parametros no requieren un tipo especifico, permitiendo aceptar estructuras de datos genericas.
--    Esto es posible porque no se realizan operaciones con el argumento en si.

--PUNTO 6
-- 2)
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False
-- 3)
-- PRECONDICION: la lista no debe estar vacia
elPrimero :: [a] -> a
elPrimero (x : _) = x
elPrimero [] = error "La lista esta vacia"
-- 4)
-- PRECONDICION: la lista no debe estar vacia
sinElPrimero :: [a] -> [a]
sinElPrimero (_:ys) = ys
sinElPrimero [] = error "La lista esta vacia"
-- 5)
-- PRECONDICION: la lista no debe estar vacia
splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (x, xs)
splitHead [] = error "La lista esta vacia"
