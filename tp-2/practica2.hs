--PUNTO 1
-- 1)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n : ns) = n + sumatoria ns

-- 2)
longitud :: [a] -> Int
longitud [] = 0
longitud (n : ns) = 1 + longitud ns

-- 3)
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n : ns) = (n+1) : sucesores ns

-- 4)
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (n : ns) = if (n)
                        then conjuncion ns
                        else False

conjuncionPM :: [Bool] -> Bool
conjuncionPM [] = True
conjuncionPM (True : ns) = conjuncionPM ns
conjuncionPM _ = False

-- 5)
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (False:ns) = disyuncion ns
disyuncion _ = True

-- 6)
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x : xs) = x ++ aplanar xs

--7)
pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (n:ns) = (n==k) || (pertenece k ns)

--8)
apariciones :: Eq a => a -> [a] -> Int
apariciones k [] = 0
apariciones k (n:ns) = unoSiCeroSiNo (k == n) + apariciones k ns

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo True = 1
unoSiCeroSiNo False = 0

--9)
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA  k [] = []
losMenoresA k (n:ns) = if (n<k)
                        then n : losMenoresA k ns
                        else losMenoresA k ns

--10)
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA k [] = [] 
lasDeLongitudMayorA k (n:ns) = if ((longitud n) > k)
                                then n : lasDeLongitudMayorA k ns
                                else lasDeLongitudMayorA k ns

--11)
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     k = [k]
agregarAlFinal (n:ns) k = n : agregarAlFinal ns k

--12)
agregar :: [a] -> [a] -> [a]
agregar [] l = l
agregar l [] = l
agregar (x:xs) l = x : agregar xs l

--13)
reversa :: [a] -> [a]
reversa [] = []
reversa (n:ns) =  agregarAlFinal (reversa ns) n

--14)
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] xs = xs
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = (maximoEntre x y) : (zipMaximos xs ys)

maximoEntre :: Int -> Int -> Int
maximoEntre x y = if (x>y)
                    then x
                    else y
--15)
PRECOND: la lista no debe estar vacia
elMinimo :: Ord a => [a] -> a
elMinimo [] = error
elMinimo [x] = x
elMinimo (n:ns) = if (n < elMinimo ns)
                        then n
                        else elMinimo ns

--PUNTO 2
--1)
-- PRECOND: el numero debe ser positivo
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--2)
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n =if (n < 1)
                        then [] 
                        else n : cuentaRegresiva (n-1)

--3)
repetir :: Int -> a -> [a]
repetir 0 k = []
repetir n k = k : repetir (n-1) k

--4)
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros k (n:ns) = n : losPrimeros (k-1) ns

--5)
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 l = l
sinLosPrimeros _ [] = []
sinLosPrimeros k (n:ns) = sinLosPrimeros (k-1) ns

--PUNTO 3
--1)
data Persona = P String Int
        deriving Show

edad :: Persona -> Int
edad (P n e) = e
--a)
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA k (n:ns) = if ((edad n) > k)
                        then n : mayoresA k ns
                        else mayoresA k ns

--b)
--PRECOND:"la lista no debe ser vacia"
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "la lista no debe ser vacia"
promedioEdad personas = div (sumatoria (listaDeEdades personas)) (longitud personas)

listaDeEdades :: [Persona] -> [Int]
listaDeEdades [] = []
listaDeEdades (n:ns) = (edad n) : listaDeEdades ns

--c)
--PRECOND:"la lista no debe ser vacia"
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "la lista no debe ser vacia"
elMasViejo [x] = P "default" 0
elMasViejo (n:ns) = if ((edad n) > (edad (elMasViejo ns)))
                        then n
                        else elMasViejo ns

--2)
--Para probar
pokDefault1 :: Pokemon
pokDefault1 = ConsPokemon Planta 40
pokDefault2 :: Pokemon
pokDefault2 = ConsPokemon Fuego 25
pokDefault3 :: Pokemon
pokDefault3 = ConsPokemon Fuego 25
entDefault1 :: Entrenador
entDefault1 = ConsEntrenador "ent1" [pokDefault1,pokDefault2,pokDefault3]

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]
--a)
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n listap) = longitud listap

--b)
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador n listap) = longitud (pokemonesDeTipo t listap)

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo t [] = []
pokemonesDeTipo t (n:ns) = if (sonTiposIguales t (tipoPok n))
                                then n : pokemonesDeTipo t ns
                                else pokemonesDeTipo t ns

sonTiposIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonTiposIguales Agua Agua = True
sonTiposIguales Fuego Fuego = True
sonTiposIguales Planta Planta = True
sonTiposIguales _ _ = False

tipoPok :: Pokemon -> TipoDePokemon
tipoPok (ConsPokemon t p) = t

--c)

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tipo (ConsEntrenador n1 listap1) (ConsEntrenador n2 listap2) = longitud(losQueLeGananATodosDe (losDeTipo tipo listap1) listap2)

losQueLeGananATodosDe :: [Pokemon] -> [Pokemon] -> [Pokemon]
losQueLeGananATodosDe [] _ = []
losQueLeGananATodosDe (p:ps) listap = if(superaATodosLosDe p listap)
                                        then p : losQueLeGananATodosDe ps listap
                                        else losQueLeGananATodosDe ps listap

superaATodosLosDe :: Pokemon -> [Pokemon] -> Bool
superaATodosLosDe _ [] = True
superaATodosLosDe pk (p:ps) = if(superaA pk p)
                                then superaATodosLosDe pk ps
                                else False

losDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
losDeTipo _ [] = []
losDeTipo tipo (p:ps) = if (sonTiposIguales (tipoPok p) tipo)
                                then p : losDeTipo tipo ps
                                else losDeTipo tipo ps

esTipoMasFuerteQue :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoMasFuerteQue Agua Fuego = True
esTipoMasFuerteQue Planta Agua = True
esTipoMasFuerteQue Fuego Planta = True
esTipoMasFuerteQue _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 p1) (ConsPokemon t2 p2) = esTipoMasFuerteQue t1 t2

--d)
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador n listap) = hayPokemonDeTipo Agua listap && hayPokemonDeTipo Planta listap && hayPokemonDeTipo Fuego listap

hayPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDeTipo t listap = (longitud (pokemonesDeTipo t listap)) > 0

--3)
data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

--a)
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa lrol) = proyectosDeRoles lrol

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (n:ns) = if(noEstaElProyecto_En (proyectoDeRol n) (proyectosDeRoles ns))
                                then proyectoDeRol n : proyectosDeRoles ns
                                else proyectosDeRoles ns

noEstaElProyecto_En :: Proyecto -> [Proyecto] -> Bool
noEstaElProyecto_En pr [] = True
noEstaElProyecto_En pr (p:ps) = if (sonMismoProyecto pr p)
                                        then False
                                        else (noEstaElProyecto_En pr ps)

sonMismoProyecto :: Proyecto -> Proyecto -> Bool
sonMismoProyecto (ConsProyecto s1) (ConsProyecto s2) = s1 == s2
sonMismoProyecto _ _ = False

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer s p) = p
proyectoDeRol (Management s p) = p

--b)

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) proyectos = lds (soloDevs roles) proyectos

--La funcion lds trabaja directamente con la lista de roles de la empresa
lds :: [Rol] -> [Proyecto] -> Int
lds [] _ = 0
lds roles [] = longitud roles
lds (r:rs) proyectos = if ((esSenior r) && (trabajaEnAlguno proyectos r))
                        then 1 + (lds rs proyectos)
                        else (lds rs proyectos)

esSenior :: Rol -> Bool
esSenior (Developer s p) = mismaSeniority s Senior
esSenior (Management s p) = mismaSeniority s Senior

tieneProyecto :: Proyecto -> Rol -> Bool
tieneProyecto p r = (nombreProyecto (proyectoDeRol r)) == (nombreProyecto p)

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s

mismaSeniority :: Seniority -> Seniority -> Bool
mismaSeniority Senior Senior = True
mismaSeniority SemiSenior SemiSenior = True
mismaSeniority Junior Junior = True
mismaSeniority _ _ = False

soloDevs :: [Rol] -> [Rol]
soloDevs [] = []
soloDevs (r:rs) = if (esDev r)
                        then r : soloDevs rs
                        else soloDevs rs

esDev :: Rol -> Bool
esDev (Developer _ _) = True
esDev (Management _ _) = False

--c)
--PRECOND : La lista de proyectos no debe estar vacia
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn lp (ConsEmpresa lrol) = longitud (losQueTrabajanEnAlguno lp lrol)

losQueTrabajanEnAlguno :: [Proyecto] -> [Rol] -> [Rol]
losQueTrabajanEnAlguno _ [] = []
losQueTrabajanEnAlguno [] _ = error "No hay proyectos"
losQueTrabajanEnAlguno lp (r:rs) = if (trabajaEnAlguno lp r)
                                        then r : losQueTrabajanEnAlguno lp rs
                                        else losQueTrabajanEnAlguno lp rs

trabajaEnAlguno :: [Proyecto] -> Rol -> Bool
trabajaEnAlguno [] _ = False
trabajaEnAlguno (p:ps) rol = tieneProyecto p rol || trabajaEnAlguno ps rol

--d)

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa lrol) = aPP lrol 

aPP :: [Rol] -> [(Proyecto, Int)]
aPP [] = []
aPP (r:rs) = if(noEstaElProyecto_EnListaDeTuplas (proyectoDeRol r) (aPP rs))
                then (cantidadDeVecesQueAparece_En (proyectoDeRol r) rs) : aPP rs
                else aPP rs

noEstaElProyecto_EnListaDeTuplas :: Proyecto -> [(Proyecto,Int)] -> Bool
noEstaElProyecto_EnListaDeTuplas pr [] = True
noEstaElProyecto_EnListaDeTuplas pr (t:ts) = if(sonMismoProyecto pr (fst t))
                                                then False
                                                else noEstaElProyecto_EnListaDeTuplas pr ts

cantidadDeVecesQueAparece_En :: Proyecto -> [Rol] -> (Proyecto,Int)
cantidadDeVecesQueAparece_En pr [] = (pr,1)
cantidadDeVecesQueAparece_En pr (r:rs) = if(sonMismoProyecto pr (proyectoDeRol r))
                                                then (pr, 1 + (snd(cantidadDeVecesQueAparece_En pr rs)))
                                                else cantidadDeVecesQueAparece_En pr rs