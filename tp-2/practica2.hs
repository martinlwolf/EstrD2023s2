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
conjuncion (b : bs) = b && conjuncion bs

-- 5)
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b : bs) = b || disyuncion bs

-- 6)
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs : xss) = xs ++ aplanar xss

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
lasDeLongitudMayorA k (xs:xss) = if ((longitud xs) > k)
                                then xs : lasDeLongitudMayorA k xss
                                else lasDeLongitudMayorA k xss

--11)
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     k = [k]
agregarAlFinal (n:ns) k = n : agregarAlFinal ns k

--12)
agregar :: [a] -> [a] -> [a]
agregar [] l = l
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
--PRECOND: la lista no debe estar vacia
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "la lista no debe estar vacia"
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
elMasViejo [x] = x
elMasViejo (n:ns) = if (esMasViejoQue n (elMasViejo ns))
                        then n
                        else elMasViejo ns

esMasViejoQue :: Persona -> Persona -> Bool
esMasViejoQue (P n edad1) (P n2 edad2) = edad1 > edad2

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
superaATodosLosDe pk (p:ps) = (superaA pk p) && (superaATodosLosDe pk ps)

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
hayPokemonDeTipo t (p:ps) = (sonTiposIguales t (tipoPok p)) || (hayPokemonDeTipo t ps)


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
proyectosDeRoles (n:ns) = let proyectosSinRepetir = proyectosDeRoles ns in
                                if(noEstaElProyecto_En (proyectoDeRol n) proyectosSinRepetir)
                                        then proyectoDeRol n : proyectosDeRoles ns
                                        else proyectosDeRoles ns

noEstaElProyecto_En :: Proyecto -> [Proyecto] -> Bool
noEstaElProyecto_En pr [] = True
noEstaElProyecto_En pr (p:ps) = not(sonMismoProyecto pr p) && (noEstaElProyecto_En pr ps)

sonMismoProyecto :: Proyecto -> Proyecto -> Bool
sonMismoProyecto (ConsProyecto s1) (ConsProyecto s2) = s1 == s2

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer s p) = p
proyectoDeRol (Management s p) = p

--b)

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) proyectos = losQueSonDevsSenior (soloSeniors(soloDevs roles)) proyectos

--La funcion lds trabaja directamente con la lista de roles de la empresa
losQueSonDevsSenior :: [Rol] -> [Proyecto] -> Int
losQueSonDevsSenior [] _ = 0
losQueSonDevsSenior (r:rs) proyectos = (unoSiCeroSiNo (trabajaEnAlguno proyectos r)) + (losQueSonDevsSenior rs proyectos)

esSenior :: Rol -> Bool
esSenior (Developer s p) = mismaSeniority s Senior
esSenior (Management s p) = mismaSeniority s Senior

tieneProyecto :: Proyecto -> Rol -> Bool
tieneProyecto p r = (nombreProyecto p) == (nombreProyecto (proyectoDeRol r))

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

soloSeniors :: [Rol] -> [Rol]
soloSeniors [] = []
soloSeniors (r:rs) = if (esSenior r)
                        then r : soloSeniors rs
                        else soloSeniors rs

esDev :: Rol -> Bool
esDev (Developer _ _) = True
esDev (Management _ _) = False

--c)
--PRECOND : La lista de proyectos no debe estar vacia
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn lp (ConsEmpresa lrol) = longitud (losQueTrabajanEnTodos lp lrol)

losQueTrabajanEnTodos :: [Proyecto] -> [Rol] -> [Rol]
losQueTrabajanEnTodos _ [] = []
losQueTrabajanEnTodos [] (r:rs) = (r:rs) 
losQueTrabajanEnTodos lp (r:rs) = if (trabajaEnTodos lp r)
                                        then r : losQueTrabajanEnTodos lp rs
                                        else losQueTrabajanEnTodos lp rs

trabajaEnAlguno :: [Proyecto] -> Rol -> Bool
trabajaEnAlguno [] _ = False
trabajaEnAlguno (p:ps) rol = tieneProyecto p rol || trabajaEnAlguno ps rol

trabajaEnTodos :: [Proyecto] -> Rol -> Bool
trabajaEnTodos [] _ = False
trabajaEnTodos (p:ps) rol = tieneProyecto p rol && trabajaEnTodos ps rol

--d)

asignadosPorProyecto :: Empresa -> [(Proyecto,Int)]
asignadosPorProyecto (ConsEmpresa rs) = contarProy rs

contarProy :: [Rol] -> [(Proyecto, Int)]
contarProy [] = []
contarProy (r:rs) = sumarProyecto r (contarProy rs)

sumarProyecto :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
sumarProyecto r [] = [(proyectoDeRol r, 1)]
sumarProyecto r ((p,n) : ps) = if (tieneProyecto p r)
                                then (p,n+1) : sumarProyecto r ps
                                else (p,n) : sumarProyecto r ps

tieneProyecto :: Proyecto -> Rol -> Bool
tieneProyecto p r = (nombreProyecto p) == (nombreProyecto (proyectoDeRol r))

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer s p) = p
proyectoDeRol (Management s p) = p

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s