asporproy :: Empresa -> [(Proyecto,Int)]
asporproy (Cons Empresa rs) = contarProy rs

contarProy :: [Rol] -> [(Proyecto, Int)]
contarProy [] = []
contarProy (r:rs) = sumarProyecto r (contarProy rs)

sumarProyecto :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
sumarProyecto r [] = 
sumarProyecto r [(p,n) : ps] = if (trabajaEn r p)
                                then (p,n+1) : ps
                                else (p,n) : sumarProyecto r ps