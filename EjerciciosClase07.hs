--clase

type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece x l | l == [] = False
              | x == head l = True
              | otherwise = pertenece x (tail l)

agregar :: Int -> Set Int -> Set Int
agregar n z | pertenece n z = z
            | otherwise = n:z

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC x l | l == [] = False
               | x == head l = True
               | otherwise = perteneceC x (tail l)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC n z | perteneceC n z = z
             | otherwise = n:z

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] z = z
unionC n [] = n
unionC (x:xs) z | perteneceC x z = unionC xs z
                | otherwise     = unionC xs (x:z)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))


--clase
--------------------------------------------------------

union :: Set Int -> Set Int -> Set Int
union [] z = z
union n [] = n
union (x:xs) z | pertenece x z = union xs z
               | otherwise     = union xs (x:z)

interseccionAux :: Set Int -> Set Int -> Set Int -> Set Int
interseccionAux n z m | n == [] = m
                      | pertenece (head n) z = interseccionAux (tail n) z ((head n):m)
                      | otherwise = interseccionAux (tail n) z m

interseccion :: Set Int -> Set Int -> Set Int
interseccion n z = interseccionAux n z []

diferenciaAux :: Set Int -> Set Int -> Set Int -> Set Int
diferenciaAux n z m | n == [] = m
                    | pertenece (head n) z = diferenciaAux (tail n) z m
                    | otherwise = diferenciaAux (tail n) z ((head n):m)

diferencia :: Set Int -> Set Int -> Set Int
diferencia n z = diferenciaAux n z []

diferenciaSimetricaAux :: Set Int -> Set Int -> Set Int
diferenciaSimetricaAux n m | n == [] = m
                           | otherwise = diferenciaSimetricaAux (tail n) (agregar (head n) m)

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica n z = diferenciaSimetricaAux (diferencia z n) (diferenciaSimetricaAux (diferencia n z) [])

partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (partesN (n-1)) (agregarATodos n (partesN (n-1)))

juntarPares :: Set Int -> Int -> Set (Int, Int) -> Set (Int, Int)
juntarPares [] _ m = m
juntarPares n  z m = juntarPares (tail n) z (((head n), z):m)

productoCartesiano1 :: Set Int -> Set Int -> Set (Int, Int) -> Set (Int, Int)
productoCartesiano1 _ [] m = m
productoCartesiano1 n z m = productoCartesiano1 n (tail z) ((juntarPares n (head z) [])++m)

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano n z = productoCartesiano1 n z []

desemparejarAux :: Set (Int, Int) -> Set Int -> Set Int
desemparejarAux [] m = m
desemparejarAux n m  = desemparejarAux (tail n) (agregar (fst (head n)) (agregar (snd (head n)) m))

desemparejar :: Set (Int, Int) -> Set Int
desemparejar n = desemparejarAux n []

emparejarConSumaN :: Int -> Set Int -> Set (Int, Int)
emparejarConSumaN n (x:xs) | xs == [] = []
                           | pertenece (n-x) xs = (n,n-x) : emparejarConSumaN n xs
                           | otherwise = emparejarConSumaN n xs

suma :: Set Int -> Int
suma n | n == [] = 0
       | otherwise = (head n) + suma (tail n)

seParteAux :: Set (Set Int) -> Set Int -> Bool
seParteAux n m | n == [] = False
               | suma (head n) == suma (diferencia m (head n)) = True
               | otherwise = seParteAux (tail n) m

seParte :: Set Int -> Bool
seParte n = seParteAux (partes n) n
