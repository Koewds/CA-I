--head [1, 2, 3] devuelve el primer ítem de la lista
--tail [1, 2, 3] devuelve los ítems siguientes al primer ítem.
-- : agrega un ítem a la cabeza de la lista.

sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = longitud (tail l) +1

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | x == head l = True
              | otherwise = pertenece x (tail l)

pertenecepm :: Int -> [Int] -> Bool
pertenecepm _ [] = False
pertenecepm x l | x == head l = True
                | otherwise = pertenecepm x (tail l)

productoria :: [Int] -> Int
productoria l | l == [] = 1
              | otherwise = head l * productoria (tail l)


sumarN :: Int -> [Int] -> [Int]
sumarN n xs | xs == [] = []
            | otherwise = ((head xs) + n):( sumarN n (tail xs) )

sumarElPrimeroAux :: Int -> [Int] -> [Int]
sumarElPrimeroAux n xs | xs == [] = []
                       | otherwise = (n + head xs):(sumarElPrimeroAux n (tail xs))


sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarElPrimeroAux (head xs) xs

sumarElUltimoAux :: Int -> [Int] -> [Int]
sumarElUltimoAux n xs | xs == [] = []
                      | otherwise = (n + head xs):(sumarElPrimeroAux n (tail xs))

ultimoDeLista :: [Int] -> Int
ultimoDeLista xs | longitud xs == 1 = head xs
                 | otherwise = ultimoDeLista (tail xs)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarElUltimoAux (ultimoDeLista xs) xs

esPar :: Int -> Bool
esPar n = mod n 2 == 0

pares :: [Int] -> [Int]
pares xs | xs == [] = []
         | esPar (head xs) == True = (head xs):(pares (tail xs))
         | otherwise = pares (tail xs)

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n xs | xs == [] = []
                 | n == (head xs) = quitarTodas n (tail xs)
                 | otherwise = (head xs):(quitarTodas n (tail xs))

quitar :: Int -> [Int] -> [Int]
quitar n xs | xs == [] = []
            | n == (head xs) = (tail xs)
            | otherwise = (head xs):(quitar n (tail xs))

firstRepeats :: Int -> [Int] -> Bool
firstRepeats n xs | xs == [] = False
                  | n == head xs = True
                  | otherwise = firstRepeats n (tail xs)

hayRepetidos :: [Int] -> Bool
hayRepetidos xs | xs == [] = False
                | (firstRepeats (head xs) (tail xs) == False) = hayRepetidos (tail xs)
                | otherwise = True


reversoAux :: [Int] -> [Int] -> [Int]
reversoAux n z | n == [] = z
                 | otherwise = reversoAux (tail n) ((head n):z)

reverso :: [Int] -> [Int]
reverso n = reversoAux n []

eliminarRepetidosAlFinalAux :: [Int] -> [Int] -> [Int]
eliminarRepetidosAlFinalAux n z | n == [] = (reverso z)
                                | not (pertenece (head n) z) = eliminarRepetidosAlFinalAux (tail n) ((head n):z)
                                | otherwise = eliminarRepetidosAlFinalAux (tail n) z

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal n = eliminarRepetidosAlFinalAux n []

eliminarRepetidosAlInicioAux :: [Int] -> [Int] -> [Int]
eliminarRepetidosAlInicioAux n z | n == [] = (reverso z)
                                 | pertenece (head n) (tail n) = eliminarRepetidosAlInicioAux (tail n) z
                                 | otherwise = eliminarRepetidosAlInicioAux (tail n) ((head n):z)

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio n = eliminarRepetidosAlInicioAux n []

maximoEntre :: [Int] -> Int -> Int
maximoEntre xs n | (tail xs) == [] && n >= (head xs) = n
                 | (tail xs) == [] && n < (head xs) = (head xs)
                 | head xs >= n = maximoEntre (tail xs) (head xs)
                 | otherwise = maximoEntre (tail xs) n

maximo :: [Int] -> Int
maximo xs = maximoEntre xs (head xs)

ordenarAux :: [Int] -> [Int] -> [Int]
ordenarAux n z | n == [] = z
               | otherwise = ordenarAux (quitar (maximo n) n) (maximo n:z)

ordenar :: [Int] -> [Int]
ordenar n = ordenarAux n []

concatenar :: [Int] -> [Int] -> [Int]
concatenar n z = n ++ z
