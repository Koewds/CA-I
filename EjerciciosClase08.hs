--clase
module EjerciciosClase08
where
import EjerciciosClase06


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

combinatorio :: Int -> Int -> Int
combinatorio n k = (fact n) `div` ((fact k) * (fact (n-k)))

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1
combinatorio' n k | n == k = 1
                  | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))

longitudC8 :: Set Int -> Int -> Int
longitudC8 [] z = 0
longitudC8 (x:xs) z = (1 + longitudC8 xs z)

cantidadDeVariaciones :: Set Int -> Int -> Int
cantidadDeVariaciones n z = (longitudC8 n z)^z

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas n z = (agregarElementoAdelante (head n) z) `union` (agregarElementosAListas (tail n) z)

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante n [] = []
agregarElementoAdelante n (ys:yss) = agregar (n:ys) (agregarElementoAdelante n yss)

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn n z k | k == 1 = z:n
                    | otherwise = agregar (head n) (insertarEn (tail n) z (k-1))

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = insertarEnCadaPos xs c (length xs +1) `union` (insertarEnCadaPosDeTodasLasListas xss c)


permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c
--clase
----------------------------------
--1
--n bolitas numeradas en k cajas
bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = variaciones [1..k] n

--2
listaConPrimeraCaja :: Set [Int] -> Set [Int] -> Set [Int]
listaConPrimeraCaja [] z = z
listaConPrimeraCaja n z | pertenecepm 1 (head n) = listaConPrimeraCaja (tail n) ((head n):z)
                        | otherwise = listaConPrimeraCaja (tail n) z

formasConPrimeraNuncaVacia :: Int -> Int -> Set [Int]
formasConPrimeraNuncaVacia n z = listaConPrimeraCaja (bolitasEnCajas n z) []

--3
estaOrdenado :: Set Int -> Bool
estaOrdenado (x:xs) | xs == [] = True
                    | x >= (head xs) = False
                    | otherwise = estaOrdenado xs

filtrarOrdenados :: Set [Int] -> Set [Int] -> Set [Int]
filtrarOrdenados [] z = z
filtrarOrdenados (xs:xss) z | estaOrdenado xs = filtrarOrdenados xss (xs:z)
                            | otherwise = filtrarOrdenados xss z

listasOrdenadas :: Int -> Int -> Set [Int]
listasOrdenadas n k = filtrarOrdenados (variaciones [1..n] k) []

--4

variacionesL :: [Char] -> Integer -> Set [Char]
variacionesL c 0 = [[]]
variacionesL c k = agregarElementosAListasL c (variacionesL c (k-1))

agregarElementosAListasL :: Set Char-> Set [Char] -> Set [Char]
agregarElementosAListasL [] _ = []
agregarElementosAListasL n z = (agregarElementoAdelanteL (head n) z) `union` (agregarElementosAListasL (tail n) z)

agregarElementoAdelanteL :: Char -> Set [Char] -> Set [Char]
agregarElementoAdelanteL n [] = []
agregarElementoAdelanteL n (ys:yss) = agregar (n:ys) (agregarElementoAdelanteL n yss)

--not used
--

agregarL :: Eq a => a -> Set a -> Set a
agregarL n c = n:c

insertarEnL :: [Char] -> Char -> Int -> [Char]
insertarEnL n z k | k == 1 = z:n
                  | otherwise = agregarL (head n) (insertarEnL (tail n) z (k-1))

insertarEnCadaPosL :: [Char] -> Char -> Int -> Set [Char]
insertarEnCadaPosL xs c 1 = agregar (insertarEnL xs c 1) vacio
insertarEnCadaPosL xs c i = agregar (insertarEnL xs c i) (insertarEnCadaPosL xs c (i-1))

insertarEnCadaPosDeTodasLasListasL :: Set [Char] -> Char -> Set [Char]
insertarEnCadaPosDeTodasLasListasL [] c = []
insertarEnCadaPosDeTodasLasListasL (xs:xss) c = insertarEnCadaPosL xs c (length xs + 1) `union` (insertarEnCadaPosDeTodasLasListasL xss c)


permutacionesL :: Set Char -> Set [Char]
permutacionesL [] = [[]]
permutacionesL (c:cs) = insertarEnCadaPosDeTodasLasListasL (permutacionesL cs) c

generarNVeces :: Integer -> Char -> [Char]
generarNVeces 0 c = []
generarNVeces n c = [c] ++ (generarNVeces (n-1) c)

--auxs
--
sucABLongNM :: Integer -> Integer -> Set [Char]
sucABLongNM n m = permutacionesL ((generarNVeces n 'a') ++ (generarNVeces m 'b'))

sucABCLongNMK :: Integer -> Integer -> Integer -> Set [Char]
sucABCLongNMK n m k = permutacionesL ((generarNVeces n 'a') ++ (generarNVeces m 'b') ++ (generarNVeces k 'n'))

sucesionAux :: [Char] -> [Integer] -> [Char]
sucesionAux [] [] = []
sucesionAux n z = (generarNVeces (head z) (head n)) ++ (sucesionAux (tail n) (tail z))

sucesion :: [Char] -> [Integer] -> Set [Char]
sucesion n z = permutacionesL (sucesionAux n z)
--
--6
subConj :: Set Int -> Int -> Set [Int]
subConj [] _ = [[]]
subConj  _  0 = [[]]
subConj c k | longitud c == k = [c]
            | otherwise = union (insertarEnCadaPosDeTodasLasListas (subConj (tail c) (k-1)) (head c)) (subConj (tail c) k)
