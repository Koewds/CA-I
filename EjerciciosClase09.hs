module EjerciciosClase09
where
import EjerciciosClase06
import EjerciciosClase07

--clase

-- Division de numeros naturales
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a-d) `divNat` d + 1

-- Resto de numeros naturales
modNat :: Int -> Int -> Int
modNat a d = a - d * (a `divNat` d)
--simplemente despejar r de a = qd + r

-- Modulo de numeros enteros
modulo :: Int -> Int -> Int
modulo a d | a >= 0 || r' == 0 = r'
           | otherwise = abs d - r'
           where
             r' = abs a `modNat` abs d

-- Division de numeros enteros:
dividido :: Int -> Int -> Int
dividido a d = sgq * absq
            where absq = abs (a-r) `divNat` (abs d)
                  sgq = (signum a) * (signum d)
                  r = a `modulo` d

--clase
-----------------------------------------------------


digitosAux :: Integer -> Integer -> [Integer]
digitosAux n b | n < b = [n]
               | otherwise = (mod n b):(digitosAux (div n b) b)

-- dados n >= 0 y b > 1, retorne su representación por listas en base b.
digitos :: Integer -> Integer -> [Integer]
digitos n b | n < 0 = undefined
            | b <= 0 = undefined
            | otherwise = digitosAux n b

numeroAux :: [Integer] -> Integer -> Integer -> Integer
numeroAux [] b n = 0
numeroAux l b n = (head l)*(b^n) + numeroAux (tail l) b (n+1)

-- dada una representación por listas de n >= 0 en base b y la base b > 1 retorne n
numero :: [Integer] -> Integer -> Integer
numero l b = numeroAux l b 0

divisoresAux :: Integer -> Integer -> [Integer] -> [Integer]
divisoresAux n k z | k == 0 = z
                   | mod n k == 0 = divisoresAux n (k-1) (k:z)
                   | otherwise = divisoresAux n (k-1) z

divisores :: Integer -> [Integer]
divisores a = divisoresAux a a []

--

mcdDef :: Integer -> Integer -> Integer
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo' (interseccion' (divisores a) (divisores b))

--inefficient
--
mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

mcmAux :: Integer -> Integer -> Integer -> Integer
mcmAux a b k | mod (a*k) b == 0 = (a*k)
             | otherwise = mcmAux a b (k+1)

mcm :: Integer -> Integer -> Integer
mcm a b | a > b = mcmAux a b 1
        | a < b = mcmAux b a 1
        | a == b = a

emcdAux :: Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
emcdAux a 0 c = (a, 0):c
emcdAux a b c = emcdAux b (mod a b) ((a, b):c)

buscarSyT :: [(Integer, Integer)] -> Integer -> Integer -> (Integer, Integer)
buscarSyT (c:cs) s t | cs == [] = (t, (s - ((div (fst c) (snd c)) * t)))
                     | snd c == 0 =  buscarSyT cs 1 2
                     | otherwise = buscarSyT cs t (s - ((div (fst c) (snd c)) * t))


--emcd, dados a y b, obtener tripla ((a,b),s,t) tal que sa + tb = (a : b)
emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a b = ((mcd a b), fst (buscarSyT (emcdAux a b []) 1 2), snd (buscarSyT (emcdAux a b []) 1 2))
