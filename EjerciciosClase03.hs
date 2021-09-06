-- Dado un número natural, determinar si es múltiplo de 3 (con recursión)
multiplo3 :: Int -> Bool
multiplo3 n | n == 0 = True
            | n < 3  = False
            | n == 3 = True
            | otherwise = multiplo3 (n - 3)


--Dado n perteneciente a N, sumar los primeros n números impares.
sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 4
              | n == 3 = 9
              | n == 0 = 0
              | mod n 2 == 0 = 4 + sumaImpares (n - 2)
              | otherwise = 6 + sumaImpares (n - 2)
--(En los impares, como el patrón se empieza a dar en n=3, resultan 3 casos base para que n=1 esté incluido)

sumaImpares' :: Int -> Int
sumaImpares' n | n == 1 = 1
               | otherwise = sumaImpares' (n - 1) + (2*n) -1

medioFact :: Int -> Int
medioFact n | n == 1 = 1
            | n == 2 = 2
            | otherwise = n * medioFact (n - 2)

--Suma de dígitos
sumDigits :: Int -> Int
sumDigits n | n == 0 = 0
            | otherwise = sumDigits (div n 10) + mod n 10




--Determinar si todos los dígitos de un número son iguales.
equalDigits :: Int -> Bool
equalDigits n | n < 10 = True
              | otherwise = mod n 10 == mod (div n 10) 10 && equalDigits (div n 10)





--Determinar si todos los dígitos de un número son iguales. (skip)
equalDigits' :: Int -> Bool
equalDigits' n | mod n 10 == mod (div n 10) 10 = True
               | otherwise = False
