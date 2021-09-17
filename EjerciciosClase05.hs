--Aux1
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)


--Aux2
menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i k | (fact i) >= k = (fact i)
                        | otherwise = menorFactDesdeDesde (i + 1) k


menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m



--------------------------------------

--Aux3
menorFactHastaAux :: Int -> Int -> Int
menorFactHastaAux i k | (fact i) >= k = (fact (i - 1))
                        | otherwise = menorFactHastaAux (i + 1) k


menorFactHasta :: Int -> Int
menorFactHasta m = menorFactHastaAux 1 m


--------------------------------------

--Aux4
esFactAux :: Int -> Int -> Bool
esFactAux n z | (fact n) == z = True
              | n > z = False
              | otherwise = esFactAux (n+1) z

esFact :: Int -> Bool
esFact n = esFactAux 1 n


--------------------------------------

--Aux5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

--Aux6
esFibonacciAux :: Int -> Int -> Bool
esFibonacciAux n z | fib n == z = True
                   | n > z  = False
                   | otherwise = esFibonacciAux (n + 1) z

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciAux 0 n

--------------------------------------

--Aux7
esPrimoHasta :: Int -> Int -> Bool
esPrimoHasta n k | k == 1 = False
                 | k == 2 = True
                 | mod n (k-1) == 0 = False
                 | otherwise = esPrimoHasta n (k-1)

--Aux8
esPrimo :: Int -> Bool
esPrimo n = esPrimoHasta n n

--Aux9
minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n + 1)

--Aux10
nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

--Aux11
sumaPrimos :: Int -> Int
sumaPrimos 1 = 2
sumaPrimos n = nEsimoPrimo n + sumaPrimos (n-1)

--Aux12
esSumaInicialDePrimosAux :: Int -> Int -> Bool
esSumaInicialDePrimosAux n z | sumaPrimos n == z = True
                             | (sumaPrimos n) > z = False
                             | otherwise = esSumaInicialDePrimosAux (n+1) z

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosAux 1 n

--------------------------------------

--Aux13
sumaDivAux :: Int -> Int -> Int
sumaDivAux n 0 = 0
sumaDivAux n m | mod n m == 0 = sumaDivAux n (m-1) + m
               | otherwise = sumaDivAux n (m-1)

--Aux14
sumaDiv :: Int -> Int
sumaDiv n = sumaDivAux n n

--tomaValorMax:

--Aux15
sumaDivMaxAux :: Int -> Int -> Int
sumaDivMaxAux n z | sumaDiv n >= sumaDiv z = n
                  | otherwise = z

--Aux16
sumaDivMaxAux1 :: Int -> Int -> Int -> Int
sumaDivMaxAux1 n z i | i == n = sumaDivMaxAux n z
                     | n == z = sumaDivMaxAux n i
                     | sumaDiv z >= sumaDiv i && z >= i = sumaDivMaxAux1 n z (i-1)
                     | sumaDiv z >= sumaDiv i && z < i = sumaDivMaxAux1 n z (z-1)
                     | sumaDiv i > sumaDiv z && i > z = sumaDivMaxAux1 n (z-1) i
                     | sumaDiv i > sumaDiv z && z > i = sumaDivMaxAux1 n (i-1) i

tomaValorMax :: Int -> Int -> Int
tomaValorMax n z = sumaDivMaxAux1 n z z

--tomaValorMin:

--Aux17
sumaDivMinAux :: Int -> Int -> Int
sumaDivMinAux n z | sumaDiv n >= sumaDiv z = z
                  | otherwise = n

--Aux18
sumaDivMinAux1 :: Int -> Int -> Int -> Int
sumaDivMinAux1 n z i | n == i = sumaDivMinAux n z
                     | n == z = sumaDivMinAux n i
                     | sumaDiv z >= sumaDiv i && z >= i = sumaDivMinAux1 n (i-1) i
                     | sumaDiv z >= sumaDiv i && z < i = sumaDivMinAux1 n (z-1) i
                     | sumaDiv i > sumaDiv z && i > z = sumaDivMinAux1 n z (z-1)
                     | sumaDiv i > sumaDiv z && i < z = sumaDivMinAux1 n z (i-1)

tomaValorMin :: Int -> Int -> Int
tomaValorMin n z = sumaDivMinAux1 n z z

--------------------------------------

--Aux19
primosGemDesde :: Int -> Int -> Int
primosGemDesde z n | z == n = 0
                   | ((z+2) == n) && (esPrimo z == True) && (esPrimo (z+2) == True) = 1
                   | ((z+2) == n) && ((esPrimo z == False) || (esPrimo (z+2))) == False = 0
                   | (esPrimo z == True) && (esPrimo (z+2) == True) = 1 + (primosGemDesde (z+1) n)
                   | otherwise = primosGemDesde (z+1) n

primosGem :: Int -> Int
primosGem n = primosGemDesde 0 n

--------------------------------------

proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem n | esPrimo (n+1) && esPrimo (n+3) = ((n+1), (n+3))
                | otherwise = proxPrimosGem (n+1)

--------------------------------------

--Aux20
conjLotharCollatz :: Int -> Int
conjLotharCollatz n | mod n 2 == 0 = div n 2
                    | otherwise = (3*n) + 1

largoSecuencia :: Int -> Int
largoSecuencia n | n == 1 = 0
                 | otherwise = 1 + largoSecuencia (conjLotharCollatz n)

--------------------------------------

--Aux21
maxLargoSecuencia :: Int -> Int -> Int
maxLargoSecuencia n z | largoSecuencia n >= largoSecuencia z = n
                      | otherwise = z

--Aux22
maxSecLotharDesde :: Int -> Int -> Int -> Int
maxSecLotharDesde n z i | z == n = maxLargoSecuencia n i
                        | i == n = maxLargoSecuencia n z
                        | (maxLargoSecuencia z i) == z && z >= i = maxSecLotharDesde n z (z+1)
                        | (maxLargoSecuencia z i) == z && z < i = maxSecLotharDesde n z (i+1)
                        | (maxLargoSecuencia z i) == i && z >= i = maxSecLotharDesde n (z+1) i
                        | (maxLargoSecuencia z i) == i && z < i = maxSecLotharDesde n (i+1) i

maxSecLothar :: Int -> Int -> Int
maxSecLothar n z = maxSecLotharDesde n z z

--end
