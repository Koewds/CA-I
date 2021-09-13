g1 :: Int -> Int -> Int
g1 i n | i > n = 0
g1 i n | i == n = i^n
g1 i n = g1 i (n-1) + i^n



--Auxiliares g2

--Aux1
g2' :: Int -> Int -> Int
g2' 0 n = 0
g2' k n = g2' (k-1) n + k^n

--Aux2
g2'' :: Int -> Int -> Int
g2'' 0 n = 0
g2'' k n = g2'' (k-1) n + n^k

g2 :: Int -> Int
g2 0 = 0
g2 n = g2 (n-1) + (g2' n n) + (g2'' n n) - n^n


-----------


g3 :: Int -> Int
g3 0 = 0
g3 1 = 2
g3 n | mod n 2 == 0 = g3 (n-2) + 2^(n-1)
     | otherwise = g3 (n-2) + 2^n


-----------

--Auxiliares g4

--equalDigits
equalDigits :: Int -> Bool
equalDigits n | n < 10 = True
              | otherwise = mod n 10 == mod (div n 10) 10 && equalDigits (div n 10)


g4 :: Int -> Int
g4 0 = 0
g4 n | equalDigits n == True = g4 (n-1) + n
     | otherwise = g4 (n-1)
