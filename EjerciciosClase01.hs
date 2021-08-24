absoluto :: Int -> Int
absoluto n | n > 0 = n
           | otherwise = -n

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | d>=f = d
                   | d<f = f
                  where d = (absoluto x)
                        f = (absoluto y)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z =  x
              | y > z = y
              | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 n z | n == 0 || z == 0 = True
              | otherwise = False


algunoEs0' :: Float -> Float -> Bool
algunoEs0' n z | n > 0 && z > 0 = False
               | n < 0 && z < 0 = False
               | otherwise = True

ambosSon0 :: Float -> Float -> Bool
ambosSon0 n z | n == 0 && z == 0 = True
              | otherwise = False

ambosSon0' :: Float -> Float -> Bool
ambosSon0' n z | n > 0 || n < 0 = False
               | z > 0 || z < 0 = False
               | otherwise = True

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False


digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = (div(mod x 100)10)


-----------------------------------------

-- Zoom Exercises

juegaAlFutbol :: Int -> Bool
juegaAlFutbol n | n == 7 || n == 14 || n == 21 || n == 28 || n == 5 || n == 12 || n == 19 || n == 26 = True
                | otherwise = False

digitoPreciso :: Int -> Int -> Int
digitoPreciso n 1 = mod n 10
digitoPreciso n k = (div (mod n (10^k)) (10^(k-1)))
