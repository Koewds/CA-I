

--Aux0
esSumaDeDosCubosDesde :: Integer -> Integer -> Bool
esSumaDeDosCubosDesde n z | z^3 > (div n 2) = False
                          | esUnCubo (n - (z^3)) = True
                          | otherwise = esSumaDeDosCubosDesde n (z+1)

esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = esSumaDeDosCubosDesde n 1

--Aux1
descomposicionCubosDesde :: Integer -> Integer -> (Integer, Integer)
descomposicionCubosDesde n z | z^3 > (div n 2) = (0, 0)
                             | esUnCubo (n - (z^3)) == True = (z, round (fromIntegral (n - (z^3))**(1/3)))
                             | otherwise = descomposicionCubosDesde n (z+1)

descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = descomposicionCubosDesde n 1

--Aux2
cantidadDeFormasDesde :: Integer -> Integer -> Integer
cantidadDeFormasDesde n z | z^3 > (div n 2) = 0
                          | esUnCubo (n - (z^3)) = 1 + cantidadDeFormasDesde n (z+1)
                          | otherwise = cantidadDeFormasDesde n (z+1)

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = cantidadDeFormasDesde n 1

--especialDesde :: Integer -> Integer

--especialNumero :: Integer -> Integer

--esMuyEspecial :: Integer -> Bool


esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x
