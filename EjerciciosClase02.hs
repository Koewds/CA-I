prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = (vx * wx) + (vy * wy)

prodInt' :: Num t => (t, t) -> (t, t) -> t
prodInt' a b = (fst a)*(fst b) + (snd a)*(snd b)


todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (vx, vy) (wx, wy) = vx < wx && vy < wy

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos a b = sqrt ((fst a - fst b)^2 + (snd a - snd b)^2)


sumaTerna :: (Float, Float, Float) -> Float
sumaTerna (a, b, c) = a + b + c

posicPrimerPar :: Integral p => (p, p, p) -> p
posicPrimerPar (a, b, c) | mod a 2 == 0 = 1
                         | mod b 2 == 0 = 2
                         | mod c 2 == 0 = 3
                         | otherwise = 4

crearPar a b = (a, b)


invertir (n, z) = (z, n)
