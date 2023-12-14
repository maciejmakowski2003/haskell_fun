sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0 
sumWith f (x:xs) = f x + sumWith f xs

sum'    = sumWith (\x -> x)
sumSqr  = sumWith (\x -> x^2)
sumCube = sumWith (\x -> x^3)
sumAbs  = sumWith (\x -> abs x)
-- sumWith (\x -> x^5) [1..15]
listLength = sumWith (\x -> 1)

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod     = prodWith (\x -> x)
prodSqr  = prodWith (\x -> x^2)
prodCube = prodWith (\x -> x^3)
prodAbs  = prodWith (\x -> abs x) 

with f g c [] = c
with f g c (x:xs) = g(f x, with f g c xs)
sumSqr' = with (\x -> x^2) (\(x,y) -> x + y) 0
