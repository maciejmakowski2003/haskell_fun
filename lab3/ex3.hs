sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n x = sum [x**k / product [1..k] | k <- [0..fromIntegral n]]

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f x+h - f x)/h 

d2f :: (Double -> Double) -> Double -> Double -> Double
d2f f h = \x -> (f (x + h) - 2 * f x + f (x - h)) / (h * h)