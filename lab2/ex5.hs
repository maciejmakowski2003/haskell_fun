isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0] 

isPrime2 :: Int -> Bool
isPrime2 n
  | n < 2     = False
  | otherwise = n `elem` (takeWhile (<= n) primes) 


primesCount :: Int -> Int 
primesCount n = length (takeWhile (<= n) primes) 


allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:xs) = (x == y) && allEqual (y:xs)