fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1) 

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

fib2 :: Int->Int
fib2 n = head (take n fibs :: [Int]) 

sum1 :: Num a => [a] -> a
sum1 []   = 0
sum1 (x:xs) = x + sum1 xs 


prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 1 
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0 
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False 
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = False 
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' n [] = False 
elem' n (x:xs) = x == n || elem' n xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll []     = []    
doubleAll (x:xs) = 2*x : doubleAll xs

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = [] 
squareAll (x:xs) = x^2 : squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = [] 
selectEven (x:xs)
    | even x = x:selectEven xs 
    | otherwise = selectEven xs


