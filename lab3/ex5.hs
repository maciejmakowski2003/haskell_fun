import Data.List(sort,reverse)
sortDesc :: Ord a => [a] -> [a]
sortDesc xs = reverse $ sort xs

sortDesc' :: Ord a => [a] -> [a]
sortDesc' = reverse . sort 

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool 
are2FunsEqAt f g xs = all(\x -> f x == g x) xs

composeFunList :: [a -> a] -> (a -> a)
composeFunList [] = id 
composeFunList (f:fs) = f . composeFunList fs 


lista = [\x -> x+1, \x -> 2*x, \x -> 2^x] 