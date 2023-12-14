sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs 

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' :: (Num b) => (a -> b) -> [a] -> b
sumWith'' g = foldr' (\x acc -> g x + acc) 0

prodWith'' :: (Num b) => (a -> b) -> [a] -> b
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z 
foldl' f z (x:xs) = foldl' f (f z x) xs 

sumWith''' g  = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1

--reverse 
--foldl (\acc x -> x : acc) [] list1To5
--foldr (\x xs -> xs ++ [x]) [] list1To5

--max min
--flodr1 - no base case 
--flodrl - no base case 
--flodl' - no thunks
--foldr1 max listRand

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs 

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = foldl (\acc x -> if p x then acc ++ [x] else acc) [] xs 

foldl2 f z xs = foldr (\x acc -> f acc x) z (reverse xs)
foldr2 f z xs = foldl (\acc x -> f x acc) z (reverse xs)



