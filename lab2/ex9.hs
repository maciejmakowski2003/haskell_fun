qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<=x) xs 
   rightPart xs = filter (>x) xs


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs 


mSort :: Ord a => [a] -> [a] 
mSort [] = [] 
mSort [x] = [x] 
mSort xs = merge (mSort left) (mSort right) 
  where (left,right) = split xs

concat' :: [[a]] -> [a] 
concat' [] = [] 
concat' (x:xs) = x ++ concat' xs 

concat'' :: [[a]] -> [a] 
concat'' lists = [x | sublist <- lists, x <- sublist]


isSorted :: [Int] -> Bool -- isSorted [1,2,2,3] = True
isSorted [] = True 
isSorted [x] = True 
isSorted (x:y:xs) = (x<=y) && isSorted(y:xs)

reverse' :: [a] -> [a] -- reverse [1,2,3] = [3,2,1]
reverse' [] = [] 
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)] -- zip' [1,2] [3,4] = [(1,3), (2,4)]
zip' [] _ = [] 
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a, b)] -> ([a],[b]) -- unzip [(1,2), (3,4)] = ([1,3],[2,4])
unzip' []           = ([], []) 
unzip' ((x, y):rest) = (x : xs, y : ys)
  where
    (xs, ys) = unzip' rest

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = [] 
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool -- subList [1,2] [3,1,2,4] = True
subList [] ys = True 
subList (x:xs) ys = x `elem` ys && subList xs ys
