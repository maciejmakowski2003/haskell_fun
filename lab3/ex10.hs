--zip [1,2,3] ['a','b']- [(1,'a'),(2,'b')] 
--unzip [(1,'a'),(2,'b')]- ([1,2],['a','b']) 
--zipWith (,) [1,2,3] ['a','b']- [(1,'a'),(2,'b')]

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ [a <= b | (a, b) <- zip xs (tail xs)]

everySecond :: [t] -> [t]
everySecond xs = [x | (i, x) <- zip [1..] xs, even i]

isSorted :: Ord a => [a] -> Bool
isSorted xs = (and $ zipWith (<=) xs (tail xs)) || (and $ zipWith (>=) xs (tail xs))

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

unzip3' :: [(a, b, c)] -> ([a], [b], [c])
unzip3' [] = ([], [], [])
unzip3' ((x, y, z):xyzs) = (x : xs, y : ys, z : zs)
  where
    (xs, ys, zs) = unzip3' xyzs

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

