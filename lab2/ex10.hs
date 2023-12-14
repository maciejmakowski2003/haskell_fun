{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False 

divides2 :: Integral a => [a] -> Bool
divides2 (x : y : _) 
    | mod y x == 0 = True 
    | otherwise = False 

divides3 (x : y : z : _) 
    | divides2 [x,z] = True
    | otherwise = False

