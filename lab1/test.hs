sgn :: Int -> Int 
sgn n | n>0 = 1
      | n == 0 = 0
      | otherwise = -1

absInt :: Int -> Int 
absInt n | n>=0 = n 
         | otherwise = -n 


f1 x = if x == 1 
    then 3
    else if x == 2
        then 10
        else 1



