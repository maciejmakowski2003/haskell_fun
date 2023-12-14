fiveToPower :: Integer -> Integer
fiveToPower = (^) 5

toPower5 :: Num a => a -> a
toPower5 = (^5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5-) 

subtr5From :: Num a => a -> a
subtr5From = flip (-) 5

flip2 :: (a -> b -> c) -> b -> a -> c  
flip2 f x y = f y x

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d 
flip3 f x y z = f z y x