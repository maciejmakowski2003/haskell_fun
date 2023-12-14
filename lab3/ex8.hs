import Data.Char 

sqrElems = map (\x -> x^2) 
lowerCase = map (\e -> toLower e) 

map' :: (a -> b) -> [a] -> [b]
map' _ [] = [] 
map' f (x:xs) = f x : map' f xs

--length . filter even $ doubleElems [1..10^7] 
--length . filter even . map (*2) $ [1..10^7]
--length . filter even $ map (*2)  [1..10^7]

evalFuncListAt fs x = map (\f -> f x) fs