import Data.Char (toUpper)
isPalindrome :: [Char] -> Bool 
isPalindrome xs = xs == reverse xs 

getElementAtIdx :: [a] -> Int -> a
getElementAtIdx xs idx = head (drop idx xs) 

capitalize :: [Char] -> [Char] 
capitalize xs = [toUpper c| c<-xs]