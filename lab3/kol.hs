import Data.List
import Data.Char

fun :: String -> Int
fun =  length . filter((>2) . length) . map(filter (`elem` vowels)) . words
  where vowels = "aeyiou" 

fun1 = \g -> map(g . (\x -> map ($x) [id,(+1),(+2)]))


fun2 = sum . map((^2) . length) . filter(all isUpper) . filter((=='K') . head) . words

fun3 xs =foldr (+) 0 . map((^3) . (+1)) . filter even . filter((<13) . (*2)) $ xs




