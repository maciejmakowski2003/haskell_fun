data D1 = D1 {s::String, i::Int} deriving Show

nonEmpty:: String -> Either String String
nonEmpty "" = Left "Empty!" 
nonEmpty s = Right s 

nonNegative:: Int -> Either String Int
nonNegative n = if n>=0 then Right n else Left "Negative!"

validate :: D1 -> Either String D1
validate d = if even $ i d then Right d else Left "Odd!"

createD1 :: String -> Int -> Either String D1
createD1 s1 i1 = nonEmpty s1 >>= \s -> nonNegative i1  >>= \i -> validate $ D1 s i

parseInt :: String -> Maybe (Int, [Char])
parseInt s = case reads s :: [(Int, [Char])] of
    [(n, "")] -> Just (n, "")
    _         -> Nothing

parseSep' :: String -> Maybe ([Char], [Char])
parseSep' s = case break (== ':') s of
    (x, ':':y) -> Just (x, y)
    _          -> Nothing

parse2Ints :: String -> Maybe (Int, Int, [Char])

parse2Ints s = parseInt s >>= \(i1,s1)-> parseSep' s1 >>= \(_,s2) -> 
    parseInt s2 >>= \(i2,s3) -> 
        let minVal = 10 
        in return (max i1 minVal, max i2 minVal, s3)