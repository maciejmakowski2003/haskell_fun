absInt :: Int -> Int
absInt n = case (n >= 0) of
   True -> n
   _    -> -n