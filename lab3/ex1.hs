f1 = \x -> x-2; 
f2 = \x y -> sqrt(x^2 + y^2)
f3 = \x -> 2^x 
f4 = \x -> if x `mod` 2 == 0 then True else False
f5 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)
f6 = \x -> case x of
                    1 -> 3
                    2 -> 2
                    _ -> 1
