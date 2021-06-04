module MP1 where


transposeTup :: ((a,a),(a,a)) -> ((a,a),(a,a))
transposeTup ((a,b),(c,d)) = ((a,c),(b,d))

compoundInterest :: Floating a => a -> a -> Int -> a
compoundInterest p r 1 = r
compoundInterest p r n = r

collatzLen :: Integer -> Integer
collatzLen 1 = 1
collatzLen n = 1 + collatzLen z
    where z | n `mod` 2 == 0 = n `div` 2
            | otherwise = 3 * n + 1


nest f v 0 = v
nest f v n = f $ nest f v (n-1)
