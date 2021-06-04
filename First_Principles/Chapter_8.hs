module Chapter_8 where

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)


applyN :: Integer -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = f (applyN (n-1) f a)


mc91 n | n > 100 = n - 10
       | otherwise = mc91 $ mc91 $ n + 11
