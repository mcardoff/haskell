module Chapter_7 where
-- Functions can be entities in lists
funList :: Num a => [(a -> a -> a)]
funList = [(+), (*),(-)]


mTh x y z = x * y * z
mTh' x y = \z -> x * y * z
mTh'' x = \y -> \z -> x * y * z
mTh''' = \x -> \y -> \z -> x * y * z

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = Unregistered | Registered Username AccountNumber


ifEvenAdd2 n = case even n of
                 True -> n+2
                 False -> n




--
