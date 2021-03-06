module Chapter_4 where

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]
also :: [String]
also = ["Quake", "The Simons"]
allAwesome :: [[String]]
allAwesome = [awesome, also]

test1 = 6 / 3
test2 = div 6 $ length [1..3]

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs
                

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f a b = ((snd a, snd b),(fst a,fst b))


f' xs = w + 1
  where w = length xs

f'' = fst

thing :: Maybe Int
thing = 1234 <$ Just 12
