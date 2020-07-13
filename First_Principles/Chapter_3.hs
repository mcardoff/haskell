module Chapter_3 where

main :: IO ()
main = putStrLn "Hello World!"


thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = s !! x
  where s = "hello world"

rvrs :: String -> String
rvrs = unwords . reverse . words

reverse' :: [x] -> [x]
reverse' xs = rv xs []
  where rv [] ys = ys
        rv (x:xs) ys = rv xs (x:ys)

reverse'' :: [x] -> [x]
reverse'' = foldl (flip (:)) []
