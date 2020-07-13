module Main where

main :: IO ()
main = putStrLn "Hello World"

f :: Integer -> Integer
f x = (*) x x
  where y | x > 12 = 12
          | otherwise = x
 
