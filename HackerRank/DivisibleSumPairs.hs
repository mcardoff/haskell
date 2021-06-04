module DivisibleSumPairs where

solve :: [Int] -> Int
solve (k:a) = length $ [() | (i,x) <- zip [0..] a,
                             (j,y) <- zip [0..] a,
                             i < j, (x+y) `mod` k == 0]


main :: IO ()
main = interact $ show . solve . map read . tail . words
