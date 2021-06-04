module MigratoryBirds where
import Data.List
import Data.Function

solve :: [Int] -> Int
solve = head . head . sortBy (flip compare `on` length) . group . sort

main :: IO ()
main = interact $ show . solve . map read . words
