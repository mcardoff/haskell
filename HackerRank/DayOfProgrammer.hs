module DayOfProgrammer where

-- Either 12.09.year or 13.09.year first if reg second if leap
leapDayG :: Int -> Bool
leapDayG y = (y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0)

leapDayJ :: Int -> Bool
leapDayJ y = (y `mod` 4 == 0)

solveG :: Int -> String
solveG y | leapDayG y = "13.09."++ show y
         | otherwise = "12.09."++show y

solveJ :: Int -> String
solveJ y | leapDayJ y = "13.09."++ show y
         | otherwise = "12.09."++show y

solve :: Int -> String
solve y | y > 1917 = solveG y
        | y == 1918 = "26.09.1918"
        | otherwise = solveJ y

main :: IO ()
main = interact $ solve . read
