> module Chapter_1_Lit where
> import Data.Char

Hello there! This is literate Haskell! The first chapter in HPFFP involves lambda
calculus, it involves using anonymous functions called lambdas. Usually we defined a
function as:

f(x) = v

Where v is a unique value for every x. However lambdas are defined without a name, theyre always 

> num :: Int
> num = 12
> 
> multbynum :: Int -> Int
> multbynum = (*num)
 
