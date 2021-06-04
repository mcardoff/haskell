module Chapter_9 where


myWords :: String -> [String]
myWords [] = []
myWords xs = a : rest
    where a = takeWhile (/=' ') xs
          b = dropWhile (/=' ') xs
          thing = dropWhile (==' ') b
          rest = myWords thing

-- First thing gets indexed last
myComp :: [(Int,Int)]
myComp = [(i,j) | i <- [1..5], j <- [1..5]]
