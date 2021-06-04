import Data.List
import Data.Function
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

type Vertex = Int

type LVertex a = (Vertex, a)

type Edge = (Vertex, Vertex)

type LEdge a = (Vertex, Vertex, a)

type AdjList = [Edge]

type LAdjList a = [LEdge a]

type AdjMap = M.Map Vertex [Vertex]

elems :: M.Map k a -> [a]
elems = M.elems

keys :: M.Map k a -> [k]
keys = M.keys

toAdjMap :: AdjList -> AdjMap
toAdjMap = M.fromList . map (\xs -> (fst $ head xs, map snd xs))
           . groupBy ((==) `on` fst)
           . sort


testAdjList :: AdjList
testAdjList = [(1,2),(2,3),(2,4),(3,4)]

testUndirList :: AdjList
testUndirList = undirect testAdjList

testMap :: AdjMap
testMap = toAdjMap testAdjList

undirMap :: AdjMap
undirMap = toAdjMap testUndirList

getFirstKey :: M.Map k [a] -> k
getFirstKey = head . keys

getFirstVal :: Ord k => M.Map k [a] -> [a]
getFirstVal = head . elems

undirect :: [(a,a)] -> [(a,a)]
undirect x = x ++ (map swap x)

-- testAll :: AdjMap -> Bool
testAll m = f k a
    where
      k = getFirstKey m
      a = getFirstVal m
      f k [] = undefined
      f k (x:xs) = undefined

adjacents :: Vertex -> AdjMap -> [Vertex]
adjacents v m = (concat . maybeToList) $ M.lookup v m

findCycle :: AdjMap -> [Vertex]
findCycle m = undefined

--eof
