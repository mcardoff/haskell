import qualified Data.Map as M
import Data.List
import Data.Ord
import Data.Maybe
import Data.Function

type Edge v = (v,v)

data GridGraph v = GridGraph
    { ggWidth :: Int
    , ggHeight :: Int
    }

newtype MapGraph v = MapGraph
    { adjMap :: M.Map v [v]
    } deriving Show

class Graph g where
    adjacent :: Ord v => g v -> v -> [v]
    addEdge :: Ord v => Edge v -> g v -> g v
    emptyGraph :: Ord v => g v

instance Graph MapGraph where
    adjacent (MapGraph adj) v = concat $ maybeToList $ M.lookup v adj
    addEdge (k, v) (MapGraph adj) = MapGraph $
                        M.insert k (v : fromMaybe [] (M.lookup k adj)) adj
    emptyGraph = MapGraph M.empty

-- instance Graph GridGraph where
    -- adjacent (GridGraph w h) n =
        -- filter (\x -> x > w || x > h)
                   -- [succ n, pred n, (iterate succ n) !! w, (iterate pred n) !! w]
        -- Last cell is w * h

fromPairs :: (Ord v, Graph g) => [(v,v)] -> g v
fromPairs = foldr addEdge emptyGraph

type Wave v = [v]
type Trace v = M.Map v Int

-- bfs :: (Ord v, Graph g) => g v -> v -> Trace v
-- bfs graph start = bfs' graph [start] M.empty 0
--     where bfs' :: (Ord v, Graph g) => g v -> Wave v -> Trace v -> Int -> Trace v
--           bfs' _ [] trace _  = trace
--           bfs' graph wave trace step = bfs' graph wave' trace' (step+1)
--               where
--                 unvisited = filter (`M.notMember` trace) wave
--                 trace' = foldr (`M.insert` step) trace unvisited
--                 wave' = concatMap (adjacent graph) unvisited



testTree :: MapGraph Int
testTree = fromPairs [(1,2),(1,3),(1,4),(2,5),(2,6),(3,7),(4,8)]

testCycle :: MapGraph Int
testCycle = fromPairs [(1,2),(2,3),(3,1)]

testGrid :: GridGraph Int
testGrid = GridGraph 3 3

generatePairs :: GridGraph Int -> MapGraph Int
generatePairs (GridGraph x y) = fromPairs pairs
    where n = x * y - 1
          pairs = [(a,b) | a <- [1..x], b <- [1..y]]

-- eof
