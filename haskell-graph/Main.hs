import Data.List
import Data.Maybe
import Data.Function
import Data.Tuple
import qualified Data.Set as S
import qualified Data.Map as M

newtype MapGraph a = MapGraph
    { mapGraph :: M.Map a [a]
    } deriving Show

type Vertex a = a
type Edge a = (Vertex a, Vertex a)

data Graph a = Graph
    { verticies :: [a]
    , edges :: M.Map a [a]
    } deriving Show

data UGraph a = UGraph
    { uVertices :: S.Set a
    , uEdges :: M.Map a (S.Set a)
    } deriving Show

fromPairs :: Ord a => [(a,a)] -> Graph a
fromPairs pairs = foldr addEdge emptyGraph pairs

addEdge :: Ord a => (a,a) -> Graph a -> Graph a
addEdge (a,b) g = Graph vert' edge''
    where edge = edges g
          edge' =  M.insertWith union a [b] edge
          edge'' = M.insertWith union b [a] edge'
          vert = verticies g
          vert' = sort $ vert `union` [a,b]
          aAdjacents = adjacentList a g
          bAdjacents = adjacentList b g

emptyGraph :: Ord a => Graph a
emptyGraph = Graph [] M.empty

adjacentList :: Ord a => a -> Graph a -> [a]
adjacentList x = concat . maybeToList . adjacents x

adjacents :: Ord a => a -> Graph a -> Maybe [a]
adjacents v g | v `elem` verticies g = M.lookup v $ edges g
              | otherwise = Nothing

-- M.elems but for Graphs
edgeCons :: Ord a => Graph a -> [[a]]
edgeCons = M.elems . edges

type Path a = [a]

dfs :: Ord a => Graph a -> a -> Path a
dfs g v | v `elem` verticies g = dfs' [v] (adjacentList v g)
        | otherwise = []
    where dfs' ys [] = reverse ys
          dfs' ys (x:xs) | x `elem` ys = dfs' ys xs
                         | otherwise = dfs' (x:ys) xs

exampleGraph :: Graph Int
exampleGraph =   addEdge (2,3)
               $ addEdge (1,3)
               $ addEdge (2,1)
               $ addEdge (1,2)
               $ emptyGraph

f g xs ys= g $ xs ++ ys
-- EOF
