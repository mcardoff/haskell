module CartProd where
import Control.Applicative

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f a b = f <$> a <*> b

pairs :: [a] -> [a] -> [[a]]
pairs = liftA2 (\x y -> [x,y])

test xs = liftA2 (:) xs (pairs xs xs)

power :: Int -> [a] -> [[a]]
power 0 _ = []
power 1 xs = [xs]
power 2 xs = liftA2 (\x y -> [x,y]) xs xs
power n xs = [y:ys | y <- xs, ys <- power (n-1) xs]

l#n = mapM (const l) [1..n]


-- Cartesian power extender:
-- liftA2 (:)

test2 :: [[a]] -> [[a]]
test2 = foldr (\x y -> [xs:ys | xs <- x, ys <- y]) [[]]
