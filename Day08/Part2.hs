import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (guard)

data Vector = Vector { vx :: Int, vy :: Int } deriving (Eq, Ord, Show)

instance Num Vector where
	v + w = Vector (vx v + vx w) (vy v + vy w)
	v - w = Vector (vx v - vx w) (vy v - vy w)
	negate v = Vector (negate $ vx v) (negate $ vy v)
	v * w = undefined
	fromInteger = undefined
	abs = undefined
	signum = undefined

data Bounds = Bounds { minx :: Int, miny :: Int, maxx :: Int, maxy :: Int } deriving (Eq, Ord, Show)

inbounds :: Vector -> Bounds -> Bool
inbounds v b = vx v <= maxx b && vx v >= minx b && vy v <= maxy b && vy v >= miny b

minimise :: Vector -> Vector
minimise v = Vector (vx v `div` g) (vy v `div` g) where g = gcd (vx v) (vy v)

parseMap :: String -> (Map Char (Set Vector), Bounds)
parseMap s = (nodes, bounds) where
	nodes = M.fromListWith (<>) $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		guard $ c /= '.'
		pure (c, S.singleton $ Vector x y)
	bounds = Bounds 1 1 (length $ head $ lines s) (length $ lines s)

antinodes :: Bounds -> Set Vector -> Set Vector
antinodes b s = foldMap getAntinodes $ S.cartesianProduct s s where
	getAntinodes (v, w)
		| v == w = S.empty
		| otherwise = S.fromList $ go v (minimise $ v - w) <> go v (negate $ minimise $ v - w)
	go v d
		| v `inbounds` b = v : go (v+d) d
		| otherwise = []

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = do
	(nodes, bounds) <- parseMap <$> getInput
	print $ length $ foldMap (antinodes bounds) $ nodes