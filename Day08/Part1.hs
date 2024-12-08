import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (guard)

data Vector = Vector { vx :: Integer, vy :: Integer } deriving (Eq, Ord, Show)

instance Num Vector where
	v + w = Vector (vx v + vx w) (vy v + vy w)
	v - w = Vector (vx v - vx w) (vy v - vy w)
	negate v = Vector (negate $ vx v) (negate $ vy v)
	v * w = undefined
	fromInteger = undefined
	abs = undefined
	signum = undefined

parseMap :: String -> (Map Char (Set Vector), Set Vector)
parseMap s = (nodes, positions) where
	nodes = M.fromListWith (<>) $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		guard $ c /= '.'
		pure (c, S.singleton $ Vector x y)
	positions = S.fromList $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		pure $ Vector x y

antinodes :: Set Vector -> Set Vector
antinodes s = foldMap getAntinodes $ S.cartesianProduct s s where
	getAntinodes (v, w)
		| v == w = S.empty
		| otherwise = S.fromList [v + v - w, w + w - v]

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = do
	(nodes, positions) <- parseMap <$> getInput
	print $ length $ S.intersection positions $ foldMap antinodes $ nodes