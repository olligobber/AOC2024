import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid (Sum(..))

data Height = Height { fromHeight :: Int } deriving (Eq, Ord, Show)

step :: Height -> Height
step h = Height $ fromHeight h + 1

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

type TopMap = Map Coord Height

neighbours :: Coord -> [Coord]
neighbours c =
	[ Coord (x c) (y c - 1)
	, Coord (x c + 1) (y c)
	, Coord (x c) (y c + 1)
	, Coord (x c - 1) (y c)
	]

parseMap :: String -> TopMap
parseMap s = M.fromListWith undefined $ do
	(y, l) <- zip [1..] $ lines s
	(x, c) <- zip [1..] l
	pure (Coord x y, Height $ read $ pure c)

getTrails :: TopMap -> Map Coord (Sum Integer)
getTrails topmap = foldl updatemap startmap (Height <$> [8,7..0]) where
	startmap =
		M.mapWithKey (\_ _ -> Sum 1) $
		M.filter (== Height 9) topmap
	updatemap uppermap h =
		M.mapWithKey (\k _ -> foldMap (\n -> M.findWithDefault mempty n uppermap) $ neighbours k) $
		M.filter (==h) topmap

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . getSum . foldMap id . getTrails . parseMap