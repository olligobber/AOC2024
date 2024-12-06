import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (guard)

getInput :: IO String
getInput = readFile "input"

data Terrain = Obstruction | Clear deriving (Eq, Ord, Show)

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

data Direction = North | East | South | West deriving (Eq, Ord, Show)

type Grid = Map Coord Terrain

move :: Direction -> Coord -> Coord
move North c = Coord (x c) (y c - 1)
move East c = Coord (x c + 1) (y c)
move South c = Coord (x c) (y c + 1)
move West c = Coord (x c - 1) (y c)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

parseMap :: String -> (Grid, Coord)
parseMap s = (fst <$> rawparse, fst $ M.findMin $ M.filter snd rawparse)
	where
		rawparse = M.fromListWith undefined $ do
			(y, l) <- zip [0..] $ lines s
			(x, c) <- zip [0..] $ l
			pure $ case c of
				'#' -> (Coord x y, (Obstruction, False))
				'.' -> (Coord x y, (Clear, False))
				'^' -> (Coord x y, (Clear, True))

data GuardState = GuardState { location :: Coord, direction :: Direction } deriving (Eq, Ord, Show)

tickGuard :: Grid -> GuardState -> Maybe GuardState
tickGuard grid guard = case grid !? move (direction guard) (location guard) of
	Nothing -> Nothing
	Just Obstruction -> Just $ GuardState (location guard) (turnRight $ direction guard)
	Just Clear -> Just $ GuardState (move (direction guard) (location guard)) (direction guard)

willLoop :: Ord a => (a -> Maybe a) -> a -> Bool
willLoop f x = go x S.empty where
	go y s = case f y of
		_ | y `S.member` s -> True
		Nothing -> False
		Just z -> go z (S.insert y s)

allLoops :: Grid -> GuardState -> [Coord]
allLoops grid start = do
	(coord, old) <- M.toList grid
	guard $ old == Clear
	let newgrid = M.insert coord Obstruction grid
	guard $ willLoop (tickGuard newgrid) start
	pure coord


main :: IO ()
main = do
	(grid, guardpos) <- parseMap <$> getInput
	print $ length $ allLoops grid $ GuardState guardpos North