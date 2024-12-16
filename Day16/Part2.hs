import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Control.Monad.State.Lazy (State, get, put, runState)
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as M
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as S

data Coord = Coord { x :: Integer, y :: Integer } deriving (Eq, Ord, Show)

data Direction = North | East | South | West deriving (Eq, Ord, Show)

move :: Direction -> Coord -> Coord
move North c = Coord (x c) (y c - 1)
move East c = Coord (x c + 1) (y c)
move South c = Coord (x c) (y c + 1)
move West c = Coord (x c - 1) (y c)

adjacent :: Direction -> (Direction, Direction)
adjacent North = (East, West)
adjacent South = (East, West)
adjacent East = (North, South)
adjacent West = (North, South)

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite West = East
opposite South = North

data Square = Wall | Clear | End | Start deriving (Eq, Ord, Show)

parseSquare :: Char -> Square
parseSquare '#' = Wall
parseSquare '.' = Clear
parseSquare 'E' = End
parseSquare 'S' = Start

type Maze = Map Coord Square

data Location = Location { coord :: Coord, direction :: Direction } deriving (Eq, Ord, Show)

neighbours :: Location -> [(Integer, Location)]
neighbours here =
	[ (1, Location (move (direction here) (coord here)) (direction here))
	, (1000, Location (coord here) (fst $ adjacent $ direction here))
	, (1000, Location (coord here) (snd $ adjacent $ direction here))
	]

revNeighbours :: Location -> [(Integer, Location)]
revNeighbours here =
	[ (1, Location (move (opposite $ direction here) (coord here)) (direction here))
	, (1000, Location (coord here) (fst $ adjacent $ direction here))
	, (1000, Location (coord here) (snd $ adjacent $ direction here))
	]

parseMap :: String -> (Maze, Location)
parseMap s = (maze, start) where
	maze = M.fromListWith undefined $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		pure (Coord x y, parseSquare c)
	start = head $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		guard $ c == 'S'
		pure $ Location (Coord x y) East

data DijkstraState = DS { pq :: PQueue Integer Location, visited :: Map Location Integer } deriving (Eq, Ord, Show)

dijkstra :: Maze -> State DijkstraState (Set Location)
dijkstra m = do
	s <- get
	case PQ.minViewWithKey $ pq s of
		Nothing -> error "No path"
		Just ((_, here), popped) | here `M.member` visited s -> do
			put $ DS popped $ visited s
			dijkstra m
		Just ((val, here), popped) -> case m ! coord here of
			End -> do
				put $ DS popped $ M.insert here val $ visited s
				S.insert here <$> finishDijkstra m val
			Wall -> do
				put $ DS popped $ visited s
				dijkstra m
			_ -> do
				put $ DS
					(foldl (\p (v, l) -> PQ.insert (val+v) l p) popped (neighbours here))
					(M.insert here val $ visited s)
				dijkstra m

finishDijkstra :: Maze -> Integer -> State DijkstraState (Set Location)
finishDijkstra m v = do
	s <- get
	case PQ.minViewWithKey $ pq s of
		Nothing -> pure S.empty
		Just ((val, _), _) | val > v -> pure S.empty
		Just ((val, here), popped) -> case m ! coord here of
			End -> do
				put $ DS popped $ M.insert here val $ visited s
				S.insert here <$> finishDijkstra m val
			_ -> do
				put $ DS popped $ M.insert here val $ visited s
				finishDijkstra m val

backtrack :: Maze -> Map Location Integer -> Location -> Set Coord
backtrack maze visited here = case maze ! coord here of
	Wall -> S.empty
	Start -> S.singleton $ coord here
	_ -> S.insert (coord here) $ foldMap (backtrack maze visited) $ do
		(v, l) <- revNeighbours here
		guard $ visited !? l == Just (val - v)
		pure l
	where
		val = visited ! here

getInput :: IO String
getInput = readFile "input"
-- getInput = readFile "demo"

main :: IO ()
main = do
	(maze, startloc) <- parseMap <$> getInput
	let (endlocs, state) = runState (dijkstra maze) $ DS (PQ.singleton 0 startloc) M.empty
	let vis = visited state
	-- print vis
	print $ length $ foldMap (backtrack maze vis) endlocs
