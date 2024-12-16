import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Control.Monad.State.Lazy (State, get, put, evalState)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Control.Monad (guard)
import Data.Set (Set, member)
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

data Square = Wall | Clear | End deriving (Eq, Ord, Show)

parseSquare :: Char -> Square
parseSquare '#' = Wall
parseSquare '.' = Clear
parseSquare 'E' = End
parseSquare 'S' = Clear

type Maze = Map Coord Square

data Location = Location { coord :: Coord, direction :: Direction } deriving (Eq, Ord, Show)

parseMap :: String -> (Maze, Location)
parseMap s = (maze, location) where
	maze = M.fromListWith undefined $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		pure (Coord x y, parseSquare c)
	location = head $ do
		(y, l) <- zip [1..] $ lines s
		(x, c) <- zip [1..] l
		guard $ c == 'S'
		pure $ Location (Coord x y) East

data DijkstraState = DS { pq :: PQueue Integer Location, visited :: Set Location } deriving (Eq, Ord, Show)

dijkstra :: Maze -> State DijkstraState Integer
dijkstra m = do
	s <- get
	case PQ.minViewWithKey $ pq s of
		Nothing -> error "No path"
		Just ((_, here), popped) | here `member` visited s -> do
			put $ DS popped $ visited s
			dijkstra m
		Just ((val, here), popped) -> case m ! coord here of
			End -> pure val
			Wall -> do
				put $ DS popped $ S.insert here $ visited s
				dijkstra m
			Clear -> do
				put $ DS
					(PQ.insert (val+1) (Location (move (direction here) (coord here)) (direction here)) $
					PQ.insert (val+1000) (Location (coord here) (fst $ adjacent $ direction here)) $
					PQ.insert (val+1000) (Location (coord here) (snd $ adjacent $ direction here)) $
					popped)
					(S.insert here $ visited s)
				dijkstra m

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = do
	(maze, startloc) <- parseMap <$> getInput
	print $ evalState (dijkstra maze) $ DS (PQ.singleton 0 startloc) S.empty
