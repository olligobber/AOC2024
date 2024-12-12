import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as St
import Data.Foldable (traverse_)
import Data.List (group)

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

neighbours :: Coord -> [Coord]
neighbours c =
	[ Coord (x c) (y c - 1)
	, Coord (x c + 1) (y c)
	, Coord (x c) (y c + 1)
	, Coord (x c - 1) (y c)
	]

type Board = Map Coord Char

type Region = Set Coord

parseBoard :: String -> Board
parseBoard s = M.fromListWith undefined $ do
	(x, l) <- zip [1..] $ lines s
	(y, c) <- zip [1..] l
	pure (Coord x y, c)

floodFill :: Board -> Coord -> Region
floodFill board c = St.execState (floodFillS board c) Set.empty

floodFillS :: Board -> Coord -> State Region ()
floodFillS board c = do
	let h = board M.!? c
	s <- St.get
	if c `Set.member` s then
		pure ()
	else do
		St.modify $ Set.insert c
		traverse_ (floodFillS board) $ filter ((== h) . (board M.!?)) $ neighbours c

data Direction = North | East | South | West deriving (Eq, Ord, Show)

allDirs :: [Direction]
allDirs = [North, East, South, West]

move :: Direction -> Coord -> Coord
move North c = Coord (x c) (y c - 1)
move East c = Coord (x c + 1) (y c)
move South c = Coord (x c) (y c + 1)
move West c = Coord (x c - 1) (y c)

adjacent :: Direction -> [Direction]
adjacent North = [East, West]
adjacent East = [North, South]
adjacent South = [East, West]
adjacent West = [North, South]

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

data WalkState = WalkState {position :: Coord, facing :: Direction, outside :: Direction} deriving (Eq, Ord, Show)

stepWalk :: Region -> WalkState -> WalkState
stepWalk s ws
	| not infront = WalkState (position ws) (opposite $ outside ws) (facing ws)
	| not frontout = WalkState (move (facing ws) (position ws)) (facing ws) (outside ws)
	| otherwise = WalkState (move (outside ws) (move (facing ws) (position ws))) (outside ws) (opposite $ facing ws)
	where
		infront = move (facing ws) (position ws) `Set.member` s
		frontout = move (outside ws) (move (facing ws) (position ws)) `Set.member` s

fullWalk :: Region -> WalkState -> [WalkState]
fullWalk region start = start : takeWhile (/= start) (tail $ iterate (stepWalk region) start)

seenPair :: WalkState -> (Coord, Coord)
seenPair ws = (position ws, move (outside ws) (position ws))

walkRegion :: Region -> Integer
walkRegion region = sum $ toInteger . length . group <$> allWalks where
	perimPairs = [(a,b) | a <- Set.toList region, b <- neighbours a, not $ b `Set.member` region]
	allWalks = snd $ foldl tryWalk (Set.empty, []) perimPairs
	tryWalk (seen, prevwalks) p
		| p `Set.member` seen = (seen, prevwalks)
		| otherwise = let
			start = WalkState perimeter (head $ adjacent outdir) outdir
			(perimeter, outside) = p
			outdir = head [d | d <- allDirs, move d perimeter == outside]
			fw = fullWalk region start
			dirs = facing <$> fw
			normdirs =
				if head dirs == last dirs then
					uncurry (flip (<>)) $ span (== head dirs) $ dirs
				else
					dirs
			in (seen <> Set.fromList (seenPair <$> fw), normdirs:prevwalks)

valueRegion :: Region -> Integer
valueRegion s = perimeter * area where
	area = toInteger $ length s
	perimeter = walkRegion s

valueRegionsS :: Board -> Coord -> State Region Integer
valueRegionsS board c = do
	s <- St.get
	if c `Set.member` s then
		pure 0
	else do
		let r = floodFill board c
		St.modify (<> r)
		pure $ valueRegion r

valueRegions :: Board -> Integer
valueRegions board =
	sum $
	flip St.evalState Set.empty $
	traverse (valueRegionsS board) $
	Set.toList $
	M.keysSet board

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . valueRegions . parseBoard