import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Prelude hiding (map)
import Data.Set (Set, member)
import qualified Data.Set as S
import Control.Monad.State.Lazy (State, gets, modify, evalState)

massInsert :: (Foldable t, Ord k) => t k -> v -> Map k v -> Map k v
massInsert ks v m = foldl (\n k -> M.insert k v n) m ks

data Coord = Coord { x :: Integer, y :: Integer } deriving (Eq, Ord, Show)

valueCoord :: Coord -> Integer
valueCoord c = y c * 100 + x c

data Square = Wall | LeftBox | RightBox | Robot | Empty deriving (Eq, Ord, Show)

data BoxCoord = BoxCoord { leftBox :: Coord, rightBox :: Coord } deriving (Eq, Ord, Show)

valueBox :: BoxCoord -> Integer
valueBox = valueCoord . leftBox

parseSquare :: Char -> (Square, Square)
parseSquare '#' = (Wall, Wall)
parseSquare 'O' = (LeftBox, RightBox)
parseSquare '@' = (Robot, Empty)
parseSquare '.' = (Empty, Empty)

data Board = Board { map :: Map Coord Square, robot :: Coord, boxes :: Set BoxCoord } deriving (Eq, Ord, Show)

parseBoard :: String -> Board
parseBoard s = Board m r b where
	m = M.fromListWith undefined $ do
		(y, l) <- zip [0..] $ lines s
		(x, c) <- zip [0,2..] l
		let (ls, rs) = parseSquare c
		[(Coord x y, ls), (Coord (x+1) y, rs)]
	r = fst $ M.findMin $ M.filter (== Robot) m
	b = S.map (\c -> BoxCoord c (move East c)) $ M.keysSet $ M.filter (==LeftBox) m

data Direction = North | East | South | West deriving (Eq, Ord, Show)

move :: Direction -> Coord -> Coord
move North c = Coord (x c) (y c - 1)
move East c = Coord (x c + 1) (y c)
move South c = Coord (x c) (y c + 1)
move West c = Coord (x c - 1) (y c)

moveBox :: Direction -> BoxCoord -> BoxCoord
moveBox d c = BoxCoord (move d $ leftBox c) (move d $ rightBox c)

parseMovement :: Char -> Direction
parseMovement '^' = North
parseMovement '>' = East
parseMovement 'v' = South
parseMovement '<' = West

parseDirections :: String -> [Direction]
parseDirections = fmap parseMovement . filter (not . isSpace)

getInput :: IO (Board, [Direction])
getInput = do
	(boards:dirs:_) <- splitOn "\n\n" <$> readFile "input"
	pure (parseBoard boards, parseDirections dirs)

boxChain :: Direction -> Coord -> Board -> State (Set Coord) (Maybe (Set BoxCoord))
boxChain d c b = case map b ! c of
	Wall -> pure Nothing
	LeftBox -> do
		seen <- gets $ member c
		if seen then pure $ Just S.empty else do
			let r = move East c
			modify $ S.insert c
			modify $ S.insert r
			if (move d c) == r then do
				x <- boxChain d (move d r) b
				pure $ S.insert (BoxCoord c r) <$> x
			else do
				x <- boxChain d (move d c) b
				y <- boxChain d (move d r) b
				pure $ (\a b -> S.insert (BoxCoord c r) $ a <> b) <$> x <*> y
	RightBox -> do
		seen <- gets $ member c
		if seen then pure $ Just S.empty else do
			let l = move West c
			modify $ S.insert c
			modify $ S.insert l
			if move d c == l then do
				x <- boxChain d (move d l) b
				pure $ S.insert (BoxCoord l c) <$> x
			else do
				x <- boxChain d (move d l) b
				y <- boxChain d (move d c) b
				pure $ (\a b -> S.insert (BoxCoord l c) $ a <> b) <$> x <*> y
	Empty -> pure $ Just S.empty
	Robot -> error "Two robots?!"

moveRobot :: Direction -> Board -> Board
moveRobot d b = case map b ! next of
	Empty -> Board (M.insert (robot b) Empty $ M.insert next Robot $ map b) next (boxes b)
	LeftBox -> case evalState (boxChain d next b) S.empty of
		Nothing -> b
		Just moveBoxes -> let
			nonmoveBoxes = boxes b `S.difference` moveBoxes
			movedBoxes = S.map (moveBox d) moveBoxes
			withoutBoxes = massInsert (S.map leftBox moveBoxes <> S.map rightBox moveBoxes) Empty (map b)
			withMovedBoxes = massInsert (S.map leftBox movedBoxes) LeftBox $ massInsert (S.map rightBox movedBoxes) RightBox $ withoutBoxes
			in Board
				(M.insert (robot b) Empty $ M.insert next Robot $ withMovedBoxes)
				next
				(nonmoveBoxes <> movedBoxes)
	RightBox -> case evalState (boxChain d next b) S.empty of
		Nothing -> b
		Just moveBoxes -> let
			nonmoveBoxes = boxes b `S.difference` moveBoxes
			movedBoxes = S.map (moveBox d) moveBoxes
			withoutBoxes = massInsert (S.map leftBox moveBoxes <> S.map rightBox moveBoxes) Empty (map b)
			withMovedBoxes = massInsert (S.map leftBox movedBoxes) LeftBox $ massInsert (S.map rightBox movedBoxes) RightBox $ withoutBoxes
			in Board
				(M.insert (robot b) Empty $ M.insert next Robot $ withMovedBoxes)
				next
				(nonmoveBoxes <> movedBoxes)
	Wall -> b
	Robot -> error "Two robots?!"
	where
		next = move d $ robot b

valueBoard :: Board -> Integer
valueBoard b = sum $ fmap valueBox $ S.toList $ boxes b

main :: IO ()
main = do
	(startBoard, directions) <- getInput
	print $ valueBoard $ foldl (flip moveRobot) startBoard directions
