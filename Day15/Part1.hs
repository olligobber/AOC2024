import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Prelude hiding (map)

data Coord = Coord { x :: Integer, y :: Integer } deriving (Eq, Ord, Show)

valueCoord :: Coord -> Integer
valueCoord c = y c * 100 + x c

data Square = Wall | Box | Robot | Empty deriving (Eq, Ord, Show)

parseSquare :: Char -> Square
parseSquare '#' = Wall
parseSquare 'O' = Box
parseSquare '@' = Robot
parseSquare '.' = Empty

data Board = Board { map :: Map Coord Square, robot :: Coord }

parseBoard :: String -> Board
parseBoard s = Board m r where
	m = M.fromListWith undefined $ do
		(y, l) <- zip [0..] $ lines s
		(x, c) <- zip [0..] l
		pure (Coord x y, parseSquare c)
	r = fst $ M.findMin $ M.filter (== Robot) m

data Direction = North | East | South | West deriving (Eq, Ord, Show)

move :: Direction -> Coord -> Coord
move North c = Coord (x c) (y c - 1)
move East c = Coord (x c + 1) (y c)
move South c = Coord (x c) (y c + 1)
move West c = Coord (x c - 1) (y c)

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

boxChain :: Direction -> Coord -> Board -> Maybe Coord
boxChain d c b = case map b !? c of
	Just Wall -> Nothing
	Just Box -> boxChain d (move d c) b
	Just Empty -> Just c
	Just Robot -> error "Two robots?!"
	Nothing -> error "Map edge"

moveRobot :: Direction -> Board -> Board
moveRobot d b = case map b !? next of
	Just Empty -> Board (M.insert (robot b) Empty $ M.insert next Robot $ map b) next
	Just Box -> case boxChain d next b of
		Nothing -> b
		Just endofchain -> Board (M.insert (robot b) Empty $ M.insert next Robot $ M.insert endofchain Box $ map b) next
	Just Wall -> b
	Just Robot -> error "Two robots?!"
	Nothing -> error "Map edge"
	where
		next = move d $ robot b

valueBoard :: Board -> Integer
valueBoard b = sum $ fmap valueCoord $ M.keys $ M.filter (==Box) $ map b

main :: IO ()
main = do
	(startBoard, directions) <- getInput
	print $ valueBoard $ foldl (flip moveRobot) startBoard directions
