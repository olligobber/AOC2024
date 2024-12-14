import Data.List.Split (splitOn)
import Data.List (sort, group)

data Vert = Vert Int deriving (Eq, Ord, Show)

vert :: Int -> Vert
vert n = Vert $ n `mod` 103

instance Num Vert where
	Vert a + Vert b = vert $ a + b
	Vert a - Vert b = vert $ a - b
	Vert a * Vert b = vert $ a * b
	abs = id
	signum = id
	fromInteger = vert . fromInteger

data Hor = Hor Int deriving (Eq, Ord, Show)

hor :: Int -> Hor
hor n = Hor $ n `mod` 101

instance Num Hor where
	Hor a + Hor b = hor $ a + b
	Hor a - Hor b = hor $ a - b
	Hor a * Hor b = hor $ a * b
	abs = id
	signum = id
	fromInteger = hor . fromInteger

data Position = Position { px :: Hor, py :: Vert } deriving (Eq, Ord, Show)

data Vector = Vector { vx :: Hor, vy :: Vert } deriving (Eq, Ord, Show)

move :: Vector -> Position -> Position
move v p = Position (px p + vx v) (py p + vy v)

data Robot = Robot { startPos :: Position, velocity :: Vector } deriving (Eq, Ord, Show)

parseRobot :: String -> Robot
parseRobot s = Robot (Position px py) (Vector vx vy) where
	(pword:vword:_) = words s
	(pxs:pys:_) = splitOn "," $ drop 2 pword
	(vxs:vys:_) = splitOn "," $ drop 2 vword
	px = hor $ read pxs
	py = vert $ read pys
	vx = hor $ read vxs
	vy = vert $ read vys

moveRobot :: Robot -> Position
moveRobot robot = iterate (move (velocity robot)) (startPos robot) !! 100

quadrant :: Position -> Maybe (Bool, Bool)
quadrant (Position (Hor x) (Vert y))
	| x == 50 = Nothing
	| y == 51 = Nothing
	| otherwise = Just (x < 50, y < 51)

getInput :: IO [Robot]
getInput = fmap parseRobot . lines <$> readFile "input"

main :: IO ()
main = getInput >>= print . product . fmap length . group . sort . filter (/= Nothing) . fmap (quadrant . moveRobot)