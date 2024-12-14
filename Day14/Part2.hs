import Data.List.Split (splitOn)
import Data.List (sort, group)
import Data.Set (Set, member)
import qualified Data.Set as S

data Vert = Vert Int deriving (Eq, Ord, Show)

vert :: Int -> Vert
vert n = Vert $ n `mod` 103

allVert :: [Vert]
allVert = Vert <$> [0..102]

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

allHor :: [Hor]
allHor = Hor <$> [0..100]

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

moveRobot :: Robot -> Robot
moveRobot robot = Robot
	(move (velocity robot) (startPos robot))
	(velocity robot)

showRobots :: Set Position -> IO ()
showRobots s = putStrLn $ unlines $ do
	y <- allVert
	pure $ do
		x <- allHor
		pure $ if Position x y `member` s then '#' else ' '

symmetric :: Set Position -> Bool
symmetric _ = True
-- symmetric s = S.map (\(Position x y) -> Position (maxX - x - minX) y) s == s
-- 	where
-- 		maxX = S.findMax $ S.map px $ s
-- 		minX = S.findMin $ S.map px $ s

-- These were obtained by noticing the robots horizontally and vertically lined up now and then.
-- It makes sense the 101 and 103 are involved, the robots will act periodically with those periods.
-- Look at indexes 7 and 108, and 53 and 156, to see what I saw.
susIndex :: Integer -> Bool
susIndex i = i `mod` 101 == 7 && i `mod` 103 == 53

getInput :: IO [Robot]
getInput = fmap parseRobot . lines <$> readFile "input"

main :: IO ()
main = do
	putStrLn "Running..."
	robs <- getInput
	mapM_ (\(i,r) ->
		let s = foldMap (S.singleton . startPos) r in
			if susIndex i then
				showRobots s *> print i *> getLine *> pure ()
			else
				pure ()
		) $ zip [0..] $ iterate (fmap moveRobot) robs