import Data.List.Split (splitOn)
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.Set (Set, member)
import qualified Data.Set as S

data Position = Position { x :: Integer, y :: Integer } deriving (Eq, Ord, Show)

startPosition :: Position
startPosition = Position 0 0

data Button = Button { incx :: Integer, incy :: Integer } deriving (Eq, Ord, Show)

pressButton :: Button -> Position -> Position
pressButton b p = Position (x p + incx b) (y p + incy b)

pressTimes :: Integer -> Button -> Position -> Position
pressTimes n b p = Position (x p + n * incx b) (y p + n * incy b)

parseButton :: String -> Button
parseButton s = Button xn yn where
	(_:_:xword:yword:_) = words s
	xn = read $ drop 2 $ init xword
	yn = read $ drop 2 $ yword

data Problem = Problem { a :: Button, b :: Button, goal :: Position } deriving (Eq, Ord, Show)

-- Under-estimate the cost to get to the goal from a position
estimateProblem :: Position -> Problem -> Integer
estimateProblem pos problem = min esta estb where
	esta = 3 * max (est a x incx) (est a y incy)
	estb = max (est b x incx) (est b y incy)
	est c z incz = (z (goal problem) - z pos) `div` incz (c problem)

parseProblem :: String -> Problem
parseProblem s = Problem buttona buttonb goalp where
	(aline:bline:gline:_) = lines s
	buttona = parseButton aline
	buttonb = parseButton bline
	(_:xword:yword:_) = words gline
	xn = read $ drop 2 $ init $ xword
	yn = read $ drop 2 $ yword
	goalp = Position (10000000000000 + xn) (10000000000000 + yn)

type MyPQ = PQueue Integer (Integer, Position)

solveProblemFrom :: Problem -> Set Position -> MyPQ -> Maybe Integer
solveProblemFrom problem tried reached = case PQ.minView reached of
	Nothing -> Nothing
	Just ((c, p), rest) ->
		if p `member` tried then
			solveProblemFrom problem tried rest
		else if x p > x (goal problem) || y p > y (goal problem) then
			solveProblemFrom problem tried rest
		else if p == goal problem then
			Just c
		else
			let
				apos = pressButton (a problem) p
				bpos = pressButton (b problem) p
				apress = PQ.insert (c+3+estimateProblem apos problem) (c+3, apos)
				bpress = PQ.insert (c+1+estimateProblem bpos problem) (c+1, bpos)
			in solveProblemFrom problem (tried <> S.singleton p) (apress $ bpress $ rest)

startPQ :: Problem -> PQueue Integer (Integer, Position)
startPQ problem = PQ.singleton (estimateProblem startPosition problem) (0, startPosition)

solveProblem :: Problem -> Integer
solveProblem problem = case solveProblemFrom problem S.empty $ startPQ problem of
	Nothing -> 0
	Just x -> x

getInput :: IO [Problem]
getInput = fmap parseProblem . splitOn "\n\n" <$> readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap solveProblem