import Data.List.Split (splitOn)
import Control.Monad (guard)

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

parseProblem :: String -> Problem
parseProblem s = Problem buttona buttonb goalp where
	(aline:bline:gline:_) = lines s
	buttona = parseButton aline
	buttonb = parseButton bline
	(_:xword:yword:_) = words gline
	xn = read $ drop 2 $ init $ xword
	yn = read $ drop 2 $ yword
	goalp = Position xn yn

solveProblem :: Problem -> Integer
solveProblem problem = case results of
	[] -> 0
	x -> minimum x
	where
	results = do
		apress <- [0..100]
		bpress <- [0..100]
		let resultpos = pressTimes apress (a problem) $ pressTimes bpress (b problem) startPosition
		let cost = 3 * apress + bpress
		guard $ resultpos == goal problem
		pure cost

getInput :: IO [Problem]
getInput = fmap parseProblem . splitOn "\n\n" <$> readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap solveProblem