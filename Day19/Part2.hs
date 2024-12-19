import Control.Monad (guard)
import Data.List.Split (splitOn)
import Control.Monad.State.Lazy (State, get, modify, evalState)
import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as M

isPossible :: [String] -> String -> State (Map String Integer) Integer
isPossible _ [] = pure 1
isPossible xs y = do
	cache <- get
	case cache !? y of
		Just b -> pure b
		Nothing -> do
			result <- fmap sum $ flip traverse xs $ \x ->
				let n = length x in
				if length y < n then
					pure 0
				else if take n y /= x then
					pure 0
				else
					isPossible xs (drop n y)
			modify $ M.insert y result
			pure result

data Problem = Problem { available :: [String], goals :: [String] }

parseProblem :: String -> Problem
parseProblem s = Problem av go where
	(avstring:gostring:_) = splitOn "\n\n" s
	av = splitOn ", " avstring
	go = lines gostring

solveAll :: Problem -> State (Map String Integer) Integer
solveAll problem =
	fmap sum $ traverse (isPossible $ available problem) $ goals problem

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . flip evalState M.empty . solveAll . parseProblem