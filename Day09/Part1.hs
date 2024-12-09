import Data.Foldable (toList)
import Data.Char (isSpace)

type ID = Integer

parseInput :: String -> [Maybe ID]
parseInput s = go s 0 where
	go [] _ = []
	go [c] id = replicate (read $ pure c) $ Just id
	go (c:d:es) id = let
		taken = replicate (read $ pure c) $ Just id
		free = replicate (read $ pure d) Nothing
		rest = go es (id + 1)
		in taken <> free <> rest

condense :: [Maybe ID] -> [ID]
condense l = go num l (reverse onlyids) where
	onlyids = l >>= toList
	num = length onlyids
	go 0 _ _ = []
	go n (Just x:xs) ys = x : go (n-1) xs ys
	go n (Nothing:xs) (y:ys) = y : go (n-1) xs ys

computeCheck :: [ID] -> Integer
computeCheck = sum . zipWith (*) [0..]

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . computeCheck . condense . parseInput . filter (not . isSpace)