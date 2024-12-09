import Data.Foldable (toList)
import Data.Char (isSpace)

type ID = Integer
type Length = Integer

parseInput :: String -> [(Maybe ID, Length)]
parseInput s = go s 0 where
	go [] _ = []
	go [c] id = [(Just id, read $ pure c)]
	go (c:d:es) id = let
		taken = (Just id, read $ pure c)
		free = (Nothing, read $ pure d)
		rest = go es (id + 1)
		in taken : free : rest

condense :: [(Maybe ID, Length)] -> [(Maybe ID, Length)]
condense l = go l $ reverse l where
	go curdisk ((Nothing, _):xs) = go curdisk xs
	go curdisk ((Just id, len):xs) =
		case break (\(x,y) -> x == Nothing && y >= len) curdisk of
			(s, e) | any (\(x,y) -> x == Just id) s -> go curdisk xs
			(_, []) -> go curdisk xs
			(oldStart, (Nothing, oldFreeLen):oldEnd) -> let
				newFreeLen = oldFreeLen - len
				newEnd = (\(x,y) -> if x == Just id then (Nothing, y) else (x,y)) <$> oldEnd
				newdisk = oldStart <> [(Just id, len), (Nothing, newFreeLen)] <> newEnd
				in go newdisk xs
	go curdisk [] = curdisk

computeCheck :: [(Maybe ID, Length)] -> Integer
computeCheck = go 0 where
	go _ [] = 0
	go n ((Nothing, len):xs) = go (n+len) xs
	go n ((Just id, len):xs) = id * sum [n..n+len-1] + go (n+len) xs

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . computeCheck . condense . parseInput . filter (not . isSpace)