import Data.Set (Set, member)
import qualified Data.Set as S
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.List (inits)

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

neighbours :: Coord -> [Coord]
neighbours c =
	[ c { x = x c + 1 }
	, c { x = x c - 1 }
	, c { y = y c + 1 }
	, c { y = y c - 1 }
	]

inRange :: Coord -> Bool
inRange c = ordin x && ordin y where
	ordin z = z c >= 0 && z c <= 70

start :: Coord
start = Coord 0 0

end :: Coord
end = Coord 70 70

bfs :: Set Coord -> Bool
bfs blocked = go S.empty (PQ.singleton 0 start) where
	go visited pq = case PQ.minViewWithKey pq of
		Nothing -> False
		Just ((_, here), popped)
			| not $ inRange here -> go visited popped
			| here `member` blocked -> go visited popped
			| here `member` visited -> go visited popped
		Just ((dist, here), _)
			| here == end -> True
		Just ((dist, here), popped) -> go
			(S.insert here visited)
			(popped <> PQ.fromList ((,) (dist + 1) <$> neighbours here))

parseByte :: String -> Coord
parseByte s = Coord (read w1) (read w2) where
	subbed = (\x -> if x == ',' then ' ' else x) <$> s
	[w1, w2] = words subbed

getInput :: IO [Coord]
getInput = fmap parseByte . lines <$> readFile "input"

main :: IO ()
main = getInput >>= print . last . snd . head . dropWhile (bfs . S.fromList . snd) . drop 3000 . zip [0..] . inits