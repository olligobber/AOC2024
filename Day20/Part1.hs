import Data.Map.Lazy (Map, (!), (!?))
import qualified Data.Map.Lazy as M
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Set (Set, member)
import qualified Data.Set as S

data Queue x = Queue { inStack :: [x], outStack :: [x] }

push :: Queue x -> x -> Queue x
push q e = q { inStack = e : inStack q }

pop :: Queue x -> Maybe (x, Queue x)
pop (Queue [] []) = Nothing
pop (Queue is []) = pop (Queue [] $ reverse is)
pop (Queue is (o:os)) = Just (o, Queue is os)

singleton :: x -> Queue x
singleton e = Queue [e] []

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

neighbours :: Coord -> [Coord]
neighbours c =
	[ c { x = x c + 1 }
	, c { x = x c - 1 }
	, c { y = y c + 1 }
	, c { y = y c - 1 }
	]

data Tile = Start | End | Clear | Wall deriving (Eq, Ord, Show)

parseTile :: Char -> Tile
parseTile '#' = Wall
parseTile '.' = Clear
parseTile 'S' = Start
parseTile 'E' = End

type Course = Map Coord Tile

parseMap :: String -> Course
parseMap s = M.fromListWith undefined $ do
	(y, l) <- zip [1..] $ lines s
	(x, c) <- zip [1..] l
	pure (Coord x y, parseTile c)

getInput :: IO Course
getInput = parseMap <$> readFile "input"

pathfind :: Course -> Map Coord Int
pathfind course = go M.empty endqueue where
	go sofar q = case pop q of
		Nothing -> sofar
		Just ((here, _), popped)
			| here `M.member` sofar -> go sofar popped
			| course !? here == Nothing -> go sofar popped
			| course !? here == Just Wall -> go sofar popped
		Just ((here, cost), popped) -> go
			(M.insert here cost $ sofar)
			(foldl push popped $ (,) <$> neighbours here <*> pure (cost + 1))
	endqueue = singleton (end,0)
	end = fst $ M.findMin $ M.filter (== End) course

findShortcuts :: Course -> Map Coord Int -> Set (Coord, Coord)
findShortcuts course distances = singleShortcuts where
	singleShortcuts = S.fromList $ do
		wall <- walls
		intowall <- neighbours wall
		outofwall <- neighbours wall
		costintowall <- toList $ distances !? intowall
		costoutofwall <- toList $ distances !? outofwall
		let saved = costoutofwall - costintowall - 2
		guard $ saved >= 100
		pure (intowall, outofwall)
	-- doubleShortcuts = S.fromList $ do
	-- 	wall1 <- walls
	-- 	wall2 <- neighbours wall1
	-- 	guard $ course !? wall2 == Just Wall
	-- 	intowall <- neighbours wall1
	-- 	outofwall <- neighbours wall2
	-- 	costintowall <- toList $ distances !? intowall
	-- 	costoutofwall <- toList $ distances !? outofwall
	-- 	let saved = costoutofwall - costintowall - 3
	-- 	guard $ saved >= 100
	-- 	pure (intowall, outofwall)
	walls = M.keys $ M.filter (== Wall) course

main :: IO ()
main = do
	course <- getInput
	let distances = pathfind course
	print $ length $ findShortcuts course distances