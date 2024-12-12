import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as St
import Data.Foldable (traverse_)

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

neighbours :: Coord -> [Coord]
neighbours c =
	[ Coord (x c) (y c - 1)
	, Coord (x c + 1) (y c)
	, Coord (x c) (y c + 1)
	, Coord (x c - 1) (y c)
	]

type Board = Map Coord Char

parseBoard :: String -> Board
parseBoard s = M.fromListWith undefined $ do
	(x, l) <- zip [1..] $ lines s
	(y, c) <- zip [1..] l
	pure (Coord x y, c)

floodFill :: Board -> Coord -> Set Coord
floodFill board c = St.execState (floodFillS board c) Set.empty

floodFillS :: Board -> Coord -> State (Set Coord) ()
floodFillS board c = do
	let h = board M.!? c
	s <- St.get
	if c `Set.member` s then
		pure ()
	else do
		St.modify $ Set.insert c
		traverse_ (floodFillS board) $ filter ((== h) . (board M.!?)) $ neighbours c

valueRegion :: Set Coord -> Integer
valueRegion s = perimeter * area where
	area = toInteger $ length s
	perimeter = toInteger $
		length [() | a <- Set.toList s, b <- neighbours a, not $ b `Set.member` s]

valueRegionsS :: Board -> Coord -> State (Set Coord) Integer
valueRegionsS board c = do
	s <- St.get
	if c `Set.member` s then
		pure 0
	else do
		let r = floodFill board c
		St.modify (<> r)
		pure $ valueRegion r

valueRegions :: Board -> Integer
valueRegions board =
	sum $
	flip St.evalState Set.empty $
	traverse (valueRegionsS board) $
	Set.toList $
	M.keysSet board

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . valueRegions . parseBoard