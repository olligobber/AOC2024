import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as M
import Control.Monad (guard)

getInput :: IO String
getInput = readFile "input"

data Coord = Coord {x :: Int, y :: Int} deriving (Eq, Ord, Show)

type Grid = Map Coord Char

loadIntoGrid :: String -> Grid
loadIntoGrid s = M.fromListWith undefined $ do
	(x, l) <- zip [1..] $ lines s
	(y, c) <- zip [1..] l
	pure (Coord x y, c)

crossCoords :: Coord -> (Coord, Coord, Coord, Coord)
crossCoords (Coord x y) = (Coord (x+1) (y+1), Coord (x+1) (y-1), Coord (x-1) (y-1), Coord (x-1) (y+1))

countXmas :: Coord -> Grid -> Int
countXmas center m = length $ do
	guard $ (m !? center) == Just 'A'
	let (dr, ur, ul, dl) = crossCoords center
	guard $ (m !? dr) `elem` [Just 'M', Just 'S']
	guard $ (m !? ur) `elem` [Just 'M', Just 'S']
	guard $ (m !? dl) `elem` [Just 'M', Just 'S']
	guard $ (m !? ul) `elem` [Just 'M', Just 'S']
	guard $ (m !? dr) /= (m !? ul)
	guard $ (m !? dl) /= (m !? ur)
	[()]

main :: IO ()
main = do
	grid <- loadIntoGrid <$> getInput
	print $ M.foldlWithKey (\a k v -> a + countXmas k grid) 0 grid