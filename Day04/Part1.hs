-- import Data.List (tails, transpose)

import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as M
import Control.Monad (guard)

getInput :: IO String
getInput = readFile "input"

-- first attempt, didnt work for some reason

-- startsWithXmas :: String -> Bool
-- startsWithXmas ('X':'M':'A':'S':_) = True
-- startsWithXmas _ = False

-- diagonal :: [[a]] -> [a]
-- diagonal [] = []
-- diagonal [x:xs] = [x]
-- diagonal ((x:xs):ys) = x : diagonal (drop 1 <$> ys)

-- search :: [String] -> Int
-- search s = length $ filter startsWithXmas $
-- 	(s >>= tails) <>
-- 	(transpose s >>= tails) <>
-- 	(s >>= tails . reverse) <>
-- 	(transpose s >>= tails . reverse) <>
-- 	(transpose (diagonal $ tails <$> s) >>= tails) <>
-- 	(transpose (diagonal $ tails <$> s) >>= tails . reverse) <>
-- 	(transpose (diagonal $ tails <$> reverse s) >>= tails) <>
-- 	(transpose (diagonal $ tails <$> reverse s) >>= tails . reverse)

data Coord = Coord {x :: Int, y :: Int} deriving (Eq, Ord, Show)

type Grid = Map Coord Char

loadIntoGrid :: String -> Grid
loadIntoGrid s = M.fromListWith undefined $ do
	(x, l) <- zip [1..] $ lines s
	(y, c) <- zip [1..] l
	pure (Coord x y, c)

countXmas :: Coord -> Grid -> Int
countXmas (Coord x y) m = length $ do
	dx <- [-1,0,1]
	dy <- [-1,0,1]
	guard $ (m !? Coord x y) == Just 'X'
	guard $ (m !? Coord (x + dx) (y + dy)) == Just 'M'
	guard $ (m !? Coord (x + 2 * dx) (y + 2 * dy)) == Just 'A'
	guard $ (m !? Coord (x + 3 * dx) (y + 3 * dy)) == Just 'S'
	pure ()

main :: IO ()
main = do
	grid <- loadIntoGrid <$> getInput
	print $ M.foldlWithKey (\a k v -> a + countXmas k grid) 0 grid