import Data.List (sort)

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = do
	input <- getInput
	let list1 = sort $ (read . head . words) <$> lines input
	let list2 = sort $ (read . head . tail . words) <$> lines input
	print . sum . fmap abs $ zipWith (-) list1 list2