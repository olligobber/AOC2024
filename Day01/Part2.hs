import Data.List (sort)

getInput :: IO String
getInput = readFile "input"

runalgo :: [Int] -> [Int] -> Int
runalgo list1 list2 = sum $ do
	x <- list1
	y <- list2
	if x == y then pure x else []

main :: IO ()
main = do
	input <- getInput
	let list1 = (read . head . words) <$> lines input
	let list2 = (read . head . tail . words) <$> lines input
	print $ runalgo list1 list2