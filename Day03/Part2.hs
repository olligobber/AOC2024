import Data.Monoid (Sum(..))
import Data.List (tails)
import Data.List.Split (splitOn)

getInput :: IO String
getInput = readFile "input"

splitDosDonts :: String -> String
splitDosDonts s = head splitOnDonts <> concat removedStarts
	where
	splitOnDonts = splitOn "don't()" s
	splitOnDos = splitOn "do()" <$> tail splitOnDonts
	removedStarts = concat . tail <$> splitOnDos

isMul :: String -> Sum Int
isMul s
	| length s < 8 = mempty
	| take 4 s /= "mul(" = mempty
	| otherwise = case reads (drop 4 s) :: [(Int, String)] of
		[] -> mempty
		[(n, t)]
			| n > 999 -> mempty
			| n < 0 -> mempty
			| head t /= ',' -> mempty
			| otherwise -> case reads (tail t) :: [(Int, String)] of
				[] -> mempty
				[(m, q)]
					| m > 999 -> mempty
					| m < 0 -> mempty
					| head q /= ')' -> mempty
					| otherwise -> Sum $ m * n

main :: IO ()
main = getInput >>= print . getSum . foldMap isMul . tails . splitDosDonts