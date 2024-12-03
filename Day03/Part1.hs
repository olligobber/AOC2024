import Data.Monoid (Sum(..))
import Data.List (tails)

getInput :: IO String
getInput = readFile "input"

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
main = getInput >>= print . getSum . foldMap isMul . tails