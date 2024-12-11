blink :: Integer -> [Integer]
blink 0 = [1]
blink n | even (length shown) = [read $ take half shown, read $ drop half shown]
	where
		shown = show n
		half = (length shown) `div` 2
blink n = [2024 * n]

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . length . (!!25) . iterate (>>= blink) . fmap read . words