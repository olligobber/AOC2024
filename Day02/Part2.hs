getInput :: IO String
getInput = readFile "input"

d :: [Int] -> [Int]
d x = zipWith (-) x (tail x)

safe :: [Int] -> Bool
safe x = (increasing || decreasing) && inRange where
	dx = d x
	increasing = all (> 0) dx
	decreasing = all (< 0) dx
	inRange = all ((< 4) . abs) dx

waysToRemove :: [a] -> [[a]]
waysToRemove [] = []
waysToRemove (x:xs) = xs : ((x:) <$> waysToRemove xs)

safeWithDamper :: [Int] -> Bool
safeWithDamper x = safe x || any safe (waysToRemove x)

main :: IO ()
main = getInput >>= (print . length . filter safeWithDamper . fmap (fmap read . words) . lines)