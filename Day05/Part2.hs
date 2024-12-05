import Data.List.Split (splitOn)

getInput :: IO String
getInput = readFile "input"

newtype V = V {fromV :: Int} deriving (Eq, Ord, Show)

parseEdges :: String -> [(V, V)]
parseEdges s = do
	l <- lines s
	let [a,b] = splitOn "|" l
	pure (V $ read a, V $ read b)

parseRequests :: String -> [[V]]
parseRequests s = do
	l <- lines s
	let r = splitOn "," l
	pure $ V . read <$> r

parseAll :: String -> ([(V,V)], [[V]])
parseAll s = (parseEdges e, parseRequests r) where
	[e,r] = splitOn "\n\n" s

isInOrder :: [V] -> [(V,V)] -> Bool
isInOrder [] _ = True
isInOrder (v:vs) es = all (\u -> not $ (u,v) `elem` es) vs && isInOrder vs es

middle :: [a] -> a
middle x = x !! (length x `div` 2)

orderRequest :: [V] -> [(V,V)] -> [V]
orderRequest vertices edges = go start where
	filteredEdges = filter (\(v,u) -> v `elem` vertices && u `elem` vertices) edges
	start = [(v, [u | (u,w) <- filteredEdges, w == v]) | v <- vertices ]
	go [] = []
	go l = let v = head [u | (u,w) <- l, w == []] in v : go (remove v l)
	remove v l = [(u, filter (/= v) ws) | (u, ws) <- l, u /= v]

main :: IO ()
main = do
	(es, rs) <-parseAll <$> getInput
	print $ sum $ fmap (fromV . middle . flip orderRequest es) $ filter (not . flip isInOrder es) $ rs