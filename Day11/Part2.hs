import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as S

type Cache = Map (Int, Stone) Integer

emptyCache :: Cache
emptyCache = M.empty

type Cached x = State Cache x

runCached :: Cached x -> x
runCached s = S.evalState s emptyCache

type Stone = Integer

trimZeros :: String -> String
trimZeros s = dropWhile (== '0') (init s) <> [last s]

blink :: Integer -> [Integer]
blink 0 = [1]
blink n | even (length shown) = [read $ take half shown, read $ drop half shown]
	where
		shown = show n
		half = (length shown) `div` 2
blink n = [2024 * n]

cachedLength :: Int -> Stone -> Cached Integer
cachedLength 0 _ = pure 1
cachedLength n x = do
	fromCache <- S.gets (M.!? (n,x))
	case fromCache of
		Nothing -> do
			let ys = blink x
			y <- sum <$> traverse (cachedLength $ n-1) ys
			S.modify $ M.insert (n, x) y
			pure y
		Just y -> pure y

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . sum . runCached . traverse (cachedLength 75) . fmap read . words