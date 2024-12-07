import Control.Monad (replicateM)

getInput :: IO String
getInput = readFile "input"

data Operator = Plus | Times deriving (Eq, Ord, Show)

allOps :: [Operator]
allOps = [Plus, Times]

parseProblem :: String -> (Integer, [Integer])
parseProblem s = (read $ init $ head $ words s, read <$> tail (words s))

eval :: [Integer] -> [Operator] -> Integer
eval (a:b:cs) (Plus:os) = eval (a+b : cs) os
eval (a:b:cs) (Times:os) = eval (a*b : cs) os
eval [a] [] = a
eval _ _ = error "Wrong amount"

solveProblem :: (Integer, [Integer]) -> Bool
solveProblem (goal, ins) = any id $ do
	ops <- replicateM (length ins - 1) allOps
	pure $ eval ins ops == goal

main :: IO ()
main = getInput >>= print . sum . fmap fst . filter solveProblem . fmap parseProblem . lines