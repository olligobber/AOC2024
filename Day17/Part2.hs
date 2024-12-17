import Control.Monad.State.Lazy (State, get, gets, modify, execState)
import Data.Map.Lazy (Map, (!), (!?))
import qualified Data.Map.Lazy as M
import Data.Bits (xor, (.&.))
import Data.List (tails)
import Control.Monad (guard)

data CompState = CompState
	{ registerA :: Integer
	, registerB :: Integer
	, registerC :: Integer
	, code :: Map Integer Int
	, pointer :: Integer
	, output :: [Int]
	} deriving (Eq, Ord, Show)

type Computation = State CompState ()

literal :: State CompState Integer
literal = gets $ \state -> toInteger $ code state ! (pointer state + 1)

combo :: State CompState Integer
combo = do
	lit <- literal
	case lit of
		0 -> pure 0
		1 -> pure 1
		2 -> pure 2
		3 -> pure 3
		4 -> gets registerA
		5 -> gets registerB
		6 -> gets registerC
		7 -> error "Reserved"

putA :: Integer -> Computation
putA n = modify $ \state -> state { registerA = n }

putB :: Integer -> Computation
putB n = modify $ \state -> state { registerB = n }

putC :: Integer -> Computation
putC n = modify $ \state -> state { registerC = n }

putPointer :: Integer -> Computation
putPointer n = modify $ \state -> state { pointer = n }

incPointer :: Computation
incPointer = modify $ \state -> state { pointer = pointer state + 2}

putOut :: Int -> Computation
putOut n = modify $ \state -> state { output = n : output state }

step :: Int -> Computation
-- adv
step 0 = do
	numerator <- gets registerA
	denominator <- (2^) <$> combo
	putA $ numerator `div` denominator
	incPointer
-- bxl
step 1 = do
	in1 <- gets registerB
	in2 <- literal
	putB $ in1 `xor` in2
	incPointer
-- bst
step 2 = do
	input <- combo
	putB $ input `mod` 8
	incPointer
-- jnz
step 3 = do
	jumpcond <- gets $ (/= 0) . registerA
	jumpdest <- literal
	if jumpcond then
		putPointer jumpdest
	else
		incPointer
-- bxc
step 4 = do
	in1 <- gets registerB
	in2 <- gets registerC
	putB $ in1 `xor` in2
	incPointer
-- out
step 5 = do
	out <- combo
	putOut $ fromInteger out `mod` 8
	incPointer
-- bdv
step 6 = do
	numerator <- gets registerA
	denominator <- (2^) <$> combo
	putB $ numerator `div` denominator
	incPointer
-- cdv
step 7 = do
	numerator <- gets registerA
	denominator <- (2^) <$> combo
	putC $ numerator `div` denominator
	incPointer

run :: Computation
run = do
	state <- get
	case code state !? pointer state of
		Just n -> do
			step n
			run
		Nothing -> pure ()

parseInput :: String -> (Integer -> CompState, [Int])
parseInput s = (\a -> CompState a b c d 0 [], instructions) where
	(_:lineb:linec:_:lined:_) = lines s
	wordb = last $ words lineb
	wordc = last $ words linec
	b = read wordb
	c = read wordc
	fulld = last $ words lined
	subd = (\char -> if char == ',' then ' ' else char) <$> fulld
	instructions = read <$> words subd
	d = M.fromListWith undefined $ zip [0..] instructions

findA :: (Integer -> CompState) -> [Int] -> [Integer]
findA f x = foldl go [0..7] $ drop 1 $ reverse $ tails x where
	go sofaropt goal = do
		addto <- [0..7]
		sofar <- sofaropt
		let newa = sofar * 8 + addto
		guard $ reverse (output $ execState run $ f newa) == goal
		pure newa

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = do
	(f, x) <- parseInput <$> getInput
	print $ minimum $ findA f x