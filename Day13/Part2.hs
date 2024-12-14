import Data.List.Split (splitOn)
import Data.Semigroup (stimes)
import Data.Ratio (numerator, denominator)

data Vector t = Vector { x :: t, y :: t } deriving (Eq, Ord, Show)

instance Functor Vector where
	fmap f v = Vector (f $ x v) (f $ y v)

instance Applicative Vector where
	f <*> v = Vector (x f $ x v) (y f $ y v)

scaleVector :: Num t => t -> Vector t -> Vector t
scaleVector s = fmap (s*)

dotProduct :: Num t => Vector t -> Vector t -> t
dotProduct v w = (\u -> x u + y u) $ (*) <$> v <*> w

instance Num t => Semigroup (Vector t) where
	v <> w = (+) <$> v <*> w
	stimes n = scaleVector (fromInteger $ toInteger $ n)

instance Num t => Monoid (Vector t) where
	mempty = Vector 0 0

e1 :: Num t => Vector t
e1 = Vector 1 0

e2 :: Num t => Vector t
e2 = Vector 0 1

data Matrix t = Matrix { e1to :: Vector t, e2to :: Vector t} deriving (Eq, Ord, Show)

instance Functor Matrix where
	fmap f m = Matrix (f <$> e1to m) (f <$> e2to m)

applyMatrix :: Num t => Matrix t -> Vector t -> Vector t
applyMatrix m v = scaleVector (x v) (e1to m) <> scaleVector (y v) (e2to m)

instance Num t => Num (Matrix t) where
	m + n = Matrix (e1to m <> e1to n) (e2to m <> e2to n)
	negate = fmap (negate 1 *)
	m * n = Matrix (applyMatrix m $ e1to n) (applyMatrix m $ e2to n)
	abs = id
	signum = id
	fromInteger n = (fromInteger n *) <$> Matrix e1 e2

determinant :: Num t => Matrix t -> t
determinant (Matrix (Vector a c) (Vector b d)) = a * d - b * c

invertMatrix :: (Fractional t, Eq t) => Matrix t -> Maybe (Matrix t)
invertMatrix m | determinant m == 0 = Nothing
invertMatrix m@(Matrix (Vector a c) (Vector b d)) = Just $
	(/ determinant m) <$> Matrix (Vector d (negate c)) (Vector (negate b) a)

data Problem = Problem { buttons :: Matrix Integer, goal :: Vector Integer } deriving (Eq, Ord, Show)

parseProblem :: String -> Problem
parseProblem s = Problem (Matrix buttona buttonb) goalp where
	(aline:bline:gline:_) = lines s
	buttona = parseButton aline
	buttonb = parseButton bline
	(_:xword:yword:_) = words gline
	xn = read $ drop 2 $ init $ xword
	yn = read $ drop 2 $ yword
	goalp = Vector (10000000000000 + xn) (10000000000000 + yn)
	parseButton cline = let
		(_:_:xword:yword:_) = words cline
		xn = read $ drop 2 $ init xword
		yn = read $ drop 2 $ yword
		in Vector xn yn

costVector :: Vector Integer
costVector = Vector 3 1

isPositiveInt :: Rational -> Maybe Integer
isPositiveInt r
	| denominator r == 1 && numerator r >= 0 = Just $ numerator r
	| otherwise = Nothing

isPositiveIntV :: Vector Rational -> Maybe (Vector Integer)
isPositiveIntV (Vector a b) = Vector <$> isPositiveInt a <*> isPositiveInt b

solveProblem :: Problem -> Integer
solveProblem problem = case invertMatrix $ fromInteger <$> buttons problem of
	Just inv -> case isPositiveIntV $ inv `applyMatrix` fmap fromInteger (goal problem) of
		Just v -> dotProduct costVector v
		Nothing -> 0
	-- The two buttons are parallel vectors
	Nothing -> error "parallel?"

getInput :: IO [Problem]
getInput = fmap parseProblem . splitOn "\n\n" <$> readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap solveProblem