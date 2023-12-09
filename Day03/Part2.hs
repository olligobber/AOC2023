import Data.Map (Map)
import qualified Data.Map as M 
import Data.Set (Set)
import qualified Data.Set as S 
import Data.Char (isDigit)

newtype AccuMap k v = AccuMap { getMap :: Map k v } deriving (Eq, Ord, Show)

instance (Ord k, Semigroup v) => Semigroup (AccuMap k v) where
	AccuMap m <> AccuMap n = AccuMap $ M.unionWith (<>) m n

instance (Ord k, Semigroup v) => Monoid (AccuMap k v) where
	mempty = AccuMap mempty

data Content
	= Empty
	| Gear
	| Symbol Char 
	| Digit Int
	deriving (Show, Eq)

parseContent :: Char -> Content 
parseContent '.' = Empty 
parseContent '*' = Gear
parseContent c
	| isDigit c = Digit $ read [c]
	| otherwise = Symbol c

data Coordinate = Coordinate 
	{ getX :: Integer
	, getY :: Integer
	} deriving (Eq, Ord, Show)

neighbours :: Coordinate -> [Coordinate] 
neighbours (Coordinate x y) = do 
	dx <- [-1..1]
	dy <- [-1..1]
	pure $ Coordinate (x + dx) (y + dy)

parseGrid :: String -> Map Coordinate Content 
parseGrid s = M.fromListWith undefined $ do
	(y, l) <- zip [1..] $ lines s 
	(x, c) <- zip [1..] l 
	pure (Coordinate x y, parseContent c)

-- Given a grid, make a map that says for each coordinate which gears are adjacent
adjacentToGears :: Map Coordinate Content -> AccuMap Coordinate (Set Coordinate) 
adjacentToGears = M.foldMapWithKey $ \k v -> case v of
	Gear -> AccuMap $ M.fromListWith undefined $ (,) <$> neighbours k <*> [S.singleton k] 
	_ -> mempty

data ScanState = ScanState 
	{ currentNumber :: Integer 
	, currentAdjacentGears :: Set Coordinate
	, gearNeighbours :: AccuMap Coordinate [Integer]
	}

startState :: ScanState 
startState = ScanState 0 mempty mempty 

scanGrid :: Map Coordinate Content -> AccuMap Coordinate (Set Coordinate) -> AccuMap Coordinate [Integer] 
scanGrid grid (AccuMap adjacents) = gearNeighbours $ foldl procState startState coordList where
	coordList = flip Coordinate <$> [minY .. maxY] <*> [minX .. maxX]
	(Coordinate minX minY, _) = M.findMin grid
	(Coordinate maxX maxY, _) = M.findMax grid
	procState s c = case grid M.! c of 
		Digit d -> ScanState 
			(currentNumber s * 10 + toInteger d) 
			(currentAdjacentGears s <> M.findWithDefault mempty c adjacents)
			(gearNeighbours s)
		_ -> ScanState 
			0
			mempty 
			(gearNeighbours s <> AccuMap (M.fromSet (const [currentNumber s]) (currentAdjacentGears s)))

totalGears :: AccuMap Coordinate [Integer] -> Integer 
totalGears = sum . foldMap totalGear . getMap where
	totalGear [a,b] = [a*b]
	totalGear _ = [] 

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . totalGears . (scanGrid <*> adjacentToGears) . parseGrid

