import Data.Map (Map)
import qualified Data.Map as M 
import Data.Set (Set)
import qualified Data.Set as S 
import Data.Char (isDigit)

data Content
	= Empty
	| Symbol Char 
	| Digit Int
	deriving (Show, Eq)

parseContent :: Char -> Content 
parseContent '.' = Empty 
parseContent c
	| isDigit c = Digit $ read [c]
	| otherwise = Symbol c

data Coordinate = Coordinate 
	{ getX :: Integer
	, getY :: Integer
	} deriving (Eq, Ord, Show)

neighbours :: Coordinate -> Set Coordinate 
neighbours (Coordinate x y) = S.fromList $ do 
	dx <- [-1..1]
	dy <- [-1..1]
	pure $ Coordinate (x + dx) (y + dy)

parseGrid :: String -> Map Coordinate Content 
parseGrid s = M.fromListWith undefined $ do
	(y, l) <- zip [1..] $ lines s 
	(x, c) <- zip [1..] l 
	pure (Coordinate x y, parseContent c)

adjacentToSymbols :: Map Coordinate Content -> Set Coordinate 
adjacentToSymbols = M.foldMapWithKey $ \k v -> case v of
	Symbol _ -> neighbours k 
	_ -> mempty

data ScanState = ScanState 
	{ currentNumber :: Integer 
	, currentAdjacentToSymbol :: Bool
	, total :: Integer
	}

startState :: ScanState 
startState = ScanState 0 False 0 

scanGrid :: Map Coordinate Content -> Set Coordinate -> Integer 
scanGrid grid adjacents = total $ foldl procState startState coordList where
	coordList = flip Coordinate <$> [minY .. maxY] <*> [minX .. maxX]
	(Coordinate minX minY, _) = M.findMin grid
	(Coordinate maxX maxY, _) = M.findMax grid
	procState s c = case grid M.! c of 
		Digit d -> ScanState 
			(currentNumber s * 10 + toInteger d) 
			(currentAdjacentToSymbol s || c `elem` adjacents)
			(total s)
		_ -> ScanState 
			0
			False 
			(total s + if currentAdjacentToSymbol s then currentNumber s else 0)

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . (scanGrid <*> adjacentToSymbols) . parseGrid

