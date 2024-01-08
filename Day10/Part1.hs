import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Foldable (toList)

data Direction
	= North
	| East
	| South
	| West
	deriving (Eq, Ord, Show)

allDirs :: [Direction]
allDirs = 
	[ North
	, East
	, South
	, West
	]

oppDir :: Direction -> Direction 
oppDir North = South 
oppDir South = North
oppDir East = West
oppDir West = East 

data Tile
	= Ground
	| Pipe Direction Direction
	| Creature
	deriving (Eq, Ord, Show)

makePipe :: Direction -> Direction -> Tile 
makePipe x y | x /= y = Pipe (min x y) (max x y)

parseTile :: Char -> Tile
parseTile '.' = Ground 
parseTile '|' = makePipe North South 
parseTile '-' = makePipe East West 
parseTile 'F' = makePipe East South 
parseTile 'L' = makePipe North East 
parseTile 'J' = makePipe North West 
parseTile '7' = makePipe South West 
parseTile 'S' = Creature

data Coordinate = Coordinate { x :: Integer, y :: Integer } deriving (Eq, Ord, Show)

moveCoord :: Coordinate -> Direction -> Coordinate 
moveCoord c North = Coordinate (x c) (y c - 1)
moveCoord c East = Coordinate (x c + 1) (y c)
moveCoord c South = Coordinate (x c) (y c + 1)
moveCoord c West = Coordinate (x c - 1) (y c)

type MoveState = (Coordinate, Direction) -- A coordinate you are at and a direction you are about to move in 

type Field = Map Coordinate Tile 

parseField :: String -> Field 
parseField s = M.fromListWith undefined $ do 
	(row, line) <- zip [1..] $ lines s 
	(col, char) <- zip [1..] line 
	pure (Coordinate col row, parseTile char)

-- Left False means reached invalid tile, Left True means reached creature
move :: Field -> MoveState -> Either Bool MoveState 
move m (c, d) = case m M.!? moveCoord c d of 
	Nothing -> Left False 
	Just Ground -> Left False 
	Just Creature -> Left True 
	Just (Pipe e f) | oppDir e == d -> Right (moveCoord c d, f)
	Just (Pipe e f) | oppDir f == d -> Right (moveCoord c d, e)
	otherwise -> Left False 

getDistanceToCreature :: Field -> MoveState -> Maybe Integer 
getDistanceToCreature m s = case move m s of 
	Left False -> Nothing 
	Left True -> Just 1 
	Right t -> (+ 1) <$> getDistanceToCreature m t 

getLoopLength :: Field -> Integer 
getLoopLength m = maximum $ do 
	let [startCoord] = M.keys $ M.filter (== Creature) m 
	startDir <- allDirs 
	toList $ getDistanceToCreature m (startCoord, startDir)

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . flip div 2 . getLoopLength . parseField