{-# LANGUAGE PatternSynonyms #-}

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
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

data TileWith x 
	= Ground x 
	| Pipe Direction Direction x x -- First is what the top left has, second is what rest has 
	deriving (Eq, Ord, Show)

instance Functor TileWith where 
	fmap f (Ground x) = Ground $ f x 
	fmap f (Pipe d e x y) = Pipe d e (f x) (f y)

type MapTile = TileWith Bool -- True is creature, false is empty ground

pattern Creature = Ground True 

makePipe :: Direction -> Direction -> x -> x -> TileWith x 
makePipe d e x y
	| d < e = Pipe d e x y 
	| d > e = Pipe e d x y 

parseTile :: Char -> MapTile
parseTile '.' = Ground False
parseTile '|' = makePipe North South False False
parseTile '-' = makePipe East West False False
parseTile 'F' = makePipe East South False False
parseTile 'L' = makePipe North East False False
parseTile 'J' = makePipe North West False False
parseTile '7' = makePipe South West False False
parseTile 'S' = Creature

data Coordinate = Coordinate { x :: Integer, y :: Integer } deriving (Eq, Ord, Show)

moveCoord :: Coordinate -> Direction -> Coordinate 
moveCoord c North = Coordinate (x c) (y c - 1)
moveCoord c East = Coordinate (x c + 1) (y c)
moveCoord c South = Coordinate (x c) (y c + 1)
moveCoord c West = Coordinate (x c - 1) (y c)

type MoveState = (Coordinate, Direction) -- A coordinate you are at and a direction you are about to move in 

type InputMap = Map Coordinate MapTile 

parseInputMap :: String -> InputMap 
parseInputMap s = M.fromListWith undefined $ do 
	(row, line) <- zip [1..] $ lines s 
	(col, char) <- zip [1..] line 
	pure (Coordinate col row, parseTile char)

-- Left False means reached invalid tile, Left True means reached creature
move :: InputMap -> MoveState -> Either Bool MoveState 
move m (c, d) = case m M.!? moveCoord c d of 
	Nothing -> Left False 
	Just (Ground False) -> Left False 
	Just Creature -> Left True 
	Just (Pipe e f _ _) | oppDir e == d -> Right (moveCoord c d, f)
	Just (Pipe e f _ _) | oppDir f == d -> Right (moveCoord c d, e)
	otherwise -> Left False 

getPathToCreature :: InputMap -> MoveState -> Maybe (Direction, Set Coordinate) 
getPathToCreature m s = case move m s of 
	Left False -> Nothing 
	Left True -> Just $ (oppDir $ snd s, S.singleton $ fst s)
	Right t -> (fmap $ S.insert $ fst s) <$> getPathToCreature m t 

getLoop :: InputMap -> (Map Coordinate (TileWith ()), Set Coordinate)
getLoop m = head $ do 
	let [startCoord] = M.keys $ M.filter (== Creature) m 
	startDir <- allDirs 
	let Just (finishDir, loopCoords) = getPathToCreature m (startCoord, startDir)
	pure (M.insert startCoord (makePipe startDir finishDir () ()) ((() <$) <$> m), loopCoords)

data Side = Inside | Outside deriving (Eq, Ord, Show)

-- type FloodFill = Map Coordinate (TileWith (Maybe Side)) 

-- TODO flood fill from a tile on the loop to work out which side is inside and which is outside
-- then count how many are inside

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = undefined