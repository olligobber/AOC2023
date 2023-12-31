{-# LANGUAGE ScopedTypeVariables #-}

import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.Monoid (Sum(Sum))
import Data.List.Split (splitOn)

data Ranges = Ranges { endpoints :: [Integer] } deriving (Eq, Ord, Show)

makeRanges :: [Integer] -> Ranges 
makeRanges = Ranges . filterRange where 
	filterRange :: [Integer] -> [Integer]
	filterRange [] = [] 
	filterRange (x:y:xs) | x == y = filterRange xs 
	filterRange (x:y:z:xs) | y == z = filterRange (x:xs)
	filterRange (x:y:xs) = x:y:filterRange xs 

emptyRange :: Ranges 
emptyRange = makeRanges [] 

unions :: [Ranges] -> Ranges 
unions rs = mergeRanges (/= Sum 0) $ (,,) (Sum 1) (Sum (-1)) <$> rs

union :: Ranges -> Ranges -> Ranges 
union x y = unions [x,y]

intersects :: [Ranges] -> Ranges 
intersects rs = mergeRanges (== Sum (length rs)) $ (,,) (Sum 1) (Sum (-1)) <$> rs 

intersect :: Ranges -> Ranges -> Ranges 
intersect x y = intersects [x, y]

minus :: Ranges -> Ranges -> Ranges 
x `minus` y = mergeRanges (== Sum 1) [(Sum 1, Sum (-1), x), (Sum 2, Sum (-2), y)]

-- Merge ranges by accumulating a value as you scan along, 
-- and the result is in the end ranges if the condition is true
mergeRanges :: forall a. Monoid a => (a -> Bool) -> [(a, a, Ranges)] -> Ranges
mergeRanges inRange ranges = scanList mergedLists where 
	lists :: [[(Integer, a)]]
	lists = do
		(x, y, r) <- ranges 
		pure $ zip (endpoints r) (cycle [x,y])
	mergedLists :: [(Integer, a)]
	mergedLists = mergeLists lists 
	scanList :: [(Integer, a)] -> Ranges 
	scanList list = makeRanges $ scanner False mempty list 
	scanner :: Bool -> a -> [(Integer, a)] -> [Integer]
	scanner _ _ [] = [] 
	scanner True _ [(k, _)] = [k] 
	scanner curIn curVal ((k, v) : xs) =
		if inRange (curVal <> v) /= curIn then 
			k : scanner (not curIn) (curVal <> v) xs 
		else 
			scanner curIn (curVal <> v) xs 

mergeLists :: forall k v. Ord k => [[(k, v)]] -> [(k,v)]
mergeLists lists = merger start where
	merger :: PQueue k [(k,v)] -> [(k,v)]
	merger pq = case PQ.minView pq of 
		Nothing -> [] 
		Just ([], rest) -> merger rest 
		Just ([x], rest) -> x : merger rest 
		Just (x:xs:xss, rest) -> x : merger (PQ.insert (fst xs) (xs:xss) rest) 
	start :: PQueue k [(k,v)]
	start = PQ.fromList $ do
		x:xs <- lists 
		pure $ (fst x, x:xs)

offsetRanges :: Ranges -> Integer -> Ranges 
offsetRanges r i = makeRanges $ (+i) <$> endpoints r  

data RangeMap = RangeMap
	{ start :: Integer 
	, end :: Integer 
	, offset :: Integer 
	}

data RangeMaps = RangeMaps { maps :: [RangeMap] }

applyMaps :: Ranges -> RangeMaps -> Ranges 
applyMaps x f = uncurry union $ foldl applyMap (x, emptyRange) (maps f) where
	applyMap :: (Ranges, Ranges) -> RangeMap -> (Ranges, Ranges)
	applyMap (unmapped, mapped) r = 
		let 
			mapRange = makeRanges [start r, end r] 
		in 
			( unmapped `minus` mapRange
			, mapped `union` (offsetRanges (unmapped `intersect` mapRange) (offset r))
			)

parseInput :: String -> (Ranges, [RangeMaps])
parseInput s = (seeds, rmaps) where 
	(seedSection:mapSection) = splitOn "\n\n" s 
	seedStrings = tail $ words seedSection
	seeds = parseSeeds seedStrings
	rmaps = RangeMaps . fmap parseMap . tail . lines <$> mapSection 

parseSeeds :: [String] -> Ranges 
parseSeeds s = unions $ processInts $ fmap read s where 
	processInts [] = [] 
	processInts (x:y:xs) = makeRanges [x, x + y - 1] : processInts xs 

parseMap :: String -> RangeMap 
parseMap s = RangeMap sourceStart (sourceStart + rangeLength - 1) (targetStart - sourceStart) where 
	[targetStart, sourceStart, rangeLength] = read <$> words s 

closestSeed :: (Ranges, [RangeMaps]) -> Integer 
closestSeed (r, ms) = head $ endpoints $ foldl applyMaps r ms 

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . closestSeed . parseInput