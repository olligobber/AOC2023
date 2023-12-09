import Data.Foldable (toList)
import Data.List.Split (splitOn)

data OffsetMap = OffsetMap 
	{ startRange :: Integer
	, endRange :: Integer
	, offset :: Integer 
	}

applyMap :: OffsetMap -> Integer -> Maybe Integer 
applyMap m x 
	| x >= startRange m && x <= endRange m = Just $ x + offset m 
	| otherwise = Nothing

parseMap :: String -> OffsetMap 
parseMap s = OffsetMap sourceStart (sourceStart + rangeLength - 1) (targetStart - sourceStart) where 
	[targetStart, sourceStart, rangeLength] = read <$> words s 

applyMaps :: [OffsetMap] -> Integer -> Integer 
applyMaps ms x = case foldMap (toList . flip applyMap x) ms of 
	[] -> x
	[y] -> y

parseInput :: String -> ([Integer], [[OffsetMap]])
parseInput s = (seeds, maps) where 
	(seedSection:mapSection) = splitOn "\n\n" s 
	seedStrings = tail $ words seedSection 
	seeds = read <$> seedStrings 
	maps = fmap parseMap . tail . lines <$> mapSection 

closestSeed :: ([Integer], [[OffsetMap]]) -> Integer 
closestSeed (seeds, maps) = minimum $ f <$> seeds where 
	f x = foldl (flip applyMaps) x maps

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . closestSeed . parseInput