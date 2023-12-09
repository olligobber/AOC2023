import Data.Set (Set)
import qualified Data.Set as S 
import Data.List.Split (splitOn)

data Card = Card 
	{ winning :: Set Integer
	, have :: [Integer]
	} deriving (Show)

parseCard :: String -> Card 
parseCard s = Card (S.fromList wins) haves where 
	[_, numbers] = splitOn ": " s 
	[wins, haves] = fmap read . words <$> splitOn " | " numbers

numWins :: Card -> Int 
numWins c = length $ filter (flip elem $ winning c) $ have c

-- Take the list of wins on each card and the multipliers applied to them and get the total score
combineCards :: [Int] -> [Int] -> Int 
combineCards [] _ = 0
combineCards (x:xs) (y:ys) = y + combineCards xs (zipWith (+) ys $ replicate x y <> repeat 0)

combineAll :: [Int] -> Int 
combineAll = flip combineCards $ repeat 1

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . combineAll . fmap (numWins . parseCard) . lines