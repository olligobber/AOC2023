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

scoreCard :: Card -> Integer 
scoreCard c
	| numWins > 0 = 2 ^ (numWins - 1)
	| numWins == 0 = 0
	where
		numWins = length $ filter (flip elem $ winning c) $ have c

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap (scoreCard . parseCard) . lines