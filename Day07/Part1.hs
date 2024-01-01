import Data.List (sort, group, sortBy)
import Data.Ord (comparing)

data Card 
	= Two
	| Three
	| Four
	| Five
	| Six
	| Seven
	| Eight 
	| Nine 
	| Ten 
	| Jack
	| Queen
	| King
	| Ace
	deriving (Eq, Ord, Show)

data HandType
	= HighCard 
	| OnePair
	| TwoPair
	| ThreeOfKind
	| FullHouse
	| FourOfKind
	| FiveOfKind
	deriving (Eq, Ord, Show)

data Hand = Hand [Card] deriving (Eq, Ord, Show)

getType :: Hand -> HandType 
getType (Hand cards) = case sort $ fmap length $ group $ sort cards of 
	[5] -> FiveOfKind 
	[1,4] -> FourOfKind 
	[2,3] -> FullHouse 
	[1,1,3] -> ThreeOfKind 
	[1,2,2] -> TwoPair 
	[1,1,1,2] -> OnePair 
	[1,1,1,1,1] -> HighCard

parseCard :: Char -> Card 
parseCard 'A' = Ace
parseCard 'K' = King
parseCard 'Q' = Queen
parseCard 'J' = Jack
parseCard 'T' = Ten 
parseCard '9' = Nine 
parseCard '8' = Eight 
parseCard '7' = Seven
parseCard '6' = Six
parseCard '5' = Five
parseCard '4' = Four
parseCard '3' = Three
parseCard '2' = Two

parseHand :: String -> Hand 
parseHand = Hand . fmap parseCard

parseGames :: String -> [(Hand, Integer)]
parseGames s = do
	line <- lines s 
	let [handString, betString] = words line 
	pure (parseHand handString, read betString)

scoreGames :: [(Hand, Integer)] -> Integer 
scoreGames gs = sum $ zipWith scoreGame [1..] $ sortBy (comparing valGame) gs where 
	scoreGame rank (_, bet) = rank * bet 
	valGame (game, _) = (getType game, game)

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . scoreGames . parseGames