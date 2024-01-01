import Control.Monad (guard)

solveRace :: Integer -> Integer -> Integer
solveRace time record = toInteger $ length $ do
	hold <- [0..time]
	let run = time - hold 
	let distance = run * hold 
	guard $ distance > record 
-- ^ could be optimised a LOT

parseRaces :: String -> ([Integer], [Integer])
parseRaces s = (times, records) where 
	[timesection, recordsection] = lines s 
	times = read <$> tail (words timesection)
	records = read <$> tail (words recordsection)

solveRaces :: ([Integer], [Integer]) -> Integer
solveRaces (times, records) = product $ zipWith solveRace times records 

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . solveRaces . parseRaces