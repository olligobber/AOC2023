extendSequenceBack :: [Integer] -> Integer 
extendSequenceBack s 
	| all (== 0) s = 0 
	| otherwise = head s - extendSequenceBack (derivative s)

derivative :: [Integer] -> [Integer]
derivative s = zipWith (-) (tail s) s 

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap (extendSequenceBack . fmap read . words) . lines