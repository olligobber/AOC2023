extendSequence :: [Integer] -> Integer 
extendSequence s 
	| all (== 0) s = 0 
	| otherwise = last s + extendSequence (derivative s)

derivative :: [Integer] -> [Integer]
derivative s = zipWith (-) (tail s) s 

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap (extendSequence . fmap read . words) . lines