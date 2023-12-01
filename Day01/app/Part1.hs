import Data.Char (isDigit)

main :: IO ()
main = interact $ (<> "\n") . show . sum . fmap processLine . lines

processLine :: String -> Int
processLine s = read $ [head digits, last digits] where
	digits = filter isDigit s