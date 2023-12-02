import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List (tails)

main :: IO ()
main = interact $ (<> "\n") . show . sum . fmap processLine . lines

processLine :: String -> Int
processLine s = head digits * 10 + last digits where
	digits = tails s >>= toList . collectDigit

collectDigit :: String -> Maybe Int
collectDigit [] = Nothing
collectDigit (x:_) | isDigit x = Just $ read [x]
collectDigit xs | take 3 xs == "one" = Just 1
collectDigit xs | take 3 xs == "two" = Just 2
collectDigit xs | take 5 xs == "three" = Just 3
collectDigit xs | take 4 xs == "four" = Just 4
collectDigit xs | take 4 xs == "five" = Just 5
collectDigit xs | take 3 xs == "six" = Just 6
collectDigit xs | take 5 xs == "seven" = Just 7
collectDigit xs | take 5 xs == "eight" = Just 8
collectDigit xs | take 4 xs == "nine" = Just 9
collectDigit _ = Nothing