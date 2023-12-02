import Data.List.Split (splitOn)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Semigroup (Max(Max, getMax))

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . sum . fmap fst . filter (possible . snd) . fmap processGame . lines

data Colours = Red | Blue | Green deriving (Eq, Ord, Show)

newtype AccuMap k v = AccuMap { getMap :: Map k v } deriving (Eq, Ord, Show)

instance (Ord k, Semigroup v) => Semigroup (AccuMap k v) where
	AccuMap m <> AccuMap n = AccuMap $ M.unionWith (<>) m n

instance (Ord k, Semigroup v) => Monoid (AccuMap k v) where
	mempty = AccuMap mempty

processGame :: String -> (Int, AccuMap Colours (Max Int))
processGame xs = (gameno, cols) where
	[gameinfo, pieceinfo] = splitOn ": " xs
	gameno = read $ words gameinfo !! 1 :: Int
	cols = foldMap processPieces (splitOn "; " pieceinfo >>= splitOn ", ")

processPieces :: String -> AccuMap Colours (Max Int)
processPieces xs = case words xs of
	[n, "red"] -> AccuMap $ M.singleton Red $ Max (read n :: Int)
	[n, "blue"] -> AccuMap $ M.singleton Blue $ Max (read n :: Int)
	[n, "green"] -> AccuMap $ M.singleton Green $ Max (read n :: Int)
	_ -> undefined

possible :: AccuMap Colours (Max Int) -> Bool
possible (AccuMap m) = (getCol Red <= 12) && (getCol Green <= 13) && (getCol Blue <= 14) where
	getCol x = getMax $ M.findWithDefault 0 x m