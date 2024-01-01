import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Prelude hiding (Either(..))

type Connections = Map String (String, String)

data Direction = Left | Right deriving (Eq, Ord, Show)

follow :: Connections -> Direction -> String -> String 
follow m Left s = fst $ m M.! s
follow m Right s = snd $ m M.! s 

followUntilZZZ :: Connections -> [Direction] -> String -> Integer 
followUntilZZZ _ _ "ZZZ" = 0
followUntilZZZ m (d:ds) s = 1 + followUntilZZZ m ds (follow m d s)

parseDirection :: Char -> Direction 
parseDirection 'L' = Left 
parseDirection 'R' = Right 

parseDirections :: String -> [Direction]
parseDirections = cycle . fmap parseDirection

parseConnections :: [String] -> Connections 
parseConnections s = M.fromListWith undefined $ do 
	line <- s 
	let [a,b,c,_,_,_,_,d,e,f,_,_,g,h,i,_] = line 
	pure $ ([a,b,c], ([d,e,f], [g,h,i]))

parseInput :: String -> (Connections, [Direction])
parseInput s = (conns, dirs) where
	dirsection:_:connsection = lines s 
	conns = parseConnections connsection
	dirs = parseDirections dirsection

solveInput :: (Connections, [Direction]) -> Integer 
solveInput (conns, dirs) = followUntilZZZ conns dirs "AAA"

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . solveInput . parseInput