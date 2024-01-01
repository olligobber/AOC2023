import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Prelude hiding (Either(..))

type Connections = Map String (String, String)

data Direction = Left | Right deriving (Eq, Ord, Show)

follow :: Connections -> Direction -> String -> String 
follow m Left s = fst $ m M.! s
follow m Right s = snd $ m M.! s 

followUntilEndsZ :: Connections -> [Direction] -> String -> Integer 
followUntilEndsZ _ _ s | last s == 'Z' = 0
followUntilEndsZ m (d:ds) s = 1 + followUntilEndsZ m ds (follow m d s)

followAllUntilEndsZ :: Connections -> [Direction] -> [String] -> Integer 
followAllUntilEndsZ c d ss = foldl1 lcm $ followUntilEndsZ c d <$> ss 

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

getAllEndA :: Connections -> [String]
getAllEndA m = filter ((== 'A') . last) $ M.keys m 

solveInput :: (Connections, [Direction]) -> Integer 
solveInput (conns, dirs) = followAllUntilEndsZ conns dirs $ getAllEndA conns

getInput :: IO String
getInput = readFile "input"

main :: IO ()
main = getInput >>= print . solveInput . parseInput