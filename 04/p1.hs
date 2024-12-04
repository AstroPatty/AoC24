import Prelude hiding (Right, Left)
import System.IO
import Data.List
import Data.Maybe
import Common

scanLocation :: Eq t => Matrix t -> Index -> [t] -> [Bool]
scanLocation m idx s = map (\i -> scan (toEnum i::Direction)) [0..7] where
    scan = scanDirection m idx s

getLocationScore :: Eq t => Matrix t -> Index -> [t] -> Int
getLocationScore m idx s = sum $ map (\x -> if x then 1 else 0) (scanLocation m idx s)


main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let m = lines contents
    let s = shape m
    let idxs = [(x,y) | x <- [0..fst s], y <- [0..snd s]]
    let score = sum $ map (\i -> getLocationScore m i "XMAS") idxs
    
    print score



