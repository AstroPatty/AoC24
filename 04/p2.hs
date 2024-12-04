import Prelude hiding (Right, Left, flip)
import System.IO
import Common

scanDiagonals :: Eq t => Matrix t -> Index -> [t] -> [Bool]
scanDiagonals m idx s = map (\i -> scan (toEnum i::Direction)) [4..7] where
    scan = scanDirection m idx s

getCrossCombos :: Index -> Direction -> [(Index, Direction)]
getCrossCombos idx dir = zip indices dirs where
    comps = components dir
    indices = map (\c -> idx ^+ (2^*(toIndexShift c))) comps
    dirs = [combine (flip $ comps !! 0) (comps !! 1), combine (comps !! 0 ) (flip $ comps !! 1)]

hasCross :: Eq t => Matrix t -> Index -> [t] -> Direction -> [Bool]
hasCross m idx s d = map (\(idx, nd) -> scanDirection m idx s nd) (getCrossCombos idx d)

countCrosses :: Eq t => Matrix t -> Index -> [t] -> Int
countCrosses m idx s = sum (map (\x -> if x then 1 else 0) totals) where
    hasSeq = scanDiagonals m idx s
    dirsToCheck = [ d | (d, hs) <- zip (map (\i -> toEnum i::Direction) [4..7]) hasSeq, hs]
    totals = foldl (++) [] (map checkCross dirsToCheck)
    checkCross = hasCross m idx s

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
    let score = sum $ map (\i -> countCrosses m i "MAS") idxs
    
    print (div score 2)


