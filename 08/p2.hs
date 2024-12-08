import System.IO
import Antenna
import Index
import Data.List (nub)

antiNodes :: Index -> Antenna -> Antenna -> [Index]
antiNodes size a1 a2
    | freq a1 /= freq a2 = []
    | otherwise = idx2 : getPoints size (^+ diff) idx2 ++ getPoints size (^- diff) idx2 where
    idx2 = loc a2
    diff = reduce (idx2 ^- loc a1)

getPoints :: Index -> (Index -> Index) -> Index -> [Index]
getPoints size f idx
    | inGrid size newIdx = newIdx : getPoints size f newIdx
    | otherwise = [] where
    newIdx = f idx


main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents 

    let size = (length ls, length (ls !! 0))
    let antennas = toFreqs $ parseLines ls
    let frequencies = map (\as -> freq $ as !! 0) antennas
    let func = antiNodes size

    let antinodes = filter (inGrid size) (concat (map (getAntiNodes func) antennas))
    print $ length $ nub antinodes
