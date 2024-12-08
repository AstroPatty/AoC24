import System.IO
import Antenna
import Index
import Data.List (nub)

antiNodes :: Antenna -> Antenna -> [Index]
antiNodes a1 a2
    | freq a1 /= freq a2 = []
    | otherwise = [loc2 ^+ diff, loc1 ^- diff] where
    loc1 = loc a1
    loc2 = loc a2
    diff = loc2 ^- loc1

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents 

    let size = (length ls, length (ls !! 0))
    let antennas = toFreqs $ parseLines ls
    let frequencies = map (\as -> freq $ as !! 0) antennas
    let antinodes = filter (inGrid size) (concat (map (getAntiNodes antiNodes) antennas))
    print $ length $ nub antinodes
