import System.IO
import Common
import Data.Set (toList)
import Data.List (nub)

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents 
    let size = (length ls, length (ls !! 0))
    let antennas = parseLines ls
    let frequencies = freqs antennas
    let as = map (findFreq antennas) (toList frequencies)
    let antinodes = filter (inGrid size) (concat (map (getAntiNodes $ inLineAntinodes size) as))
    print $ length $ nub antinodes
