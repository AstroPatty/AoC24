import System.IO
import Data.Maybe
import Data.List
import Disk

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let disk = readDisk 0 (init contents)
    let score = getScore disk
    print score
