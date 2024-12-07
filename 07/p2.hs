import System.IO
import Common

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let res = sum $ map (getLineValue [(+), (*), concatInts]) ls
    print res
