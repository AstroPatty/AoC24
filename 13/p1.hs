import System.IO
import Data.Maybe
import Data.Char
import Common

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let machines = parseMachines contents
    let costs = map (getMachineCost) machines
    let total = sum $ (map (fromJust) . filter (isJust)) costs
    print total
