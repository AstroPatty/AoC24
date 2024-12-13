import System.IO
import Data.Maybe
import Data.Char
import Common

updateMachine :: Machine -> Machine
updateMachine ((b1, b2), (t1,t2)) = ((b1,b2), (t1+10000000000000, t2+10000000000000))

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let machines = map updateMachine $ parseMachines contents
    let costs = map (getMachineCost) machines
    let total = sum $ (map (fromJust) . filter (isJust)) costs
    print total
