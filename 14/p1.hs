import System.IO
import Data.Maybe
import Data.Char
import Common

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let robots = map (parseRobot) $ lines contents
    let newrobots = map (\r -> evolve r (101,103) 100) robots
    let score = getScore newrobots (101,103)
    print score
