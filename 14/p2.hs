import System.IO
import Data.Maybe
import Data.Char
import Data.List
import Common
import Data.IntMap (elems)
import Matrix

allDifferent :: [Robot] -> Bool
allDifferent r = length pos == length upos where
    pos = map (\(p,_) -> p) r
    upos = nub pos

totalSymmetric :: (Int, Int) -> [Robot] -> Bool
totalSymmetric size rs = if not $ symmetric size rs then False else total where
   total = isTotallySymmetric grid
   grid = makeGrid rs size

isTotallySymmetric :: Eq t => Matrix t -> Bool
isTotallySymmetric [] = True
isTotallySymmetric (row:rest) = fh == reverse sh && isTotallySymmetric rest where
    (fh, m:sh) = splitAt hw row 
    hw = ((length row) - 1) `div` 2

symmetric :: (Int, Int) -> [Robot] -> Bool
symmetric size rs = if length vals == 4 then n0 == n2 && n1 == n3 else False where
    vals = elems $ countByQuadrant rs size 
    [n0, n1, n2, n3] = vals

makeGrid :: [Robot] -> (Int, Int) -> Matrix Bool
makeGrid rs (x,y) = setAll empty idxs True where
    empty = fill (y,x) False
    idxs = map (\((x,y),_) -> (y,x)) rs

showRobotGrid :: Matrix Bool -> String
showRobotGrid [] = ""
showRobotGrid (row:rs) = map (\p -> if p then '0' else '.') row ++ "\n" ++ showRobotGrid rs

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let size = (101,103)
    let robots = map (parseRobot) $ lines contents
    let (step,rs) = evolveTo robots size (allDifferent)
    print step
    putStrLn $ showRobotGrid $ makeGrid rs size
