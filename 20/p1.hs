import Data.List
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Debug.Trace
import System.IO

findStart :: [String] -> Int
findStart = fromJust . elemIndex 'S' . concat

findEnd :: [String] -> Int
findEnd = fromJust . elemIndex 'E' . concat

getNext :: Int -> Int -> Int -> [Int] -> Maybe Int
getNext width prev curr locations = n
  where
    toCheck = [curr - 1, curr + 1, curr - width, curr + width]
    neighbors = filter (`elem` toCheck) locations
    n = find (/= prev) neighbors

findPath :: [Int] -> Int -> Int -> Int -> Int -> [Int]
findPath tiles previous current width end = current : rest
  where
    next = getNext width previous current tiles
    rest = if isNothing next then [end] else findPath tiles current (fromJust next) width end

makePath :: [String] -> Seq.Seq Int
makePath d = Seq.fromList order
  where
    width = length $ head d
    startidx = findStart d
    endidx = findEnd d
    pathidxs = elemIndices '.' $ concat d
    order = findPath pathidxs (startidx + width) startidx width endidx

distance :: Int -> Int -> Int -> Int
distance i1 i2 width = abs (x1 - x2) + abs (y1 - y2)
  where
    x1 = mod i1 width
    x2 = mod i2 width
    y1 = div i1 width
    y2 = div i2 width

getCheats :: Seq.Seq Int -> Int -> Int -> Int -> Int -> Int
getCheats path width idx maxdist mintime = length results
  where
    inrange = getAllWithinDistance path width spatialIdx maxdist
    spatialIdx = Seq.index path idx
    results = Seq.filter (\(i, dist) -> (i - idx - dist) >= mintime) inrange

getAllWithinDistance :: Seq.Seq Int -> Int -> Int -> Int -> Seq.Seq (Int, Int)
getAllWithinDistance path width spatialIdx maxdist =
  Seq.filter (\(_, dist) -> dist <= maxdist) $ Seq.mapWithIndex (\i val -> (i, distance spatialIdx val width)) path

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let width = length $ head ls
  let path = makePath ls
  let cheatsp1 = sum (map (\i -> getCheats path width i 2 100) [0 .. length path - 1])
  let cheatsp2 = sum (map (\i -> getCheats path width i 20 100) [0 .. length path - 1])
  print cheatsp1
  print cheatsp2
