import Data.List
import Data.Maybe
import Data.Sequence qualified as Seq
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

countCheatsLongerThan :: Int -> Int -> Seq.Seq Int -> Int
countCheatsLongerThan len width path = pt
  where
    perTile = map (length . filter (>= len) . cheatLengths path width) [0 .. length path - 1]
    pt = sum perTile

cheatLengths :: Seq.Seq Int -> Int -> Int -> [Int]
cheatLengths path width tileidx = cheatLenghts
  where
    tile = Seq.index path tileidx
    toExclude = tile : catMaybes [path Seq.!? (tileidx + 2), path Seq.!? (tileidx - 2)]
    toCheck = filter (`notElem` toExclude) $ generateNeighbors width tile >>= generateNeighbors width
    indices = Seq.findIndicesL (`elem` toCheck) path
    cheatLenghts = map (\i -> i - tileidx - 2) indices

generateNeighbors :: Int -> Int -> [Int]
generateNeighbors width idx = [idx - 1, idx + 1, idx - width, idx + width]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let path = makePath ls
  print $ countCheatsLongerThan 100 (length $ head ls) path
  print $ Seq.length path
