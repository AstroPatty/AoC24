import Control.Monad.State
import Data.Char
import Data.IntMap qualified as IM
import Data.Maybe
import Debug.Trace
import Index (Index, (^+), (^-))
import System.IO

type LengthCache = IM.IntMap Int

type Cache = State LengthCache

data DirPad
  = North
  | South
  | West
  | East
  | A
  deriving (Enum, Show, Eq, Ord)

getKey :: Int -> (DirPad, DirPad) -> Int
getKey layer (d1, d2) = layer * 25 + 5 * (fromEnum d1) + fromEnum d2

getDirPadIndex :: DirPad -> Index
getDirPadIndex dp = case dp of
  North -> (0, 1)
  South -> (1, 1)
  West -> (1, 0)
  East -> (1, 2)
  A -> (0, 2)

getKeyIndex :: Int -> Int
getKeyIndex c = case c of
  10 -> 2
  0 -> 1
  v -> v + 2

indexToDirections :: Index -> ((DirPad, Int), (DirPad, Int))
indexToDirections (y, x) = ((ys, abs y), (xs, abs x))
  where
    ys = if y >= 0 then South else North
    xs = if x >= 0 then East else West

toIdx :: Int -> Index
toIdx i = (y, x)
  where
    x = mod i 3
    y = 3 - div i 3

buildDirPadPaths :: DirPad -> DirPad -> [[DirPad]]
buildDirPadPaths from to = res
  where
    fromIdx = getDirPadIndex from
    toIdx = getDirPadIndex to
    (y, x) = toIdx ^- fromIdx
    ((diry, ny), (dirx, nx)) = indexToDirections (y, x)
    ys = replicate ny diry
    xs = replicate nx dirx
    p1 = [xs ++ ys ++ [A]]
    p2 = [ys ++ xs ++ [A]]
    res
      | fromIdx ^+ (y, 0) == (0, 0) || y == 0 = p1
      | fromIdx ^+ (0, x) == (0, 0) || x == 0 = p2
      | otherwise = p1 ++ p2

buildNumPadPaths :: Int -> Int -> [[DirPad]]
buildNumPadPaths from to = res
  where
    fromi = toIdx $ getKeyIndex from
    toi = toIdx $ getKeyIndex to
    (y, x) = toi ^- fromi
    ((diry, ny), (dirx, nx)) = indexToDirections (y, x)
    ys = replicate ny diry
    xs = replicate nx dirx
    p1 = [xs ++ ys ++ [A]]
    p2 = [ys ++ xs ++ [A]]
    res
      | fromi ^+ (y, 0) == (3, 0) || y == 0 = p1
      | fromi ^+ (0, x) == (3, 0) || x == 0 = p2
      | otherwise = p1 ++ p2

getNumMoves :: Int -> (DirPad, DirPad) -> Cache Int
getNumMoves layer move = do
  cache <- get
  let key = getKey layer move
  let cachedMoveCount = fromMaybe (-1) $ IM.lookup key cache
  if cachedMoveCount == -1
    then do
      moveCount <- countNumMoves layer move
      let newCache = cache
      modify (IM.insert key moveCount)
      return moveCount
    else return cachedMoveCount

countNumMoves :: Int -> (DirPad, DirPad) -> Cache Int
countNumMoves layer move = do
  let paths = map (A :) $ uncurry buildDirPadPaths move
  if layer == 0
    then return $ minimum $ map (\p -> length p - 1) paths
    else do
      pathLengths <-
        mapM
          ( \path -> do
              let moves = zip (init path) (tail path)
              lengths <- mapM (getNumMoves (layer - 1)) moves
              return $ sum lengths
          )
          paths
      return $ minimum pathLengths

getComplexity :: [DirPad] -> Int -> Int
getComplexity path layers = evalState (computeComplexity path layers) IM.empty

computeComplexity :: [DirPad] -> Int -> Cache Int
computeComplexity path layers = do
  let moves = zip (init path) (tail path)
  complexities <- mapM (getNumMoves layers) moves
  return $ sum complexities

getCodeValue :: [Int] -> Int
getCodeValue ls = sum $ map (\(i, val) -> val * 10 ^ i) (zip [0 ..] $ reverse $ init ls)

getCodeCost :: Int -> [Int] -> Int
getCodeCost layers code = (getCodeValue code) * minimum pathLengths
  where
    aCode = 10 : code
    pathSegments = map (uncurry buildNumPadPaths) (zip (init aCode) (tail aCode))
    numPadPaths = map ((A :) . concat) (sequence pathSegments)
    pathLengths = map (\p -> getComplexity p layers) numPadPaths

parseCode :: [Char] -> [Int]
parseCode [] = []
parseCode (c : rest) = d : parseCode rest
  where
    d = if isDigit c then digitToInt c else 10

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let codes = map parseCode $ lines contents
  let p1solution = sum $ map (getCodeCost 1) codes
  let p2solution = sum $ map (getCodeCost 24) codes
  print p1solution
  print p2solution
