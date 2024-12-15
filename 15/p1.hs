import System.IO
import Data.Array
import Data.List
import Data.Maybe
import Index

type Walls = Array Index Bool
type Boxes = Array Index Bool
type Warehouse = (Walls, Boxes)

splitBy :: Eq t => t -> [t] -> [[t]]
splitBy _ [] = []
splitBy value vals = first : rest where
    (first, rest_) = break (==value) vals
    rest = if length rest_ == 0 then [] else splitBy value $ tail rest_

extractBy :: Char -> [String] -> Array Index Bool
extractBy c d = listArray ((1, 1), (rows, cols)) $ entries where
    rows = length d
    cols = length (d !! 0)
    dat = concat d
    entries = map (\c_ -> if c_ == c then True else False) dat

makeWalls :: [String] -> Walls
makeWalls = extractBy '#' 

makeBoxes :: [String] -> Boxes
makeBoxes = extractBy 'O'

makeWarehouse :: [String] -> Warehouse
makeWarehouse d = (makeWalls d, makeBoxes d)

makeMoves :: String -> [Direction]
makeMoves "" = []
makeMoves (f:rest) = toDirection f : makeMoves rest

findStart :: [String] -> Index
findStart d = fst res where 
    res = fromJust $ find (\(_,val) -> val ==True) (assocs arr) 
    arr = extractBy '@' d

hasWall :: Warehouse -> Index -> Bool
hasWall (walls, _) idx = walls ! idx

hasBox :: Warehouse -> Index -> Bool
hasBox (_, boxes) idx = boxes ! idx

canMove :: Warehouse -> Index -> Direction -> Bool
canMove wh idx dir
    | hasWall wh nidx = False
    | otherwise = not $ all (\i -> hasWall wh i || hasBox wh i) (toWall wh nidx dir) where
    nidx = idx +^ dir

toWall :: Warehouse -> Index -> Direction -> [Index]
toWall wh idx dir
    | hasWall wh idx = []
    | otherwise = idx : toWall wh (idx +^ dir) dir where

takeBoxes :: Warehouse -> Index -> Direction -> [Index]
takeBoxes wh idx dir = takeWhile (hasBox wh) $ toWall wh idx dir

doMoves :: Warehouse -> Index -> [Direction] -> (Warehouse, Index)
doMoves wh idx [] = (wh, idx)
doMoves wh idx (dir:rest) = doMoves nwh nidx rest where
    (nwh, nidx) = move wh idx dir

move :: Warehouse -> Index -> Direction -> (Warehouse, Index)
move wh idx dir
    | not (hasWall wh midx ||  hasBox wh midx) = (wh, midx)
    | not $ canMove wh idx dir = (wh, idx)
    | otherwise = (nwh, midx) where
    midx = idx +^ dir
    nwh = push wh idx dir


push :: Warehouse -> Index -> Direction -> Warehouse
push wh idx dir = (walls, newboxes) where
    (walls, boxes) = wh
    boxidxs = takeBoxes wh (idx +^ dir) dir
    first = head boxidxs
    newlast = (last boxidxs) +^ dir
    newboxes = boxes // [(first, False), (newlast, True)]

toString :: Warehouse -> Index -> String
toString wh robot = concat rows where
    rows = map (rowToString) [1..nrows]
    rowToString = \i -> (map (idxToChar wh robot) (getRowIdxs i)) ++ "\n"
    getRowIdxs = \i -> [(i,x) | x <- [1..ncols]]
    ((_,_), (nrows, ncols)) = bounds $ fst wh

idxToChar :: Warehouse -> Index -> Index -> Char
idxToChar wh robot loc
    | hasWall wh loc = '#'
    | hasBox wh loc = 'O'
    | loc == robot = '@'
    | otherwise = '.'


score :: Warehouse -> Int
score (_,boxes) = sum $ map getBoxScore $ assocs boxes where
    getBoxScore = \((y,x), hasBox) -> if not hasBox then 0 else 100 * (y - 1) + (x - 1)

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let [wd, md] = splitBy "" $ lines contents
    let moves = map toDirection $ concat md
    let warehouse = makeWarehouse wd
    let start = findStart wd
    putStr $ toString warehouse start
    let (wh, fidx) = doMoves warehouse start moves
    putStr $ toString wh fidx
    print $ score wh
