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

widenWarehouse :: Warehouse -> Warehouse
widenWarehouse (walls, boxes) = (widen walls, widen boxes)

widen :: Array Index Bool -> Array Index Bool
widen boxes = listArray ((1,1), (height, 2*width)) entries where
    entries = concat $ map (getNewBox) (assocs boxes)
    getNewBox = \(_, hasBox) -> [hasBox, hasBox]
    (_,(height,width)) = bounds boxes

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
    | not $ hasBox wh nidx = True
    | otherwise = boxCanMove wh nidx dir where
    nidx = idx +^ dir

boxCanMove :: Warehouse -> Index -> Direction -> Bool
boxCanMove wh idx dir
    | dir == West || dir == East = not $ all (\i -> hasWall wh i || hasBox wh i) (toWall wh (idx +^ dir) dir)
    | hasWall wh (fidx1) || hasWall wh (fidx2) = False
    | f1hb && f2hb = boxCanMove wh fidx1  dir && boxCanMove wh fidx2 dir
    | f1hb = boxCanMove wh fidx1 dir 
    | f2hb = boxCanMove wh fidx2 dir 
    | otherwise = True where
    isLeft = isLeftHalf wh idx
    idx2 = if isLeft then idx +^ East else idx +^ West
    fidx1 = (idx +^ dir) 
    fidx2 = (idx2 +^ dir) 
    f1hb = hasBox wh fidx1
    f2hb = hasBox wh fidx2

isLeftHalf :: Warehouse -> Index -> Bool
isLeftHalf wh idx = even $ length (takeBoxes wh idx East) 

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
    nwh = 
        if dir == East || dir == West 
        then pushThin wh idx dir
        else pushWide wh idx dir


pushWide :: Warehouse -> Index -> Direction -> Warehouse
pushWide wh idx dir = (walls, newboxes) where
    (walls,boxes) = wh
    boxidxs = getPushesUD wh (idx +^ dir) dir
    newBoxIds = map (\ix -> ix +^ dir) boxidxs
    needsBox = filter (\ix -> not $ hasBox wh ix) newBoxIds
    needsDrop = filter (\ix -> not $ elem ix newBoxIds) boxidxs
    updates = (map (\ix -> (ix, True)) needsBox) ++ (map (\ix -> (ix, False)) needsDrop)
    newboxes = boxes // updates

areSameBox :: Warehouse -> (Index, Index) -> Bool
areSameBox wh (idx1, idx2) = isLeftHalf wh idx1 && idx1 +^ East == idx2

getPushesUD :: Warehouse -> Index -> Direction -> [Index]
getPushesUD wh idx dir
    | not $ hasBox wh idx = []
    | otherwise = [idx, idx2] ++ (getPushesUD wh fidx1 dir ++ getPushesUD wh fidx2 dir) where
    isLeft = isLeftHalf wh idx
    fidx1 = idx +^ dir 
    idx2 = if isLeft then idx +^ East else idx +^ West
    fidx2 = idx2 +^ dir 


pushThin :: Warehouse -> Index -> Direction -> Warehouse
pushThin wh idx dir = (walls, newboxes) where
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
score wh = sum $ map (getBoxScore) (assocs boxes) where
    (_, boxes) = wh
    getBoxScore = \(idx, hasBox) -> if (not hasBox) || (not $ isLeftHalf wh idx) then 0 else boxScore idx
    boxScore (y,x) = 100*(y-1) + (x-1)

xdist :: Warehouse -> Index -> Int
xdist wh idx = xd where
    (y,x) = idx
    (_, (ylim, xlim)) = bounds (fst wh)
    xd = min (x - 1) (xlim - x - 1)
    






main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let [wd, md] = splitBy "" $ lines contents
    let moves = map toDirection $ concat md
    let warehouse = widenWarehouse $ makeWarehouse wd
    let (y,x) = findStart wd
    let start = (y, 2*x - 1)
    let (nw, nidx) = doMoves warehouse start moves

    putStr $ toString warehouse start
    putStr $ toString nw nidx
    print $ score nw
