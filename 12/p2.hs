import Matrix
import System.IO
import Data.Maybe
import Data.List

type Score = (Int, Int)
type Labels = Matrix (Maybe Int)

labelPixels :: Matrix Char -> Labels -> [Index] -> Int -> (Labels,Int)
labelPixels _ ls [] l = (ls,l)
labelPixels m ls (idx:rest) l = labelPixels m newlabels rest newLabel where
    (newlabels, labelWasUsed) = labelPixel m ls idx l
    newLabel = if labelWasUsed then (l+1) else l

labelPixel :: Matrix Char -> Labels -> Index -> Int -> (Labels, Bool)
labelPixel values labels idx tag
    | (leftVal /= val) && (upVal /= val) = (setVal (Just tag) labels idx, True)
    | (leftVal /= val) = (setVal (fromJust (getVal labels upidx)) labels idx, False)
    | (upVal /= val) = (setVal (fromJust (getVal labels leftidx)) labels idx, False)
    | upLabel /= leftLabel = (setVal leftLabel relabeled idx, False)
    | otherwise = (setVal (fromJust (getVal labels upidx)) labels idx, False) where
    val = fromJust (getVal values idx)
    leftidx = idx ^+ (0,-1)
    upidx = idx ^+ (-1,0)
    (leftv,upv) = (getVal values leftidx, getVal values upidx)
    leftVal = if isJust leftv then fromJust leftv else '%'
    upVal = if isJust upv then fromJust upv else '%'
    (upl, leftl) = (getVal labels leftidx, getVal labels upidx)
    upLabel = if isJust upl then fromJust upl else Just (-1)
    leftLabel = if isJust leftl then fromJust leftl else Just (-2)
    relabeled = relabelPixels labels upLabel leftLabel


isCorner :: Labels -> Index -> (Index, Index) -> Bool
isCorner ls idx (m1,m2) =
    bothEmpty || (bothFull && not cFull) where
    val = fromJust $ getVal ls idx
    v1 = getVal ls (idx ^+ m1)
    v2 = getVal ls (idx ^+ m2)
    v3 = getVal ls (idx ^+ m1 ^+ m2)
    v1full = isJust v1 && fromJust v1 == val
    v2full = isJust v2 && fromJust v2 == val
    bothFull = v1full && v2full
    bothEmpty = not (v1full || v2full)
    cFull = isJust v1 && fromJust v3 == val



nCorners :: Labels -> Index -> Int
nCorners ls idx = length $ filter (==True) $ map (isCorner ls idx) checks where
    checks = [((1,0), (0,1)), ((-1,0), (0,1)), ((1,0),(0,-1)), ((-1,0), (0,-1))]

relabelPixels :: Labels -> Maybe Int -> Maybe Int -> Labels
relabelPixels ls old new = if old == new then ls else setAll ls idxs new where
    idxs = findAll ls old

labelGroups :: Matrix Char -> (Labels, Int)
labelGroups m = labelPixels m ls idxs 0 where
    s = shape m
    ls = fill s Nothing
    idxs = [(y,x) | y <- [0..(fst s) - 1], x <- [0..(snd s) - 1]]

getGroupScores :: Labels -> Int -> [Int]
getGroupScores labels max = map (getGroupScore labels) [0..max]

getGroupScore :: Labels -> Int -> Int
getGroupScore labels tag = (fst score) * (snd score) where
    score = getGroupValue labels tag

getGroupValue :: Labels -> Int -> Score
getGroupValue labels tag = foldl (^+) (0,0) $ map (getPixelValue labels) pixels where
    pixels = findAll labels (Just tag) 


getPixelValue :: Labels -> Index -> Score
getPixelValue ls idx = (1,corners) where
    corners =  nCorners ls idx

asString :: Show t => Matrix t -> String
asString [] = ""
asString (row:rest) = (concat (map show row)) ++ "\n" ++ asString rest

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let garden = lines contents
    let (labels,max) = labelGroups garden
    let s = getGroupScores labels max     
    print $ sum s
