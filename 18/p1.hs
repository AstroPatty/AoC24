import qualified Data.IntMap as IM
import Data.Maybe
import Parser
import System.IO

type Map = [Int]
type ScoreMap = IM.IntMap [Int]
type Scores = IM.IntMap Int
type MapState = (ScoreMap, Scores, Int)

getNextVisit :: ScoreMap -> (Maybe Int, ScoreMap)
getNextVisit scoremap
    | IM.null scoremap = (Nothing, scoremap)
    | otherwise = (Just (head vals), newmap) where
    (score, vals) = fromJust $ IM.lookupGE 0 scoremap
    newmap = 
        if length vals == 1
        then IM.delete score scoremap
        else IM.insert score (tail vals) scoremap

updateScoremap :: ScoreMap -> Int -> Int -> Int -> ScoreMap
updateScoremap scoremap location old new = newscoremap where
    newscoremap = ins . del $ scoremap
    ins = IM.alter (\v -> if isJust v then fmap (location:) v else  Just [location]) new
    del = IM.alter (\v -> if isJust v && (length $ fromJust v) == 1 then Nothing else fmap (filter (/= location)) v) old 

updateScore :: Scores -> Int -> Int -> Scores
updateScore scores location new = IM.insert location new scores

doScoreUpdate :: MapState -> [(Int, Int)] -> MapState
doScoreUpdate ms []  = ms
doScoreUpdate (scoremap, scores, width) (first:rest) = doScoreUpdate (nsm, nscores, width) rest where
    (newscore, index) = first
    curscore = IM.lookup index scores
    oldscore = if isJust curscore then fromJust curscore else -1
    nsm = updateScoremap scoremap index oldscore newscore
    nscores = updateScore scores index newscore

getNeighbors :: Int -> Int -> [(Int, Int)]
getNeighbors width start = ns ++ ew where
    ns = [(start+width, 1), (start - width, 1)]
    ew = 
        if mod start width == 0 then [(start + 1, 1)]
        else if mod (start+1) width == 0 then [(start-1,1)]
        else [(start+1,1), (start-1,1)]

isBlocked :: Map -> Int -> Int -> Bool
isBlocked map width idx = elem idx map || idx < 0 || idx > (width * width) where
    

getNeighborCostUpdates :: Map -> Scores -> Int -> Int -> [(Int, Int)]
getNeighborCostUpdates mp scores width index = ns where
    curscore = scores IM.! index
    neighbors = filter (\(i,_) -> not $ isBlocked mp width i) $ getNeighbors width index
    updates = map (\(idx, ns) -> (curscore + ns, idx)) neighbors
    ns = filterNeighborCostUpdates scores index updates

filterNeighborCostUpdates :: Scores -> Int -> [(Int, Int)] -> [(Int, Int)]
filterNeighborCostUpdates _  _ [] = []
filterNeighborCostUpdates scores index (first:rest)
    | isNothing curscore || (fromJust curscore) > newscore = first : filterNeighborCostUpdates scores index rest
    | otherwise = filterNeighborCostUpdates scores index rest where
    (newscore, index) = first
    curscore = IM.lookup index scores


doUpdate :: Map -> MapState -> MapState
doUpdate map state = if isNothing next || nextidx < 0 then state else doUpdate map newstate where
    (scoremap, scores, width) = state
    (next, ism) = getNextVisit scoremap
    nextidx = fromJust next
    curscore = scores IM.! nextidx
    updates = getNeighborCostUpdates map scores width nextidx 
    newstate = doScoreUpdate (ism, scores, width) updates

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let width = 71
    let vals = map (parseDigits) ls
    let walls = take 1024 $ map (\(x,y) -> x + y*width) vals
    let scores = IM.fromList [(0,0)]
    let scoremap = IM.fromList [(0,[0])]
    let state = (scoremap, scores, width)
    let (_,sc,_) = doUpdate walls state
    print $ sc

