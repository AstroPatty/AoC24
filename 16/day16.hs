import Parser
import Index
import System.IO
import Data.Maybe
import Data.List
import Data.Function 
import Debug.Trace
import qualified Data.IntMap as IM

type Map = [Int]
type Scores = IM.IntMap Int
type ScoreMap = IM.IntMap [Int]
type MapState = (ScoreMap, Scores, Int)
type InPath = IM.IntMap Bool

makeInitialState :: Int -> Int -> MapState
makeInitialState width start = (scoremap, scores, width) where
    idx = (fromEnum East)*width*width + start
    scores = IM.singleton idx 0
    scoremap = IM.singleton 0 [idx]

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
getNeighbors width start = forward: side where 
    area = width*width
    dir = toEnum (start `div` area) :: Direction
    idx =  mod start area
    turns = getTurns dir
    forward = (toShift width start dir, 1)
    side = map (\d -> (area*(fromEnum d) + (idx), 1000)) turns

hasWall :: Map -> Int -> Int -> Bool
hasWall map width idx = elem mapidx map where
    mapidx = mod idx (width*width)

getNeighborCostUpdates :: Map -> Scores -> Int -> Int -> [(Int, Int)]
getNeighborCostUpdates mp scores width index = ns where
    curscore = scores IM.! index
    neighbors = filter (\(i,_) -> not $ hasWall mp width i) $ getNeighbors width index
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

toShift :: Int -> Int -> Direction -> Int
toShift width start direction = case direction of
    North -> start - width
    South -> start + width
    East -> start + 1
    West -> start - 1

getTilesInPath :: Map -> Scores -> Int -> InPath -> Int -> InPath
getTilesInPath mp scores width pathmap idx
    | isJust curscore && fromJust curscore == 0 = newpm
    | otherwise = result where
    neighbors = getLowestFeeders mp scores idx width
    unvisited = filter (\i -> not $ IM.findWithDefault False i pathmap) neighbors
    curscore = IM.lookup idx scores
    newpm = IM.insert idx True pathmap
    result = foldl (getTilesInPath mp scores width) newpm unvisited

getLowestFeeders :: Map -> Scores -> Int -> Int -> [Int]
getLowestFeeders mp scores idx width = feeders where
    tscore = fromJust (IM.lookup idx scores) 
    tiles = getFeedTiles idx width
    notedge = filter (\i -> not $ hasWall mp width i) tiles
    withscores = [(i,x) | (i, Just x) <- map (\i -> (i, IM.lookup i scores)) notedge]
    sorted = sortBy (\(_, b1) (_, b2) -> compare b1 b2) withscores
    lowest = snd $ head sorted 
    islow = filter (\(i,score) -> score == (tscore - 1000) || score == (tscore - 1)) sorted
    feeders = map (\(i,_) -> i) islow

getFeedTiles :: Int -> Int -> [Int]
getFeedTiles idx width = idxs ++ turnsInTile where
    area = width*width
    tindx = mod idx area
    dir = toEnum (div idx area) :: Direction
    turns = getTurns dir
    feeders = map (\dir -> (toShift width tindx dir, flipDir dir)) [North, South, East, West]
    turnsInTile = map (\dir -> tindx + area*(fromEnum dir)) turns
    idxs = map (\(idx,dir) -> idx + area*(fromEnum dir)) feeders
    
toTile :: Int -> Int -> Int
toTile width idx = yx where
    yx = mod idx (width*width)


main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let (walls, width) = extractLocs '#' ls
    let start = head . fst $ extractLocs 'S' ls
    let end = head . fst $ extractLocs 'E' ls
    let ends = [end + i*width*width | i <- [0..3]]
    let state = makeInitialState width start
    let (scoremap, scores, _) = doUpdate walls state
    let endscores = [(x,i)| (Just x, i) <- map (\i -> (IM.lookup i scores,i)) ends]
    let (value, index) = foldr1 min endscores
    print value
    let p = getTilesInPath walls scores width IM.empty index
    let visitedtiles = nub $ map (toTile width) (IM.keys p)
    print $ length visitedtiles
