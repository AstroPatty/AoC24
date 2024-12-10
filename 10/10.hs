import Matrix
import System.IO
import Data.Maybe
import Data.List

getScores :: [(Index, [Index])] -> [Int]
getScores = map (\(_, p) -> length p)

findUniqueTrailEnds :: Matrix Int -> [(Index, [Index])]
findUniqueTrailEnds m = map (\(s, es) -> (s, nub es)) (findTrailEnds m)

findTrailEnds :: Matrix Int -> [(Index, [Index])]
findTrailEnds m = map (\s -> (s, findPeak m 0 s)) (findAll m 0)


findPeak :: Matrix Int -> Int -> Index -> [Index]
findPeak mat level idx 
    | isNothing entry = []
    | level /= val = [] 
    | val == 9 = [idx]
    | otherwise = foldl (++) [] (map (findPeak mat (level+1)) newIdxs) where
    entry = get mat idx
    val = fromJust entry
    newIdxs = map (\d -> idx ^+ toIndexShift (toEnum d::Direction)) [0..3]

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let m = readDigits $ lines contents
    let p1peaks = findUniqueTrailEnds m
    let p1score = sum $ getScores p1peaks
    print $ "Part 1: " ++ show p1score
    let p2peaks = findTrailEnds m
    let p2score = sum $ getScores p2peaks
    print $ "Part 2: " ++ show p2score
