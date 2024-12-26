import Data.List
import Parser
import System.IO

parseKey :: [String] -> [Int]
parseKey = map (length . filter (== '#'))

parseLock :: [String] -> [Int]
parseLock = map (negate . length . filter (== '#'))

parseSchematic :: [String] -> [Int]
parseSchematic schem = if all (== '#') (head schem) then parseLock sc else parseKey sc
  where
    sc = transpose schem

parseSchematics :: [[String]] -> [[Int]]
parseSchematics = map parseSchematic

fits :: [Int] -> [Int] -> Bool
fits key lock = all (<= 7) $ zipWith (+) key (map abs lock)

splitSchematics :: [[Int]] -> ([[Int]], [[Int]])
splitSchematics [] = ([], [])
splitSchematics (s : rest) = res
  where
    (keys, locks) = splitSchematics rest
    res = if head s > 0 then (s : keys, locks) else (keys, s : locks)

countGood :: ([[Int]], [[Int]]) -> Int
countGood (keys, locks) = sum $ map (fromEnum) $ map (uncurry fits) [(key, lock) | key <- keys, lock <- locks]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let schematics = parseSchematics $ splitBy "" ls
  print $ countGood $ splitSchematics schematics
