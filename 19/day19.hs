import Control.Monad.State
import Data.Char
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import Parser
import System.IO

type Towels = [String]

type Combos = Set.Set String

type PatternCounts = Map.Map String Int

type PatternMap = Map.Map String Combos

checkPattern :: Towels -> String -> Bool
checkPattern _ "" = True
checkPattern towels pattern = isMatch
  where
    heads = filter (\s -> elem s towels) (inits pattern)
    rems = map (\h -> drop (length h) pattern) heads
    isMatch = if null heads then False else any (checkPattern towels) rems

countValid :: Towels -> [String] -> Int
countValid towels = sum . map (fromEnum . checkPattern towels)

countPattern :: Towels -> PatternCounts -> String -> PatternCounts
countPattern _ counts "" = Map.insert "" 1 counts
countPattern towels counts pattern = if null remainders then Map.insert pattern 0 counts else finalcounts
  where
    remainders = [remainder | Just remainder <- map (\t -> (stripPrefix t pattern)) towels]
    entriesToCalculate = [rem | (rem, Nothing) <- map (\rem -> (rem, Map.lookup rem counts)) remainders]
    newcounts = foldl' (\cs p -> countPattern towels cs p) counts entriesToCalculate
    count = sum $ map (\rem -> newcounts Map.! rem) remainders
    finalcounts = Map.insert pattern count newcounts

getPatternCounts :: Towels -> [String] -> [Int]
getPatternCounts towels patterns = map (map_ Map.!) patterns
  where
    map_ = foldr (\p cs -> countPattern towels cs p) Map.empty patterns

getTotalCombos :: Towels -> [String] -> Int
getTotalCombos towels = sum . getPatternCounts towels

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let (t : p) = splitBy "" $ lines contents
  let towels = parseBy (isLetter) (t !! 0)
  let patterns = p !! 0
  print $ countValid towels patterns
  print $ getTotalCombos towels patterns
