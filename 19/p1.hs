import System.IO
import Parser
import Data.Char
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace
import Data.Maybe

type Towels = [String]
type Combos = Set.Set String
type PatternCounts = Map.Map String Int
type PatternMap = Map.Map String Combos


makePatternCounts :: Towels -> PatternCounts
makePatternCounts towels = foldr (\s -> Map.insert s 1) Map.empty towels

checkPattern :: Towels -> String -> Bool
checkPattern _ "" = True
checkPattern towels pattern = isMatch where
    heads = filter (\s -> elem s towels) (inits pattern)
    rems = map (\h -> drop (length h) pattern) heads
    isMatch = if null heads then False else any (checkPattern towels) rems 

countPattern :: Towels -> PatternCounts -> String -> PatternCounts
countPattern _ counts "" = Map.insert "" 1 counts
countPattern towels counts pattern = if null remainders then Map.insert pattern 0 counts else finalcounts where
    remainders = [remainder | Just remainder <- map (\t -> (stripPrefix t pattern)) towels]
    entriesToCalculate = [rem | (rem, Nothing) <- map (\rem -> (rem, Map.lookup rem counts)) remainders]
    newcounts = foldl' (\cs p -> countPattern towels cs p) counts entriesToCalculate
    count = sum $ map (\rem -> newcounts Map.! rem) remainders
    finalcounts = Map.insert pattern count newcounts

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let (t: p) = splitBy "" $ lines contents
    let towels = parseBy (isLetter) (t !! 0)
    let patterns = p !! 0

    print $ sum $ map (fromEnum . checkPattern towels) patterns

    let counts = Map.empty
    let newcounts = foldr (\p cs -> countPattern towels cs p) counts patterns
    print $ sum $ map (newcounts Map.!) patterns
