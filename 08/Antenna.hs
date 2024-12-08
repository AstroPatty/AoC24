module Antenna (
    Antenna,
    inLineAntinodes,
    getAntiNodes,
    parseLine,
    parseLines,
    freq,
    loc,
    freqs,
    findFreq,
    toFreqs
) where

import Index
import Data.Set (Set, fromList, toList)

data Antenna = A Char Index deriving Show

parseLine :: String -> Index -> [Antenna]
parseLine [] _ = []
parseLine (f:s) idx
    | f == '.' = rest
    | otherwise = (A f idx) :  rest where
    rest = parseLine s (idx ^+ (0,1))

parseLines :: [String] -> [Antenna]
parseLines [] = []
parseLines lns = foldl (++) [] (zipWith parseLine lns idxs) where
    idxs = [(y,0) | y <- [0..(length lns) - 1]]

freq :: Antenna -> Char
freq (A c _) = c

loc :: Antenna -> Index
loc (A _ idx) = idx

toFreqs :: [Antenna] -> [[Antenna]]
toFreqs as = map (findFreq as) (freqs as)

freqs :: [Antenna] -> [Char]
freqs as = toList $ fromList $ map freq as

findFreq :: [Antenna] -> Char -> [Antenna]
findFreq as f = filter (\a -> freq a == f) as

getAntiNodes :: (Antenna -> Antenna -> [Index]) -> [Antenna] -> [Index]
getAntiNodes f [] = []
getAntiNodes f (ant:rest) =
    concat (map (f ant) rest) ++ getAntiNodes f rest

inLineAntinodes :: Index -> Antenna -> Antenna -> [Index]
inLineAntinodes size a1 a2
    | freq a1 /= freq a2 = []
    | otherwise = idx2 : getPoints size (^+ diff) idx2 ++ getPoints size (^- diff) idx2 where
    idx2 = loc a2
    diff = reduce (idx2 ^- loc a1)

getPoints :: Index -> (Index -> Index) -> Index -> [Index]
getPoints size f idx
    | inGrid size newIdx = newIdx : getPoints size f newIdx
    | otherwise = [] where
    newIdx = f idx


