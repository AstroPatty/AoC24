module Common (
    Index,
    (^+),
    (^-),
    antiNodes,
    inLineAntinodes,
    getAntiNodes,
    parseLine,
    parseLines,
    freqs,
    findFreq,
    inGrid
) where

import Data.Set (Set, fromList)

type Index = (Int, Int)

infixl 7 ^+
(^+) :: Index -> Index -> Index
(^+) (y1,x1) (y2,x2) = (y1+y2, x1+x2)

infixl 7 ^-
(^-) :: Index -> Index -> Index
(^-) (y1,x1) (y2,x2) = (y1-y2, x1-x2)

inGrid :: Index -> Index -> Bool
inGrid (yb, xb) (y,x) = yi && xi where
    yi = y >= 0 && y < yb
    xi = x >= 0 && x < xb

reduce :: Index -> Index
reduce (y, x)
    | y == x = (1,1)
    | even x && even y = reduce (div y 2, div x 2)
    | otherwise = (y,x)

data Antenna = A Char Index deriving Show

freq :: Antenna -> Char
freq (A c _) = c

loc :: Antenna -> Index
loc (A _ idx) = idx

antiNodes :: Antenna -> Antenna -> [Index]
antiNodes a1 a2
    | freq a1 /= freq a2 = []
    | otherwise = [loc2 ^+ diff, loc1 ^- diff] where
    loc1 = loc a1
    loc2 = loc a2
    diff = loc2 ^- loc1

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

getAntiNodes :: (Antenna -> Antenna -> [Index]) -> [Antenna] -> [Index]
getAntiNodes f [] = []
getAntiNodes f (ant:rest) =
    concat (map (f ant) rest) ++ getAntiNodes f rest

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


freqs :: [Antenna] -> Set Char
freqs as = fromList $ map freq as

findFreq :: [Antenna] -> Char -> [Antenna]
findFreq as f = filter (\a -> freq a == f) as

