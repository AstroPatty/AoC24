import Data.Maybe
import System.IO
import qualified Data.IntMap as IntMap

type Counter = IntMap.IntMap Int

evolveN :: [(Int -> Maybe [Int])] -> (Int -> [Int]) -> Counter -> Int -> Counter
evolveN  _ _ vals 0 = vals
evolveN fs lf vals n
    | n < 0 = vals
    | otherwise = evolveN fs lf (evolve fs lf vals) (n-1)

evolve :: [(Int -> Maybe [Int])] -> (Int -> [Int]) -> Counter -> Counter
evolve fs lf counts = IntMap.fromListWith (+) newMap where
    newMap = concat $ map toEntries (IntMap.toList counts)
    toEntries = \(val,count) -> [(newval, count) | newval <- apply fs lf val]


apply :: [(Int -> Maybe [Int])] -> (Int -> [Int]) -> Int -> [Int]
apply [] f i = f i
apply (f:rest) lf i
    | isNothing res = apply rest lf i
    | otherwise = fromJust res where
    res = f i

rule1 :: Int -> Maybe [Int]
rule1 i = if i == 0 then Just [1] else Nothing

rule2 :: Int -> Maybe [Int]
rule2 i = if even ndigits then isplit else Nothing where
    ndigits = countDigits i
    p10 = 10^(ndigits `div` 2)
    first = (i `div` p10)
    isplit = Just [first, i - first*p10]

rule3 :: Int -> [Int]
rule3 i = [i*2024]


countDigits :: Int -> Int
countDigits n = if n > 0 then 1 + countDigits (div n 10) else 0

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let digits = map (read) (words $ init contents) :: [Int]
    let counter = IntMap.fromListWith (+) (zip digits (repeat 1))
    let result = evolveN [rule1, rule2] rule3 counter 75
    let score = IntMap.foldl (+) 0 result
    print $ score
