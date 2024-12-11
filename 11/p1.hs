import Data.Maybe
import System.IO

evolveN :: [(Int -> Maybe [Int])] -> (Int -> [Int]) -> [Int] -> Int -> [Int]
evolveN  _ _ vals 0 = vals
evolveN fs lf vals n
    | n < 0 = vals
    | otherwise = evolveN fs lf (evolve fs lf vals) (n-1)

evolve :: [(Int -> Maybe [Int])] -> (Int -> [Int]) -> [Int] -> [Int]
evolve fs lf vals = concat $ map (apply fs lf) vals

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
    let result = evolveN [rule1, rule2] rule3 digits 75
    print $ length result
