import Control.Monad.State
import Data.Bits
import Data.Map qualified as Map
import Data.Maybe
import Data.String
import Debug.Trace
import System.IO

type SecretNumber = State Int

mix :: Int -> Int -> Int
mix = xor

prune a = mod a 16777216

step1 num = prune $ mix num (num * 64)

step2 num = prune $ mix num (div num 32)

step3 num = prune $ mix num (num * 2048)

getN :: Int -> Int -> Int
getN 0 i = i
getN n i = getN (n - 1) $ step3 $ step2 $ step1 i

getNextPrice :: SecretNumber Int
getNextPrice = do
  n <- get
  let newn = step3 $ step2 $ step1 n
      newprice = mod newn 10
  put newn
  return newprice

getNextNPrices :: Int -> SecretNumber [Int]
getNextNPrices n = replicateM n getNextPrice

getPrices :: Int -> Int -> [Int]
getPrices n start = mod start 10 : rest
  where
    (rest, _) = runState (getNextNPrices (n - 1)) start

getPriceChanges :: [Int] -> [Int]
getPriceChanges list = zipWith (-) (tail list) (init list)

buildSequenceMap :: [Int] -> [Int] -> Map.Map [Int] Int
buildSequenceMap prices changes = if length changes >= 4 then res else Map.empty
  where
    rest = buildSequenceMap (tail prices) (tail changes)
    price = prices !! 4
    res = Map.insert (take 4 changes) price rest

getSequenceMap :: Int -> Map.Map [Int] Int
getSequenceMap val = buildSequenceMap prices changes
  where
    prices = getPrices 2000 val
    changes = getPriceChanges prices

getSequences :: [Int] -> Map.Map [Int] Int
getSequences vals = foldl (Map.unionWith (+)) Map.empty maps
  where
    maps = map getSequenceMap vals

combine :: Int -> Maybe Int -> Maybe Int
combine left right
  | isNothing right = Just left
  | otherwise = Just left

getMax xs = Map.keys $ Map.filter (== m) xs
  where
    m = maximum $ Map.elems xs

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let starts = map read (lines contents) :: [Int]
  let seqs = getSequences starts
  print $ sum $ map (getN 2000) starts
  print $ maximum $ Map.elems seqs
