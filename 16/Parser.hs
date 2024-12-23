module Parser (
    splitBy,
    extractIndices,
    extractLocs,
    extractBy
) where

import Data.Array
import Data.List
import Index


splitBy :: Eq t => t -> [t] -> [[t]]
splitBy _ [] = []
splitBy value vals = first : rest where
    (first, rest_) = break (==value) vals
    rest = if length rest_ == 0 then [] else splitBy value $ tail rest_

extractLocs :: Char -> [String] -> ([Int], Int)
extractLocs c d = (idxs, width) where
    width = length (d !! 0)
    idxs = extract c (concat d) 0

extract :: Char -> String -> Int -> [Int]
extract c "" i = []
extract c (ch:rest) i = if ch == c then i:rem else rem where
    rem = extract c rest (i+1)

extractIndices :: Char -> [String] -> [Index]
extractIndices c d = idxs where
    arr = extractBy c d
    istrue = filter (\(_,entry) -> (entry == True)) (assocs arr)
    idxs = map (\(idx,_) -> idx) istrue

extractBy :: Char -> [String] -> Array Index Bool
extractBy c d = listArray ((1, 1), (rows, cols)) $ entries where
    rows = length d
    cols = length (d !! 0)
    dat = concat d
    entries = map (\c_ -> if c_ == c then True else False) dat

