module Parser (
    splitBy,
    parseDigits,
) where

import Data.Array
import Data.List
import Data.Char


splitBy :: Eq t => t -> [t] -> [[t]]
splitBy _ [] = []
splitBy value vals = first : rest where
    (first, rest_) = break (==value) vals
    rest = if length rest_ == 0 then [] else splitBy value $ tail rest_

parseDigits :: String -> [Int]
parseDigits "" = []
parseDigits s = val : parseDigits rest where 
    digitString = dropWhile (not . isDigit) s
    (v,rest) = span (isDigit) digitString
    val = read v :: Int
    

