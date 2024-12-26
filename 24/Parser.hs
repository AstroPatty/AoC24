module Parser
  ( splitBy,
    parseBy,
  )
where

import Data.Array
import Data.Char
import Data.List

splitBy :: (Eq t) => t -> [t] -> [[t]]
splitBy _ [] = []
splitBy value vals = first : rest
  where
    (first, rest_) = break (== value) vals
    rest = if null rest_ then [] else splitBy value $ tail rest_

parseBy :: (t -> Bool) -> [t] -> [[t]]
parseBy _ [] = []
parseBy f list = first : parseBy f rest
  where
    (_, r) = break f list
    (first, rest) = span f r
