module Matrix
  ( Matrix,
    Index,
    (^+),
    (^*),
    Direction,
    toIndexShift,
    readDigits,
    findVal,
    findAll,
    getVal,
    setVal,
    setAll,
    shape,
    fill,
    directions,
  )
where

import Data.Char
import Data.List
import Data.Maybe
import System.IO
import Prelude hiding (Left, Right, flip)

type Matrix t = [[t]] -- We'll use row orientation

type Index = (Int, Int)

infixl 7 ^+

(^+) :: Index -> Index -> Index
(^+) i1 i2 = (fst i1 + fst i2, snd i1 + snd i2)

infixl 7 ^*

(^*) :: Int -> Index -> Index
(^*) s i = (s * fst i, s * snd i)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Enum)

directions :: [Index]
directions = [(1, 0), (-1, 0), (0, -1), (0, 1)]

toIndexShift :: Direction -> Index
toIndexShift d = case d of
  Up -> (1, 0)
  Down -> (-1, 0)
  Left -> (0, -1)
  Right -> (0, 1)

fill :: Index -> t -> Matrix t
fill (rows, cols) val = replicate rows $ replicate cols val

getVal :: Matrix t -> Index -> Maybe t
getVal m (y, x)
  | y < 0 || y >= length m = Nothing
  | x < 0 || x >= length row = Nothing
  | otherwise = Just $ row !! x
  where
    row = m !! y

setVal :: t -> Matrix t -> Index -> Matrix t
setVal val m (rowi, coli) = if isNothing $ getVal m (rowi, coli) then m else newm
  where
    newm = zipWith (replaceRow) m [0 ..]
    replaceRow = \row ri -> if ri /= rowi then row else zipWith (replaceInRow) row [0 ..]
    replaceInRow = \v ci -> if ci /= coli then v else val

setAll :: Matrix t -> [Index] -> t -> Matrix t
setAll m idxs val = newm
  where
    newm = zipWith (replaceRow) m [0 ..] where
    replaceRow = \row ri -> setAllInRow row (getRowIdxs ri) val
    getRowIdxs = \rowi -> map (snd) $ filter (\idx -> rowi == (fst idx)) idxs

setAllInRow :: [t] -> [Int] -> t -> [t]
setAllInRow vals [] _ = vals
setAllInRow vals idxs val = zipWith (replaceInRow) vals [0 ..]
  where
    replaceInRow = \v ci -> if elem ci idxs then val else v

findVal :: (Eq t) => Matrix t -> t -> Maybe Index
findVal [] _ = Nothing
findVal matrix value = if length idxs > 0 then Just $ head idxs else Nothing
  where
    idxs = findAll matrix value

findAll :: (Eq t) => Matrix t -> t -> [Index]
findAll matrix value = foldl (++) [] (zipWith (findInRow) matrix [0 ..])
  where
    findInRow r i = [(i, c) | c <- findIndices (== value) r]

readDigits :: Matrix Char -> Matrix Int
readDigits [] = []
readDigits (r1 : rest) = readDigitRow r1 : readDigits rest
  where
    readDigitRow r = map (digitToInt) r

shape :: Matrix t -> (Int, Int)
shape [[]] = (0, 0)
shape m = (length column, length m)
  where
    column = m !! 0
