module Index
  ( Index,
    DirIdx,
    manMag,
    flipDir,
    (^+),
    (^-),
    (+^),
    (^*),
    Direction (..),
    directionsToIndex,
    toIndexShift,
    indexToDirections,
    toDirIdx,
    directions,
    getTurns,
    toDirection,
  )
where

import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

type Index = (Int, Int)

type DirIdx = (Int, Int, Int)

indexToDirections :: Index -> ((Direction, Int), (Direction, Int))
indexToDirections (y, x) = ((ys, abs y), (xs, abs x))
  where
    ys = if y >= 0 then South else North
    xs = if x >= 0 then East else West

directionsToIndex :: [Direction] -> Index
directionsToIndex [] = (0, 0)
directionsToIndex (f : rest) = case f of
  North -> (-1, 0) ^+ r
  South -> (1, 0) ^+ r
  East -> (0, 1) ^+ r
  West -> (0, -1) ^+ r
  where
    r = directionsToIndex rest

toDirIdx :: Direction -> Index -> DirIdx
toDirIdx dir (y, x) = (fromEnum dir, y, x)

manMag :: Index -> Int
manMag (y, x) = abs y + abs x

infixl 7 ^+

(^+) :: Index -> Index -> Index
(^+) i1 i2 = (fst i1 + fst i2, snd i1 + snd i2)

infixl 7 ^-

(^-) :: Index -> Index -> Index
(^-) i1 i2 = (fst i1 - fst i2, snd i1 - snd i2)

infixl 7 ^*

(^*) :: Int -> Index -> Index
(^*) s i = (s * fst i, s * snd i)

infixl 7 +^

(+^) :: Index -> Direction -> Index
(+^) idx dir = idx ^+ toIndexShift dir

data Direction
  = North
  | South
  | West
  | East
  deriving (Enum, Show, Eq)

getTurns :: Direction -> [Direction]
getTurns dir
  | elem dir [North, South] = [East, West]
  | elem dir [East, West] = [North, South]

toDirection :: Char -> Direction
toDirection c = case c of
  '<' -> West
  '^' -> North
  'v' -> South
  '>' -> East

directions :: [Index]
directions = [(1, 0), (-1, 0), (0, -1), (0, 1)]

toIndexShift :: Direction -> Index
toIndexShift d = case d of
  North -> (-1, 0)
  South -> (1, 0)
  West -> (0, -1)
  East -> (0, 1)

flipDir :: Direction -> Direction
flipDir d = case d of
  North -> South
  South -> North
  East -> West
  West -> East
