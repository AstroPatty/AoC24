module Index (
    Index,
    (^+),
    (+^),
    (^*),
    Direction (..),
    toIndexShift,
    directions,
    toDirection
) where

import System.IO
import Data.List
import Data.Maybe
import Data.Char

type Index = (Int, Int)


infixl 7 ^+
(^+) :: Index -> Index -> Index
(^+) i1 i2 = (fst i1 + fst i2, snd i1 + snd i2)

infixl 7 ^*
(^*) :: Int -> Index -> Index
(^*) s i = (s * fst i, s * snd i)

infixl 7 +^
(+^) :: Index -> Direction -> Index
(+^) idx dir = idx ^+ toIndexShift dir

data Direction = 
      North 
    | South 
    | West
    | East
    deriving (Enum, Show, Eq)

toDirection :: Char -> Direction
toDirection c = case c of
        '<' -> West
        '^' -> North
        'v' -> South
        '>' -> East

directions :: [Index]
directions = [(1,0), (-1,0), (0,-1), (0,1)]

toIndexShift :: Direction -> Index
toIndexShift d = case d of
     North -> (-1,0)
     South -> (1,0)
     West -> (0, -1)
     East -> (0,1)

