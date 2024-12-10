module Matrix (
    Matrix,
    Index,
    (^+),
    (^*),
    Direction,
    toIndexShift,
    readDigits,
    findAll,
    get,
    shape,
) where

import Prelude hiding (Right, Left, flip)
import System.IO
import Data.List
import Data.Maybe
import Data.Char

type Matrix t = [[t]] -- We'll use row orientation
type Index = (Int, Int)


infixl 7 ^+
(^+) :: Index -> Index -> Index
(^+) i1 i2 = (fst i1 + fst i2, snd i1 + snd i2)

infixl 7 ^*
(^*) :: Int -> Index -> Index
(^*) s i = (s * fst i, s * snd i)

data Direction = 
      Up 
    | Down 
    | Left
    | Right
    deriving Enum
    
toIndexShift :: Direction -> Index
toIndexShift d = case d of
     Up -> (1,0)
     Down -> (-1,0)
     Left -> (0, -1)
     Right -> (0,1)

get :: Matrix t -> Index -> Maybe t
get m idx
    | y < 0 || y >= length m = Nothing
    | x < 0 || x >= length row = Nothing 
    | otherwise = Just $ row !! x where
    y = fst idx
    x = snd idx
    row = m !! y

readDigits :: Matrix Char -> Matrix Int
readDigits [] = []
readDigits (r1:rest) = readDigitRow r1 : readDigits rest where
    readDigitRow r = map (digitToInt) r



findAll :: Eq t => Matrix t -> t -> [Index]
findAll matrix value = foldl (++) [] (zipWith (findInRow) matrix [0..]) where
    findInRow r i = [(i,c) | c <- findIndices (==value) r]

shape :: Matrix t -> (Int, Int)
shape [[]] = (0,0)
shape m = (length column, length m) where
    column = m !! 0

