module Common (
    Matrix,
    Index,
    (^+),
    (^*),
    Direction,
    toIndexShift,
    get,
    shape,
    flip,
    scanDirection,
    combine,
    components
) where

import Prelude hiding (Right, Left, flip)
import System.IO
import Data.List
import Data.Maybe

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
    | UpLeft
    | UpRight
    | DownLeft
    | DownRight
    deriving Enum
    
toIndexShift :: Direction -> Index
toIndexShift d = case d of
     Up -> (1,0)
     Down -> (-1,0)
     Left -> (0, -1)
     Right -> (0,1)
     UpLeft -> (1, -1)
     UpRight -> (1, 1)
     DownLeft -> (-1, -1)
     DownRight -> (-1, 1)            

components :: Direction -> [Direction]
components d = case d of
    UpRight -> [Up, Right]
    UpLeft -> [Up, Left]
    DownRight -> [Down, Right]
    DownLeft -> [Down, Left]
    _ -> [d]

combine :: Direction -> Direction -> Direction
combine d1 d2 = case (d1, d2) of
    (Up, Right) -> UpRight
    (Up, Left) -> UpLeft
    (Down, Right) -> DownRight
    (Down, Left) -> DownLeft


flip :: Direction -> Direction
flip d = case d of
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left
    UpLeft -> DownRight
    UpRight -> DownLeft
    DownLeft -> UpRight
    DownRight -> UpLeft



get :: Matrix t -> Index -> Maybe t
get m idx
    | y < 0 || y >= length m = Nothing
    | x < 0 || x >= length row = Nothing 
    | otherwise = Just $ row !! x where
    y = fst idx
    x = snd idx
    row = m !! y

shape :: Matrix t -> (Int, Int)
shape [[]] = (0,0)
shape m = (length column, length m) where
    column = m !! 0


scanDirection :: Eq t => Matrix t -> Index -> [t] -> Direction -> Bool
scanDirection _ _ [] _ = True
scanDirection m idx (item : []) _ = case (get m idx) of
    Just val -> val == item
    Nothing -> False
scanDirection m idx (item : rest) dir
    | isJust val = 
        if fromJust val == item 
        then scanDirection m (idx ^+ toIndexShift dir) rest dir
        else False
    | otherwise = False
    where val = get m idx

