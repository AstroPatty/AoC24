import Prelude hiding (Right, Left)
import System.IO
import Data.List
import Data.Maybe

type Matrix t = [[t]] -- We'll use row orientation
type Index = (Int, Int)


infixl 7 ^+
(^+) :: Index -> Index -> Index
(^+) i1 i2 = (fst i1 + fst i2, snd i1 + snd i2)

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

scanDiagonals :: Eq t => Matrix t -> Index -> [t] -> [Bool]
scanDiagonals m idx s = map (\i -> scan (toEnum i::Direction)) [4..7] where
    scan = scanDirection m idx s

scanLocation :: Eq t => Matrix t -> Index -> [t] -> [Bool]
scanLocation m idx s = map (\i -> scan (toEnum i::Direction)) [0..7] where
    scan = scanDirection m idx s

getLocationScore :: Eq t => Matrix t -> Index -> [t] -> Int
getLocationScore m idx s = sum $ map (\x -> if x then 1 else 0) (scanLocation m idx s)


main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let m = lines contents
    let s = shape m
    let idxs = [(x,y) | x <- [0..fst s], y <- [0..snd s]]
    let score = sum $ map (\i -> getLocationScore m i "XMAS") idxs
    
    print score



