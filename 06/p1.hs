import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import System.IO

type Map t = [[t]]
type Index = (Int, Int)

data Status = Passed | Missed deriving Show
data Floor = Clear | Blocked deriving (Eq, Show)
data Direction = Up | Right | Down | Left deriving (Enum, Eq, Show)

type FloorMap = Map Floor

move :: Index -> Direction -> Index
move (y,x) d = case d of
    Up -> (y-1, x)
    Down -> (y+1,x)
    Right -> (y, x+1)
    Left -> (y, x-1)

turnRight :: Direction -> Direction
turnRight d = if d == Left then Up else succ d

parseMapRow :: String -> [Floor]
parseMapRow = map (\c -> if c == '#' then Blocked else Clear)

parseMap :: [String] -> Map Floor
parseMap = map parseMapRow


findValue :: Eq t => Map t -> t -> Maybe Index
findValue [] _ = Nothing
findValue m v = if isJust col then Just (fromJust col, fromJust row) else Nothing where
    rows = map (findInRow v) m
    col = findIndex (isJust) rows
    row = (rows !! fromJust col)

findInRow :: Eq t => t -> [t] -> Maybe Int
findInRow v = findIndex (==v)


get :: Map t -> Index -> Maybe t
get m idx
    | y < 0 || y >= length m = Nothing
    | x < 0 || x >= length row = Nothing 
    | otherwise = Just $ row !! x where
    y = fst idx
    x = snd idx
    row = m !! y

set :: Map t -> Index -> t -> Map t
set m idx val = if isJust curval then newmap else m where
   curval = get m idx
   newmap = hd ++ nr:tl
   nr = setInRow cr (snd idx) val
   (hd,cr:tl) = splitAt (fst idx) m

setInRow :: [t] -> Int -> t -> [t]
setInRow l i v =  hd ++ v:tl where
    (hd,_:tl) = splitAt i l

getStep :: Map Floor -> Index -> Direction -> Direction
getStep f idx d
    | nx == Blocked = getStep f idx (turnRight d)
    | otherwise = d where
    next = get f (move idx d)
    nx = if isJust next then fromJust next else Clear

step :: FloorMap -> Index -> Direction -> (Index, Direction)
step f idx d = (nidx, nd) where
   nd = getStep f idx d
   nidx = move idx nd

walk :: FloorMap -> Index -> Direction -> [Index]
walk f idx dir 
    | isNothing (get f idx) = []
    | otherwise = idx : walk f nidx ndir where
    (nidx, ndir) = step f idx dir
    




main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let map = parseMap $ ls
    let start = fromJust $ findValue ls '^'
    let path = walk map start Up
    let squares = nub path
    print $ length squares

