module Common (
    canCombine,
    concatInts,
    parseLine,
    getLineValue
    ) where

canCombine :: (Num t, Ord t) => [t -> t -> t] -> [t] -> t -> t -> Bool
canCombine _ [] acc target = (acc == target)
canCombine ops (n:rest) 0 target = canCombine ops rest n target
canCombine ops (n:rest) acc target = 
    any (\op -> canCombine ops rest ((op) acc n) target) ops

powten :: Int -> Int
powten 0 = 1
powten i = 10 * powten (i `div` 10)

concatInts :: Int -> Int -> Int
concatInts i1 i2 = i1 * (powten i2) + i2

parseLine :: String -> (Int, [Int])
parseLine s = (read sum :: Int, map read rest :: [Int]) where
    (leader: rest) = words s
    sum = takeWhile (/=':') leader

getLineValue :: [Int -> Int -> Int] -> String -> Int
getLineValue ops s =
    if canCombine ops nums 0 sum_
    then sum_
    else 0 where
    (sum_, nums) = parseLine s

