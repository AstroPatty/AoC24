module Common (
    Robot,
    parseDigits,
    parseRobot,
    partitionRobots,
    countByQuadrant,
    getScore,
    evolve,
    evolveAll,
    evolveTo
) where

import Data.List
import Data.Maybe
import Data.Char
import Data.IntMap (IntMap, fromListWith, elems)

type Robot = ((Int, Int), (Int, Int))


parseDigits :: String -> Int -> Maybe [Int]
parseDigits s 0 = Just []
parseDigits s n = pure (:) <*> val1 <*> rest where
    signs = ['-', '+']
    digitString = dropWhile (\c -> not (isDigit c || elem c signs)) s
    (xd, remainder) = span (\c -> isDigit c || elem c signs ) digitString
    (val1,rest) = if length xd /= 0 then (Just (read xd :: Int), parseDigits remainder (n-1)) else (Nothing, Nothing)

parseRobot :: String -> Robot
parseRobot s = ((x,y), (vx,vy)) where
    [x,y,vx,vy] = fromJust $ parseDigits s 4

evolveTo :: [Robot] -> (Int, Int) -> ([Robot] -> Bool) -> (Int, [Robot])
evolveTo rs size predicate = if cond then (1, newrobots) else (1+n, evolvedrobots) where
    newrobots = evolveAll rs size 1
    cond = predicate newrobots
    (n,evolvedrobots) = evolveTo newrobots size predicate

evolveAll :: [Robot] -> (Int, Int) -> Int -> [Robot]
evolveAll rs size steps = map (\r -> evolve r size steps) rs

evolve :: Robot -> (Int, Int) -> Int -> Robot
evolve ((x,y), (vx,vy)) (xwidth, ywidth) nsteps = ((newx, newy), (vx,vy)) where
    newx = regularize (x + vx*nsteps) xwidth
    newy = regularize (y + vy*nsteps) ywidth

regularize :: Int -> Int -> Int
regularize v w = if vmod >= 0 then vmod else w + vmod where
    vmod = v `mod` w

countByQuadrant :: [Robot] -> (Int,Int) -> IntMap Int
countByQuadrant rs size = fromListWith (+) $ zip counts (repeat 1) where
    counts = map fromJust . filter isJust $ partitionRobots rs size


partitionRobots :: [Robot] -> (Int, Int) -> [Maybe Int]
partitionRobots robots (xw, yw) = map (getQuadrant (xmid, ymid)) robots where
    xmid = (xw-1) `div` 2
    ymid = (yw-1) `div` 2

getQuadrant :: (Int, Int) -> Robot -> Maybe Int
getQuadrant (xm, ym) ((x,y), _) 
    | x == xm || y == ym = Nothing
    | x > xm && y > ym = Just 3
    | y > ym = Just 2
    | x > xm = Just 1
    | otherwise = Just 0

getScore :: [Robot] -> (Int,Int) -> Int
getScore rs size = if length quads == 4 then foldr (*) 1 quads else 0 where
    quads = countByQuadrant rs size
