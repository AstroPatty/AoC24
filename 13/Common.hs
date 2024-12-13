module Common (
    Machine,
    parseMachines,
    getMachineCost
) where

import Data.Maybe
import Data.Char

type Target = (Int, Int)
type Button = (Int, Int, Int)
type Machine = ((Button, Button), Target)

ba :: Machine -> Button
ba ((b,_), _) = b

bb :: Machine -> Button
bb ((_,b), _) = b

target :: Machine -> Target
target ((_,_), t) = t

xcoord :: Target -> Int
xcoord (x,_) = x

ycoord :: Target -> Int
ycoord (_,y) = y

dx :: Button -> Int
dx (x,_,_) = x

dy :: Button -> Int
dy (_,y,_) = y

type Matrix t = ((t,t), (t,t))

cost :: Button -> Int
cost (_,_,cost) = cost


getMachineCost :: Machine -> Maybe Int
getMachineCost m = if isJust sol then Just (3*a1 + a2) else Nothing where
    sol = getMachineSolution m
    (a1, a2) = fromJust sol

getMachineSolution :: Machine -> Maybe (Int, Int)
getMachineSolution m = if isValid then Just (i1, i2) else Nothing where
    intersects = getMachineIntersects m
    (a1,a2) = if isJust intersects then fromJust intersects else (-1.0,-1.0)
    (i1,i2) = (round a1, round a2)
    (xt, yt) = target m
    (x1, y1, _) = ba m
    (x2, y2, _) = bb m
    xintersects = x1*i1 + x2*i2 == xt
    yintersects = y1*i1 + y2*i2 == yt
    isValid = xintersects && yintersects
    

getMachineIntersects :: Machine -> Maybe (Double, Double)
getMachineIntersects m = if isNothing inverse then Nothing else Just (a1,a2) where
    inverse = invertMachine m
    ((x1,x2), (y1,y2)) = fromJust inverse
    (t1, t2) = target m
    a1 = x1*(fromIntegral t1) + x2*(fromIntegral t2)
    a2 = y1*(fromIntegral t1) + y2*(fromIntegral t2)
    

invertMachine :: Machine -> Maybe (Matrix Double)
invertMachine ((b1, b2), _) = invert ((x1,x2), (y1,y2)) where
    (x1,y1,_) = b1
    (x2,y2,_) = b2

invert :: Matrix Int -> Maybe (Matrix Double)
invert ((a,b), (c,d)) = if det == 0.0 then Nothing else Just ((e,f), (g,h)) where
    det = fromIntegral (a*d - b*c)
    e = fromIntegral d / det
    f = fromIntegral b / (-1.0*det)
    g = fromIntegral c / (-1.0*det)
    h = fromIntegral a / det



splitBy :: Eq t => t -> [t] -> [[t]]
splitBy _ [] = []
splitBy value vals = first : rest where
    (first, rest_) = break (==value) vals
    rest = if length rest_ == 0 then [] else splitBy value $ tail rest_

parseMachines :: String -> [Machine]
parseMachines = map (parseMachine) . splitBy "" . lines

parseMachine :: [String] -> Machine
parseMachine s = ((b1, b2), t) where
    b1 = parseButton (s !! 0) 3
    b2 = parseButton (s !! 1) 1
    t = parseDigits (s !! 2)

parseButton :: String -> Int -> Button
parseButton s val = (x, y, val) where
    (x,y) = parseDigits s

parseDigits :: String -> (Int, Int)
parseDigits s = (x,y) where 
    digitString = dropWhile (not . isDigit) s
    (xd,rest) = span (isDigit) digitString
    yd = takeWhile isDigit . dropWhile (not . isDigit) $ rest
    x = read xd :: Int
    y = read yd :: Int

