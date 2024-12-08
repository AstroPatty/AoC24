module Index (
   Index,
   (^+),
   (^-),
   inGrid,
   reduce
) where

type Index = (Int, Int)

infixl 7 ^+
(^+) :: Index -> Index -> Index
(^+) (y1,x1) (y2,x2) = (y1+y2, x1+x2)

infixl 7 ^-
(^-) :: Index -> Index -> Index
(^-) (y1,x1) (y2,x2) = (y1-y2, x1-x2)

inGrid :: Index -> Index -> Bool
inGrid (yb, xb) (y,x) = yi && xi where
    yi = y >= 0 && y < yb
    xi = x >= 0 && x < xb

reduce :: Index -> Index
reduce (y, x)
    | y == x = (1,1)
    | even x && even y = reduce (div y 2, div x 2)
    | otherwise = (y,x)

