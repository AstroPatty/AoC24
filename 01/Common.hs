module Common 
( getLineValues
, parseLine
, parseArrs
, quickSort
, getDiff
, getTotalDiff
) where

import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Data.Char

getLineValues :: [String] -> (Int, Int)
getLineValues (x:y:[]) = (read x, read y)


parseLine :: Text.Text -> (Int, Int)
parseLine s = getLineValues t where
	t = words ( Text.unpack s)

parseArrs :: [Text.Text] -> ([Int], [Int])
parseArrs t = unzip pairs where
	pairs = map parseLine t

quickSort :: (Ord t) => [t] -> [t]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = headSort ++ [x] ++ tailSort  where
	headSort = quickSort (filter (<=x) xs)
	tailSort = quickSort (filter (>x) xs)

getDiff :: (Num t, Ord t) => ([t], [t]) -> [t]
getDiff ([], _) = []
getDiff (_, []) = []
getDiff ([x], [y])
	| x >= y = [x - y]
	| otherwise = [y - x]

getDiff ((x:xs), (y:ys)) = diff : getDiff((xs, ys)) where
	diff 
		| x >= y = x - y
		| otherwise = y - x

getTotalDiff :: (Num t, Ord t) => ([t], [t]) -> t
getTotalDiff (x, y) = sum (getDiff (xs, ys)) where
	xs = quickSort x
	ys = quickSort y

