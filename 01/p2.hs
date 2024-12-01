import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Data.Char
import qualified Data.Map as Map

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

nHeadRepeats :: (Eq t) => [t] -> Int
nHeadRepeats [] = 0
nHeadRepeats [_] = 1
nHeadRepeats (x:rest)
	| x == head rest = 1 + nHeadRepeats rest
	| otherwise = 1

nOccurrences :: (Ord t) => [t] -> Map.Map t Int
nOccurrences [] = Map.empty
nOccurrences [x] = Map.singleton x 1
nOccurrences l = Map.insert lh n rest where
	lh = head l
	n = nHeadRepeats l
	rest = nOccurrences (drop n l)

getScore :: Map.Map Int Int -> Map.Map Int Int -> Int
getScore m1 m2 = sum (Map.keys a)
	where a = Map.mapKeys (getValueScore m1 m2) m1

getValueScore :: Map.Map Int Int -> Map.Map Int Int -> Int -> Int
getValueScore map1 map2 val = case lookups of
	(Just count1, Just count2) -> count1 * count2 * val
	(_, _) -> 0 
	where
		lookups = (Map.lookup val map1, Map.lookup val map2)

getTotalValueScore :: ([Int], [Int]) -> Int
getTotalValueScore (l1, l2) = getScore m1 m2 where
	m1 = nOccurrences (quickSort l1)
	m2 = nOccurrences (quickSort l2)

main :: IO()
main = do
	contents <- TextIO.readFile "input.txt"
	let rows = Text.lines contents
	let vals = parseArrs rows
	let total = getTotalValueScore vals
	print total
