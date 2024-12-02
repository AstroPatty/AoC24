import System.IO

parseRow :: String -> [Int]
parseRow "" = []
parseRow s = parseNums (words s)

parseNums :: [String] -> [Int]
parseNums [] = []
parseNums (f : r) =  (read f :: Int) : parseNums r

isSafe :: Bool -> [Int] -> Bool
isSafe _ [] = True
isSafe _ [_] = True
isSafe isIncreasing ( h : ls )
	| diff > 0 && diff < 4 = isSafe isIncreasing ls
	| otherwise = False where
		diff = if isIncreasing then (head ls - h) else (h - head ls)

checkRow :: [Int] -> Bool
checkRow l@(x:y:_) = isSafe (y > x) l

getRowValue :: String -> Int
getRowValue s = if checkRow nums then 1 else 0 where
	nums = parseRow s

main = do
	handle <- openFile "input.txt" ReadMode
	contents <- hGetContents handle
	let rows = lines contents
	let result = sum (map getRowValue rows)
	print result
