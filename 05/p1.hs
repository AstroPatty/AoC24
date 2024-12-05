import System.IO
import Data.List
import Data.Maybe

type Rule = (Int, Int) -- First is rule it applies to
type RuleSet = [Rule]
type PageList = [Int]

isSatisfied :: Int -> RuleSet -> Bool
isSatisfied _ [] = True
isSatisfied n (r:rs) = if n == snd r then False else isSatisfied n rs

setFoundPage :: Int -> RuleSet -> RuleSet
setFoundPage i = filter (\r -> (fst r) /= i)

dropRules :: PageList -> RuleSet -> RuleSet
dropRules p = filter (\r -> fst r `elem` p)

isValidPageList :: RuleSet -> PageList -> Bool
isValidPageList rs pl = checkPageList nrs pl where
	nrs = dropRules pl rs

checkPageList :: RuleSet -> PageList -> Bool
checkPageList _ [] = True
checkPageList r (p:pl) = 
	if isSatisfied p r 
	then isValidPageList (setFoundPage p r) pl
	else False

parseRuleSet :: [String] -> [Rule]
parseRuleSet = map (\s -> parseRule s)

parseRule :: String -> Rule
parseRule s = (n1, n2) where
	vals = splitOn s '|'
	n1 = read (vals !! 0) :: Int
	n2 = read (vals !! 1) :: Int

parsePageList :: String -> PageList
parsePageList s = map (\t -> read t :: Int) vals where
	vals = splitOn s ','

parsePageLists :: [String] -> [PageList]
parsePageLists = map (\s -> parsePageList s)

splitOn :: Eq a => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn s c = sublist : splitOn remainder c where
	si = findIndex (\v -> v == c) s
	(sublist,remainder) = 
		if isJust si then (take i s, drop (i+1) s) else (s, [])
	i = fromJust si

getPagelistValue :: RuleSet -> PageList -> Int
getPagelistValue rs pl = if isvalid then value else 0 where
	isvalid = isValidPageList rs pl
	value = pl !! ((length pl - 1) `div` 2)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let d = splitOn ls ""
    let ruleset = parseRuleSet (d !! 0)
    let pagelists = parsePageLists (d !! 1)
    print $ sum (map (getPagelistValue ruleset) pagelists)



