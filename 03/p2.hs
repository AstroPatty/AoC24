import System.IO
import Data.List
import Data.Char
import Data.Maybe

startsMul :: String -> (Bool, Int) -- Bool is whether it is a match, int is how many characters were required to determine this
startsMul s = (isMatch, if isMatch then 4 else n + 1) where
    n = nMatches "mul(" s
    isMatch = n == 4

nMatches :: String -> String -> Int
nMatches "" _ = 0
nMatches _ "" = 0
nMatches (h1:s1) (h2:s2) = if h1 == h2 then (1 + (nMatches s1 s2)) else 0

parseMul :: String -> (Int, Int) -- First int is value of multiplication, second is number of characters read
parseMul [] = (0, 0) --By the time we get here, we already know the string starts with mul(
parseMul s
    | isNothing pair = (0, 4 + nRead) 
    | hasParen = (n1 * n2, 4 + nRead)
    | not hasParen = (0, 5 + nRead) where
    res = readPair (drop 4 s)
    pair = fst res
    nRead = snd res
    vals = fromJust pair
    hasParen = hasClosingParen s vals
    n1 = read (fst vals) :: Int
    n2 = read (snd vals) :: Int

hasClosingParen :: String -> (String, String) -> Bool
hasClosingParen s (n1, n2) = s !! idx == ')' where
    idx = 5 + length n1 + length n2

readPair :: String -> (Maybe (String, String), Int)
readPair s
    | l1 > 0 && l2 > 0 && hasComma = (Just (n1, n2), l1 + l2 + 1)
    | l1 > 0 && hasComma = (Nothing, l1 + 1)
    | otherwise = (Nothing, l1) where
    n1 = getDigits s
    l1 = length n1
    hasComma = s !! l1 == ','
    n2 = if hasComma && l1 > 0 then getDigits (drop (l1 + 1) s) else ""
    l2 = length n2

getDigits :: String -> String
getDigits "" = ""
getDigits (s:r)
    | isDigit s = s : getDigits r
    | otherwise = []

getValue :: String -> (Int, Int) -- First int is value of multiplication, second is number of characters read
getValue s
    | not $ fst maybeMul = (0, snd maybeMul)
    | otherwise = parseMul s where
    maybeMul = startsMul s
    
getLineValues :: String -> [Int]
getLineValues "" = []
getLineValues s = val : getLineValues (drop nRead s) where
    (val, nRead) = getValue s

getLineSum :: String -> Int
getLineSum "" = 0
getLineSum s = sum $ getLineValues s

hasEnablePrefix :: String -> (Bool, Int) --
hasEnablePrefix s = (isMatch, if isMatch then n else n+1) where
    n = nMatches "do()" s
    isMatch = n == 4

hasDisablePrefix :: String -> (Bool, Int) --
hasDisablePrefix s = (isMatch, if isMatch then n else n+1) where
    n = nMatches "don't()" s
    isMatch = n == 7

getStringToFlip :: String -> Bool -> String -- Get the string up to the next state flip
getStringToFlip "" _ = ""
getStringToFlip s isEnabled = sub where
    preMatch = if isEnabled then hasDisablePrefix s else hasEnablePrefix s
    hasPre = fst preMatch
    nRead = snd preMatch
    sub = if hasPre then "" else take nRead s ++ getStringToFlip (drop nRead s) isEnabled 

getEnabledStrings :: String -> Bool -> [String]
getEnabledStrings "" _ = []
getEnabledStrings s isEnabled
    | isEnabled = substring : rest
    | otherwise = rest where 
    sub = getStringToFlip s isEnabled
    substring = if isEnabled then sub else ""
    remainder = drop (length sub) s
    rest = getEnabledStrings remainder (not isEnabled)

getAllEnabledStrings :: [String] -> [String]
getAllEnabledStrings strs = getEnabledStrings (unwords strs) True

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let stf = getAllEnabledStrings ls
    let value = sum $ map getLineSum stf
    print value
