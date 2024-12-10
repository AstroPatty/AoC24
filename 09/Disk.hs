module Disk (
    Block,
    Disk,
    hasSpace,
    readDisk,
    reorg,
    printIndex,
    reorgFiles,
    pack,
    findInsertionPoint,
    insertFile,
    getScore,
    getMapScore,
    getFileIndex,
) where

import Data.Char
import Data.List
import Data.Maybe

type Block = Maybe Int
type Disk = [Block]
type File = (Int, Int)
type FileIndex = (Int, Int, Int)

startsAt :: FileIndex -> Int
startsAt (i, _, _) = i

len :: FileIndex -> Int
len (_,l,_) = l

val :: FileIndex -> Int
val (_, _, v) = v

nBlocks :: File -> Int
nBlocks (_, n) = n


takeFile :: Int -> Char -> [Block]
takeFile i c = [Just i | _ <- [0..n-1]] where
    n = digitToInt c

takeFree :: Char -> [Block]
takeFree c = [Nothing | _ <- [0..n-1]] where
    n = digitToInt c 

readDisk :: Int -> String -> Disk
readDisk _ [] = []
readDisk i (c:[]) = takeFile i c
readDisk i (c1:c2:rest) = takeFile i c1 ++ takeFree c2 ++ readDisk (i+1) rest

printIndex :: [FileIndex] -> String
printIndex [last] = replicate n value where
    n = len last
    value = intToDigit $ val last
printIndex (first:rest) = (replicate n value) ++ replicate space '.' ++ printIndex rest where
    n = len first
    value = intToDigit $ val first
    space = startsAt (head rest) - startsAt first - n

getFileIndex :: Disk -> Int -> [FileIndex]
getFileIndex [] _ = []
getFileIndex disk@(f:rs) i
    | f == Nothing = getFileIndex rs (i+1)
    | otherwise = (i, fileLength, value) : getFileIndex rest (i+fileLength) where
    (fb,rest) = span (==f) disk
    value = fromJust (fb !! 0)
    fileLength = length fb

hasSpace :: Disk -> File -> Bool
hasSpace d f
    | nfree >= nBlocks f  = True
    | otherwise = nfree == length d where
    nfree = length $ takeWhile (==Nothing) d

fileBlocks :: Disk -> Disk
fileBlocks = filter (isJust)

pack :: FileIndex -> FileIndex -> FileIndex
pack f1 f2 =  (startsAt f1 + len f1, len f2, val f2)

spaceBetween :: FileIndex -> FileIndex -> Int
spaceBetween f1 f2 = startsAt f2 - startsAt f1 - len f1

reorgFiles :: [FileIndex] -> [FileIndex]
reorgFiles fi = reorgFiles_ fi (reverse fi) 

reorgFiles_ :: [FileIndex] -> [FileIndex] -> [FileIndex]
reorgFiles_ forwards [] = forwards
reorgFiles_ forwards (file:backwards)
    | isNothing insertPoint = reorgFiles_ forwards backwards
    | otherwise = reorgFiles_ newForward backwards where
    insertPoint = findInsertionPoint forwards file 1
    ip = fromJust insertPoint
    (head, tail_) = splitAt ip forwards
    (tail1, tail2) = break (== file) tail_
    newForward = head ++ (newFile : tail1) ++ finalTail
    finalTail = if length tail2 == 0 then [] else tail tail2
    newFile = pack (last head) file

findInsertionPoint :: [FileIndex] -> FileIndex -> Int -> Maybe Int
findInsertionPoint ls@(first:rest) file idx
    | file == first = Nothing
    | hasSpace = Just idx
    | otherwise = findInsertionPoint rest file (idx+1) where
    freeSpace = startsAt (head rest) - startsAt first - len first
    hasSpace = len file <= freeSpace 

insertFile :: [FileIndex] -> FileIndex -> [FileIndex]
insertFile ls@(first:rest) file
    | first == file = ls
    | hasSpace = first : packed : (delete file rest)
    | otherwise = first : insertFile rest file where
    hasSpace = length rest == 0 || spaceBetween first (head rest) >= len file 
    packed = pack first file



reorg :: Disk -> Disk
reorg [] = []
reorg d = take (length files) (reorg_ d files) where
    files = fileBlocks d

reorg_ :: Disk -> Disk -> Disk
reorg_ [] _ = []
reorg_ _ [] = []
reorg_ (b:rest) fs
    | isJust b = b: reorg_ rest fs
    | otherwise = item: reorg_ rest rfs where
    (rfs, item) = (init fs, last fs)



getMapScore :: [FileIndex] -> Int
getMapScore fi = sum $ map (getSingleScore) fi

getSingleScore :: FileIndex -> Int
getSingleScore fm = sum [i*v | i <- [s..s+l-1]] where 
    (s, l, v) = fm 

getScore :: Disk -> Int 
getScore d = foldr (+) 0 $ zipWith getScore_ [0..length nd] nd where
    nd = reorg d

getScore_ :: Int -> Maybe Int -> Int
getScore_ _ Nothing = 0
getScore_ i item
    | isJust item = (fromJust item) * i
    | otherwise = 0




