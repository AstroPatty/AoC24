module Disk (
    Block,
    Disk,
    readDisk,
    reorg,
    getScore
) where

import Data.Char
import Data.List
import Data.Maybe

type Block = Maybe Int
type Disk = [Block]

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

fileBlocks :: Disk -> Disk
fileBlocks = filter (isJust)

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

getScore :: Disk -> Int
getScore d = foldr (+) 0 $ zipWith getScore_ [0..length nd] nd where
    nd = reorg d

getScore_ :: Int -> Maybe Int -> Int
getScore_ _ Nothing = 0
getScore_ i item
    | isJust item = (fromJust item) * i
    | otherwise = 0




