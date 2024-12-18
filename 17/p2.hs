
import Data.Bits
import Data.Maybe
import Data.List 
import Parser
import Prelude hiding (length, drop)
import qualified Data.IntMap as IM
import System.IO

type Registers = IM.IntMap Int
type Instructions = [Int]
type Pointer = Int

type ProgramState = (Registers, Instructions, Pointer)
type Operation = (ProgramState -> (Maybe Int, ProgramState))

get :: Registers -> Char -> Int
get registars char
    | isJust val = fromJust val
    | otherwise = 0 where
    val = IM.lookup (fromEnum char) registars

set :: Registers -> Char -> Int -> Registers
set registers char newval = IM.insert (fromEnum char) newval registers

getCombo :: ProgramState -> Int
getCombo (registers, instructions, pointer)
    | val <= 3 = val
    | val == 4 = get registers 'A'
    | val == 5 = get registers 'B'
    | val == 6 = get registers 'C' where
    val = instructions !! (pointer + 1)

adv :: Operation
adv state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    num = get registers 'A'
    denom = 2 ^ (getCombo state)
    res = div num denom
    newregisters = set registers 'A' res 

bxl :: Operation
bxl (registers, instructions, pointer) = (Nothing, (newregisters, instructions, pointer+2)) where
    op1 = get registers 'B'
    op2 = instructions !! (pointer + 1)
    res = xor op1 op2
    newregisters = set registers 'B' res

bst :: Operation
bst state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    op = getCombo state
    res = mod op 8
    newregisters = set registers 'B' res

jnz :: Operation
jnz (registers, instructions, pointer) = (Nothing, (registers, instructions, newpointers)) where
    ra = get registers 'A'
    lit = instructions !! (pointer+1)
    newpointers = if ra == 0 then pointer + 2 else lit

bxc :: Operation
bxc (registers, instructions, pointer) = (Nothing, (newregisters, instructions, pointer+2)) where
    b = get registers 'B'
    c = get registers 'C'
    res = xor b c
    newregisters = set registers 'B' res

out :: Operation
out state = (Just result, (registers, instructions, pointer+2)) where
    (registers,instructions,pointer) = state
    op = getCombo state
    result = mod op 8

bdv :: Operation
bdv state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    num = get registers 'A'
    denom = 2 ^ (getCombo state)
    res = div num denom
    newregisters = set registers 'B' res

cdv :: Operation
cdv state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    num = get registers 'A'
    denom = 2 ^ (getCombo state)
    res = div num denom
    newregisters = set registers 'C' res


getOp :: Int -> Operation
getOp i = case i of
    0 -> adv
    1 -> bxl
    2 -> bst
    3 -> jnz
    4 -> bxc
    5 -> out
    6 -> bdv
    7 -> cdv

doNextOp :: ProgramState -> Maybe (Maybe Int, ProgramState)
doNextOp state = if isJust op then Just $ (fromJust op) state else Nothing where
    (_, instructions, pointer) = state
    opcode = if pointer >= length instructions then Nothing else Just (instructions !! pointer)
    op = fmap (getOp) opcode 

run :: ProgramState -> ([Int], ProgramState)
run state 
    | isNothing opEffect = ([], state)
    | isNothing output = (vals, fstate)
    | otherwise = (out: vals, fstate) where
    opEffect = doNextOp state
    (output, newstate) = fromJust opEffect
    (vals, fstate) = run newstate
    out = fromJust output

getOutputVal :: Instructions -> Int -> [Int]
getOutputVal instructions input = v where
    registers = IM.fromList [(fromEnum 'A', input), (fromEnum 'B', 0), (fromEnum 'C', 0)] 
    state = (registers, instructions, 0)
    (v, _) = run state

type Octal = [Int]
toInt :: Octal -> Int
toInt oct = sum $ map (\(val,pow) -> val * (8^pow)) (zip (reverse oct) [0..])

findNext :: Instructions -> Octal -> Maybe Octal
findNext inst curr 
    | idxToCheck < 0 = Just curr
    | length idxsToCheck == 0 = Nothing
    | isJust solution = fromJust solution
    | otherwise = Nothing where
    idxToCheck = length inst - length curr - 1
    element = inst !! idxToCheck
    outputs = map (\i -> getOutputVal inst (toInt (curr ++ [i]))) [0..7]
    idxsToCheck = findIndices (==element) $ map (head) outputs
    next = map (\i -> findNext inst (curr ++ [i])) idxsToCheck
    solution = find (isJust) next

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let [regin, pin] = splitBy "" (lines contents) 
    let registerVals = map (head . parseDigits) regin
    let registers = IM.fromList $ map (\(val,reg) -> (fromEnum reg, val)) $ zip registerVals "ABC"
    let instructions = parseDigits (head pin)
    let (p1result,_) = run (registers, instructions, 0)
    print p1result
    let p2result = findNext instructions [3]
    print $ toInt $ fromJust p2result
    
