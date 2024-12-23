import Control.Monad.State
import Data.Bits
import Data.IntMap qualified as IM
import Data.List
import Data.Maybe
import Debug.Trace
import Parser
import System.IO

type Registers = IM.IntMap Int

type Instructions = [Int]

type Pointer = Int

type ProgramState = (Registers, Instructions, Pointer)

type Program = State ProgramState

getRegisterVal :: Registers -> Char -> Int
getRegisterVal registars char
  | isJust val = fromJust val
  | otherwise = 0
  where
    val = IM.lookup (fromEnum char) registars

set :: Registers -> Char -> Int -> Registers
set registers char newval = IM.insert (fromEnum char) newval registers

getCombo :: ProgramState -> Int
getCombo (registers, instructions, pointer)
  | val <= 3 = val
  | val == 4 = getRegisterVal registers 'A'
  | val == 5 = getRegisterVal registers 'B'
  | val == 6 = getRegisterVal registers 'C'
  where
    val = instructions !! (pointer + 1)

execOp :: (ProgramState -> ProgramState) -> Program ()
execOp f = modify f

adv :: Program ()
adv = execOp $ \(registers, instructions, pointer) ->
  let num = getRegisterVal registers 'A'
      denom = 2 ^ getCombo (registers, instructions, pointer)
      res = div num denom
      newRegisters = set registers 'A' res
   in (newRegisters, instructions, pointer + 2)

bxl :: Program ()
bxl = execOp $ \(registers, instructions, pointer) ->
  let op1 = getRegisterVal registers 'B'
      op2 = instructions !! (pointer + 1)
      res = xor op1 op2
      newRegisters = set registers 'B' res
   in (newRegisters, instructions, pointer + 2)

bst :: Program ()
bst = execOp $ \(registers, instructions, pointer) ->
  let op = getCombo (registers, instructions, pointer)
      res = mod op 8
      newRegisters = set registers 'B' res
   in (newRegisters, instructions, pointer + 2)

jnz :: Program ()
jnz = execOp $ \(registers, instructions, pointer) ->
  let ra = getRegisterVal registers 'A'
      lit = instructions !! (pointer + 1)
      newPointer = if ra == 0 then pointer + 2 else lit
   in (registers, instructions, newPointer)

bxc :: Program ()
bxc = execOp $ \(registers, instructions, pointer) ->
  let b = getRegisterVal registers 'B'
      c = getRegisterVal registers 'C'
      res = xor b c
      newRegisters = set registers 'B' res
   in (newRegisters, instructions, pointer + 2)

out :: Program (Maybe Int)
out = state $ \(registers, instructions, pointer) ->
  let op = getCombo (registers, instructions, pointer)
      result = mod op 8
   in (Just result, (registers, instructions, pointer + 2))

bdv :: Program ()
bdv = execOp $ \(registers, instructions, pointer) ->
  let num = getRegisterVal registers 'A'
      denom = 2 ^ getCombo (registers, instructions, pointer)
      res = div num denom
      newRegisters = set registers 'B' res
   in (newRegisters, instructions, pointer + 2)

cdv :: Program ()
cdv = execOp $ \(registers, instructions, pointer) ->
  let num = getRegisterVal registers 'A'
      denom = 2 ^ getCombo (registers, instructions, pointer)
      res = div num denom
      newRegisters = set registers 'C' res
   in (newRegisters, instructions, pointer + 2)

getOp :: Int -> Program (Maybe Int)
getOp i = case i of
  0 -> adv >> return Nothing
  1 -> bxl >> return Nothing
  2 -> bst >> return Nothing
  3 -> jnz >> return Nothing
  4 -> bxc >> return Nothing
  5 -> out
  6 -> bdv >> return Nothing
  7 -> cdv >> return Nothing

doNextOp :: Program (Maybe Int)
doNextOp = do
  (registers, instructions, pointer) <- get
  let opcode = if pointer >= length instructions then Nothing else Just (instructions !! pointer)
  case opcode of
    Just op -> getOp op
    Nothing -> return Nothing

run :: Program [Int]
run = do
  (_, instructions, pointer) <- get
  result <- doNextOp
  case result of
    Nothing -> if pointer >= (length instructions) then return [] else run
    Just output -> (output :) <$> run

type Octal = [Int]

getOutputVal :: Instructions -> Int -> [Int]
getOutputVal instructions input = v
  where
    registers = IM.fromList [(fromEnum 'A', input), (fromEnum 'B', 0), (fromEnum 'C', 0)]
    state = (registers, instructions, 0)
    (v, _) = runState run state

findNext :: Instructions -> Octal -> Maybe Octal
findNext inst curr
  | idxToCheck < 0 = Just curr
  | length idxsToCheck == 0 = Nothing
  | isJust solution = fromJust solution
  | otherwise = Nothing
  where
    idxToCheck = length inst - length curr - 1
    element = inst !! idxToCheck
    outputs = map (\i -> getOutputVal inst (toInt (curr ++ [i]))) [0 .. 7]
    idxsToCheck = findIndices (== element) $ map (head) outputs
    next = map (\i -> findNext inst (curr ++ [i])) idxsToCheck
    solution = find (isJust) next

toInt :: Octal -> Int
toInt oct = sum $ map (\(val, pow) -> val * (8 ^ pow)) (zip (reverse oct) [0 ..])

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let [regin, pin] = splitBy "" (lines contents)
  let registerVals = map (head . parseDigits) regin
  let registers = IM.fromList $ map (\(val, reg) -> (fromEnum reg, val)) $ zip registerVals "ABC"
  let instructions = parseDigits (head pin)
  let (p1result, _) = runState run (registers, instructions, 0)
  print p1result
  let p2result = findNext instructions [3]
  print $ toInt $ fromJust p2result
