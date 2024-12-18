import Data.Bits
import Data.Maybe
import Data.Sequence

type Registers = (Int, Int, Int)
type Instructions = Seq Int
type Pointer = Int

type ProgramState = (Registers, Instructions, Pointer)
type Operation = (ProgramState -> (Maybe Int, ProgramState))

getA :: Registers -> Int
getA (a, _, _) = a

getB :: Registers -> Int
getB (_, b, c) = b

getC :: Registers -> Int
getC  (_, _, c) = c

setA :: Int -> Registers -> Registers
setA a (_, b, c)  = (a, b, c)

setB :: Int -> Registers -> Registers
setB b (a, _, c) = (a, b, c)

setC :: Int -> Registers -> Registers
setC c (a, b, _) = (a,b,c)

getCombo :: ProgramState -> Int
getCombo (registers, instructions, pointer)
    | val <= 3 = val
    | val == 4 = getA registers
    | val == 5 = getB registers
    | val == 6 = getC registers where
    val = index instructions (pointer + 1)

adv :: Operation
adv state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    num = getA registers
    denom = 2 ^ (getCombo state)
    res = div num denom
    newregisters = setA res registers

bxl :: Operation
bxl (registers, instructions, pointer) = (Nothing, (newregisters, instructions, pointer+2)) where
    op1 = getB registers
    op2 = index instructions (pointer + 1)
    res = xor op1 op2
    newregisters = setB res registers

bst :: Operation
bst state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    op = getCombo state
    res = mod op 8
    newregisters = setB res registers

jnz :: Operation
jnz (registers, instructions, pointer) = (Nothing, (registers, instructions, newpointers)) where
    ra = getA registers
    lit = index instructions (pointer+1)
    newpointers = if ra == 0 then pointer + 2 else lit

bxc :: Operation
bxc (registers, instructions, pointer) = (Nothing, (newregisters, instructions, pointer+2)) where
    b = getB registers
    c = getC registers
    res = xor b c
    newregisters = setB res registers

out :: Operation
out state = (Just result, (registers, instructions, pointer+2)) where
    (registers,instructions,pointer) = state
    op = getCombo state
    result = mod op 8

bdv :: Operation
bdv state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    num = getA registers
    denom = 2 ^ (getCombo state)
    res = div num denom
    newregisters = setB res registers

cdv :: Operation
cdv state = (Nothing, (newregisters, instructions, pointer+2)) where
    (registers, instructions, pointer) = state
    num = getA registers
    denom = 2 ^ (getCombo state)
    res = div num denom
    newregisters = setC res registers

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
    opcode = instructions !? pointer
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

runToOutput :: ProgramState -> (Maybe Int, ProgramState)
runToOutput state 
    | isNothing opEffect = (Nothing, state)
    | isNothing output = runToOutput newstate
    | otherwise = (output, newstate) where
    opEffect = doNextOp state
    (output, newstate) = fromJust opEffect

main :: IO()
main = do
    let registers = (37283687, 0, 0)
    let instructions = fromList [2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0]
    let pointer = 0
    let state = (registers, instructions, pointer)
    let (output, newstate) = runToOutput state
    let (out2, fstate) = runToOutput newstate
    print output
    print out2
