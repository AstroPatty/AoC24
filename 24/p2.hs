import Control.Monad.State
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import Parser
import System.IO

data Operation
  = AND
  | OR
  | XOR
  deriving (Show, Read, Enum, Eq)

type Gate = (String, String, String, Operation)

i1 :: Gate -> String
i1 (i, _, _, _) = i

i2 :: Gate -> String
i2 (_, i, _, _) = i

out :: Gate -> String
out (_, _, o, _) = o

op :: Gate -> Operation
op (_, _, _, op) = op

parseGates :: [String] -> [Gate]
parseGates = map parseGate

parseGate :: String -> Gate
parseGate s = (w1, w2, out, opf)
  where
    (w1 : op : w2 : _ : out : _) = splitBy ' ' s
    opf = read op :: Operation

getOutputWire :: [Gate] -> String -> Operation -> String -> Maybe String
getOutputWire gates w1 operation w2 = fmap out entry
  where
    entry = find (\(i1, i2, out, op) -> op == operation && ((i1, i2) == (w1, w2) || (i2, i1) == (w1, w2))) gates

findByWireOp :: [Gate] -> String -> Operation -> Maybe Gate
findByWireOp gates wire operation = find (\(i1, i2, _, op) -> op == operation && (i1 == wire || i2 == wire)) gates

check :: [Gate] -> Int -> String -> Set.Set String
check _ 45 _ = Set.empty
check gates num cin = Set.union missing $ check gates (num + 1) cout
  where
    (cout, missing) = checkAdder gates num cin

checkAdder :: [Gate] -> Int -> String -> (String, Set.Set String)
checkAdder gates num cin = (cout, foldl Set.union Set.empty [m1, m2, m3])
  where
    xname = if num < 10 then "x0" ++ show num else "x" ++ show num
    yname = if num < 10 then "y0" ++ show num else "y" ++ show num
    zname = if num < 10 then "z0" ++ show num else "z" ++ show num
    xor1out = fromJust $ getOutputWire gates xname XOR yname
    and1out = fromJust $ getOutputWire gates xname AND yname
    m1 = checkSumOutputWires gates xor1out cin zname
    (orin, m2) = checkAnd gates cin xor1out
    (cout, m3) = checkOr gates and1out orin

checkSumOutputWires :: [Gate] -> String -> String -> String -> Set.Set String
checkSumOutputWires gates xor1out cin expout = output
  where
    xorout = getOutputWire gates xor1out XOR cin
    cinxor = findByWireOp gates cin XOR
    xor1or = findByWireOp gates xor1out XOR
    output
      | fromMaybe "none" xorout == expout = Set.empty
      | isJust xorout = Set.singleton $ out $ fromJust xor1or
      | isJust xor1or = Set.singleton cin
      | isJust cinxor = Set.singleton xor1out

checkAnd :: [Gate] -> String -> String -> (String, Set.Set String)
checkAnd gates cin xor1out = output
  where
    andout = getOutputWire gates cin AND xor1out
    cinand = findByWireOp gates cin AND
    xoroand = findByWireOp gates xor1out AND
    output
      | isJust andout = (fromJust andout, Set.empty)
      | isJust xoroand = (out $ fromJust xoroand, Set.singleton cin)
      | isJust cinand = (out $ fromJust cinand, Set.singleton xor1out)

checkOr :: [Gate] -> String -> String -> (String, Set.Set String)
checkOr gates and1out and2out = output
  where
    orout = getOutputWire gates and1out OR and2out
    a1out = findByWireOp gates and1out OR
    a2out = findByWireOp gates and2out OR
    output
      | isJust orout = (fromJust orout, Set.empty)
      | isJust a1out = (out $ fromJust a1out, Set.singleton and2out)
      | isJust a2out = (out $ fromJust a2out, Set.singleton and1out)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let [_, g] = splitBy "" ls
  let gates = parseGates g
  let cin = fromJust $ getOutputWire gates "x00" AND "y00"
  print $ intercalate "," $ Set.toList $ check gates 1 cin
