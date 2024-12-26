import Control.Monad.State
import Data.Map qualified as Map
import Parser
import System.IO

type Wires = Map.Map String Bool

type WireStates = State Wires

updateWireState :: (Bool -> Bool -> Bool) -> String -> String -> String -> WireStates Bool
updateWireState f w1 w2 target = do
  state <- get
  let s1 = Map.lookup w1 state
  let s2 = Map.lookup w2 state
  let ns = liftM2 f s1 s2
  case ns of
    Just newState -> do
      put $ Map.insert target newState state
      return True
    Nothing -> return False

updateWireStates :: Wires -> [WireStates Bool] -> Wires
updateWireStates wires [] = wires
updateWireStates wires updates = updateWireStates newState stillToRun
  where
    (newState, updated) = foldr applyUpdate (wires, []) updates
    stillToRun = map snd $ filter (not . fst) $ zip updated updates
    applyUpdate update (currentState, results) =
      let (result, newState) = runState update currentState
       in (newState, result : results)

wor = (||)

wand = (&&)

wxor = (/=)

parseWireStates :: [String] -> Wires
parseWireStates = foldr (Map.union . parseWire) Map.empty

parseWire :: String -> Wires
parseWire wire = Map.singleton name (state == 1)
  where
    [name, s] = splitBy ':' wire
    state = read (tail s) :: Int

parseCombinations :: [String] -> [WireStates Bool]
parseCombinations = map parseCombination

parseCombination :: String -> WireStates Bool
parseCombination s = f
  where
    [w1, op, w2, _, out] = splitBy ' ' s
    opf = getOp op
    f = updateWireState opf w1 w2 out

getOp :: String -> (Bool -> Bool -> Bool)
getOp s = case s of
  "XOR" -> wxor
  "AND" -> wand
  "OR" -> wor

getWiresWithStart :: Wires -> Char -> Wires
getWiresWithStart wires start = Map.filterWithKey (\k _ -> head k == start) wires

getValue :: Wires -> Int
getValue wires = sum $ map (\(i, b) -> if b then 2 ^ i else 0) $ zip [0 ..] (Map.elems wires)

getOutput :: Wires -> [WireStates Bool] -> Int
getOutput wires updates = getValue $ getWiresWithStart (updateWireStates wires updates) 'z'

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let [starts, ops] = splitBy "" ls
  let startState = parseWireStates starts
  let combinations = parseCombinations ops
  print $ getOutput startState combinations
