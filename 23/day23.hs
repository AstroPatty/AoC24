import Control.Applicative (liftA2)
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Parser
import System.IO

type Graph = Map.Map String (Set.Set String)

-- Initialization functions
buildGraph :: [String] -> Graph
buildGraph [] = Map.empty
buildGraph (item : rest) = Map.unionWith Set.union fg $ buildGraph rest
  where
    [n1, n2] = splitBy '-' item
    fg = Map.fromList [(n1, Set.singleton n2), (n2, Set.singleton n1)]

-- Query functions
getAllVertices :: Graph -> Set.Set String
getAllVertices = Set.fromList . Map.keys

hasEdge :: String -> String -> Graph -> Bool
hasEdge n1 n2 graph = fromMaybe False e1has
  where
    e1has = (n2 `elem`) <$> Map.lookup n1 graph

getAllEdges :: Graph -> Set.Set (String, String)
getAllEdges graph = foldl Set.union Set.empty $ Map.mapWithKey getEdges graph
  where
    getEdges = \k v -> Set.fromList $ zip (repeat k) (Set.toList v)

getNeighbors :: String -> Graph -> Maybe (Set.Set String)
getNeighbors = Map.lookup

getCommonNeighbors :: String -> String -> Graph -> Maybe (Set.Set String)
getCommonNeighbors s1 s2 graph = liftA2 Set.intersection (getNeighbors s1 graph) (getNeighbors s2 graph)

countTrianglesWith :: Graph -> Char -> Int
countTrianglesWith g c = Set.size $ Set.filter (hasStart c) $ getAllTriangles g

getTriangles :: String -> String -> Graph -> Set.Set (Set.Set String)
getTriangles s1 s2 graph = fromMaybe Set.empty tris
  where
    neighbors = getCommonNeighbors s1 s2 graph
    start = Set.fromList [s1, s2]
    tris = Set.map (`Set.insert` start) <$> neighbors

getAllTriangles :: Graph -> Set.Set (Set.Set String)
getAllTriangles g = Set.foldl Set.union Set.empty $ Set.map gt (getAllEdges g)
  where
    gt (n1, n2) = getTriangles n1 n2 g

hasStart :: Char -> Set.Set String -> Bool
hasStart c tri = any (\s -> head s == c) $ Set.toList tri

isClique :: Graph -> Set.Set String -> Bool
isClique graph vertices = Set.foldl (\b v -> b && isConnectedToAll v) True vertices
  where
    isConnectedToAll v = fromMaybe False isConnected
      where
        neighbors = Set.difference vertices <$> getNeighbors v graph
        isConnected = (== Set.singleton v) <$> neighbors

bronKerbosch :: Graph -> Set.Set String -> Set.Set String -> Set.Set String -> [Set.Set String] -> [Set.Set String]
bronKerbosch g clique queue checked found
  | Set.null queue && Set.null checked = clique : found
  | null queue = found
  | otherwise = others
  where
    next = head $ Set.toList queue
    neighbors = fromJust $ getNeighbors next g
    rest = bronKerbosch g (Set.insert next clique) (Set.intersection queue neighbors) (Set.intersection queue neighbors) found
    others = bronKerbosch g clique (Set.delete next queue) (Set.insert next checked) rest

getMaximalClique graph = cliques !! idx
  where
    cliques = bronKerbosch graph Set.empty (getAllVertices graph) Set.empty []
    sizes = map Set.size cliques
    idx = fromJust $ elemIndex (maximum sizes) sizes

getPassword graph = intercalate "," $ Set.toList $ getMaximalClique graph

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let connections = lines contents
  let g = buildGraph connections
  let ntriangles = countTrianglesWith g 't'
  let password = getPassword g
  print ntriangles
  print password
