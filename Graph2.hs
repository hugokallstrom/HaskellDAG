-- Graph representation

module Graph2
(Graph(Graph)
, create_graph
, add_vertex
, add_edge
, topological_ordering
, weight_of_longest_path
, selectNodes
, getIdentifiers
, removeEdges
, edgesToRemove
, findNeighbours
) where

import Data.Char
import qualified Data.Set as Set
import Data.List

-- Datatypes
type Vertex w = (Char, w)
type Edge w   = (Char, Char, w)
data Graph w = Graph {vertices :: [Vertex w],
                      edges :: [Edge w]} deriving (Show, Eq, Read)

-- Creates an empty graph.
create_graph :: Graph w -> Graph w
create_graph emptyGraph = Graph [] []

-- Adds a vertex to the graph and returns a char
-- identifier which is the length of the vertice 
-- list + 97. 
add_vertex :: Graph w -> w -> Graph w
add_vertex (Graph v w) weight = Graph (tuple:v) w
	where tuple = (chr((length v)+97), weight)

-- Adds an edge to the graph.
add_edge :: Graph w -> Char -> Char -> w -> Graph w
add_edge (Graph v w) a b weight = Graph v (edge:w)
	where edge = (find_vertice v a, find_vertice v b, weight) 

-- Checks to see if the vertex identifier is
-- present in the graph.
find_vertice :: [(Char, w)] -> Char -> Char
find_vertice [] a = error "Can not find vertice"
find_vertice (x:xs) a = 
		if fst x == a then a 
		else find_vertice xs a 

-- Gets a topological ordering of the graph.
topological_ordering :: Graph w -> [Char]
topological_ordering (Graph v w) = 
		let startingNodes = selectNodes (getIdentifiers v) w 
		    emptyList = []
		    sorted = sortAlgorithm startingNodes emptyList (Graph v w) 
		in sorted	

-- Sort algorithm for topological ordering. 
sortAlgorithm :: [Char] -> [Char] -> Graph w -> [Char]
sortAlgorithm [] sorted _ = sorted 
sortAlgorithm (firstVert:remainingVert) sorted (Graph v w) =
		let neighbours = findNeighbours firstVert w
		    newEdges = removeEdges (edgesToRemove firstVert neighbours w) w
		    nodes = selectNodes neighbours newEdges
		in  sortAlgorithm (remainingVert ++ nodes) (sorted ++ [firstVert]) (Graph v newEdges)

-- Finds the longest path from vertice a to b
weight_of_longest_path :: Graph w -> Char -> Char -> (w -> Int) -> (w -> Int) -> Int
weight_of_longest_path  (Graph v w) startVert endVert f g = 
			let topSort = dropWhile (/= startVert) $ topological_ordering (Graph v w) 
			    finalList = getFinalList (Graph v w) topSort (makeDistList v topSort startVert f) f g 
			in  snd $ head $ filter (\(a,b) -> a == endVert) finalList


-- Finds the distance from all nodes in the topological sort 
-- to their neighbours in the graph 
getFinalList :: Graph w -> [Char] -> [(Char, Int)] -> (w -> Int) -> (w -> Int) -> [(Char, Int)]
getFinalList _  [] finalList f g = finalList
getFinalList (Graph v w) (firstVert:rest) distList f g = 
	    let neighbours = findNeighbours firstVert w 
		finalList = updateList firstVert neighbours distList (Graph v w) f g
	    in  getFinalList (Graph v w) rest finalList f g 

-- calculates the max distance from a vertice in the topological order
-- to all its neighbours
updateList :: Char -> [Char] -> [(Char, Int)] -> Graph w -> (w -> Int) -> (w -> Int) -> [(Char, Int)]
updateList _ [] updatedList _ _ _= updatedList 
updateList firstVert (neighbour:rest) distList (Graph vertices weights) f g = 
	   let edgeWeight = g $ getEdgeWeight firstVert neighbour weights 
	       verticeWeight = f $ getVerticeWeight neighbour vertices
	       newDist = calcDist firstVert neighbour verticeWeight edgeWeight distList 
	       updatedList = replace distList neighbour newDist
	   in  updateList firstVert rest updatedList (Graph vertices weights) f g 

-- Calculates the max distance from a vertice to its neighbour
calcDist :: Char -> Char -> Int -> Int -> [(Char, Int)] -> Int 
calcDist firstVert neighbour verticeWeight edgeWeight distList = 
	 if (getVerticeWeight neighbour distList) < ((getVerticeWeight firstVert distList) + edgeWeight)
	 then (getVerticeWeight firstVert distList) + edgeWeight + verticeWeight 
	 else getVerticeWeight neighbour distList

-- replaces a value in the longest path list 
replace :: [(Char, w)] -> Char -> w -> [(Char, w)]
replace distList vertice value = map (\p@(f, _) -> if f == vertice then (vertice, value) else p) distList

-- creates a list of nodes and weights which will contain
-- the longest path from the root node to all other nodes
makeDistList :: [(Char, w)] -> [Char] -> Char -> (w -> Int) -> [(Char, Int)]
makeDistList vertices topSort startVert f = distList
             where distList = zip topSort $ (f $ snd $ head $ filter (\(a,b) -> a == startVert) vertices):(repeat (-999))

-- finds the neighbours of a vertice
findNeighbours :: Char -> [(Char, Char, w)] -> [Char]
findNeighbours vert edges = secondNodes $ filter (\(a,b,w) -> a == vert) edges 

-- gets the weight of the vertice
getVerticeWeight :: Char -> [(Char, w)] -> w
getVerticeWeight vert vertList = snd $ head $ filter (\(a,b) -> a == vert) vertList

-- gets the weight of the edge between two vertices
getEdgeWeight :: Char -> Char -> [(Char, Char, w)] -> w
getEdgeWeight vert1 vert2 edges = selectThird $ head $ filter (\(a,b,w) -> a == vert1 && b == vert2) edges 

-- Checks which edges to remove 
edgesToRemove :: Char -> [Char] -> [(Char, Char, w)] -> [(Char, Char, w)]
edgesToRemove _ [] _ = []
edgesToRemove firstVert (firstNbr:neighbours) edges = 
	    filter (\(a,b,w) -> (a == firstVert && b == firstNbr)) edges ++ 
	    edgesToRemove firstVert neighbours edges

-- Removes edges 
removeEdges :: [(Char, Char, w)] -> [(Char, Char, w)] -> [(Char, Char, w)]
removeEdges remove edges = deleteFirstsBy compChars edges remove

compChars :: (Char, Char, w) -> (Char, Char, w) -> Bool
compChars (c1, c2, _) (c1', c2', _) = c1 == c1' && c2 == c2'

-- Selects vertices depending on 
selectNodes :: [Char] -> [(Char, Char, w)] -> [Char]
selectNodes  [] _ = []
selectNodes  (firstNbr:neighbours) edges =  
	     if (filter (\(a,b) -> b == firstNbr) (removeWeights edges)) == []
	     then firstNbr:selectNodes neighbours edges
	     else selectNodes neighbours edges

getIdentifiers :: [(Char, w)] -> [Char]
getIdentifiers [] = []
getIdentifiers (vert:rest) = fst vert:getIdentifiers rest

removeWeight :: (Char, Char, w) -> (Char, Char)
removeWeight (a, b, _) = (a, b)

removeWeights :: [(Char, Char, w)] -> [(Char, Char)]
removeWeights [] = []
removeWeights ((a, b, _):rest) = (a, b):removeWeights rest

selectFirst :: (Char, Char, w) -> Char
selectFirst (a, _, _) = a

selectSecond :: (Char, Char, w) -> Char
selectSecond (_, a, _) = a

selectThird :: (Char, Char, w) -> w
selectThird (_, _, w) = w

firstNodes :: [(Char, Char, w)] -> [Char]
firstNodes [] = []
firstNodes (x:xs) = selectFirst x:firstNodes xs

secondNodes :: [(Char, Char, w)] -> [Char]
secondNodes [] = []
secondNodes (x:xs) = selectSecond x:secondNodes xs
