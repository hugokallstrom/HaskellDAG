module Graph
(Graph(Graph)
, create_graph
, add_vertex
, add_edge
, find_vertice
, topological_ordering
) where

import Data.Char
import qualified Data.Set as Set

data Graph w = Graph {vertices :: [(Char, w)],
                      edges :: [(Char, Char, w)]} deriving Show

create_graph :: Graph w -> Graph w
create_graph emptyGraph = Graph [] []

add_vertex :: Graph w -> w -> Graph w
add_vertex (Graph v w) weight = Graph (tuple:v) w
	where tuple = (chr((length v)+97), weight)

add_edge :: Graph w -> Char -> Char -> w -> Graph w
add_edge (Graph v w) a b weight = Graph v (edge:w)
	where edge = (find_vertice v a, find_vertice v b, weight) 

find_vertice :: [(Char, w)] -> Char -> Char
find_vertice [] a = error "Can not find vertice"
find_vertice (x:xs) a = 
		if fst x == a then a 
		else find_vertice xs a 

topological_ordering :: Graph w -> [Char]
topological_ordering (Graph v w) = 
		let startingNodes = noIncEdges (Graph v w)
		    emptyList = []
		    sorted = sortAlgorithm startingNodes emptyList (Graph v w) 
		in sorted	

sortAlgorithm :: [Char] -> [Char] -> Graph w -> [Char]
sortAlgorithm [] sorted _ = sorted 
sortAlgorithm (firstVert:remainingVert) sorted (Graph v w) =
		let neighbours = secondNodes $ filter (\(a,b,w) -> a == firstVert) w 
		    newEdges = removeEdges firstVert neighbours w 		
		    nodes = noIncEdges (Graph v (filter (\(a,b,w) -> a == firstVert) newEdges))
		in  sortAlgorithm remainingVert (sorted ++ nodes) (Graph v newEdges)

noIncEdges :: Graph w -> [Char]
noIncEdges (Graph v w) =
	let set1 = Set.fromList $ firstNodes w
	    set2 = Set.fromList $ secondNodes w
	    startNodes = Set.toList $ Set.difference set1 set2
	in  startNodes 

removeEdges :: Char -> [Char] -> [(Char, Char, w)] -> [(Char, Char, w)]
removeEdges _ [] _ = []
removeEdges firstVert (firstNbr:neighbours) edges = filter (\(a,b,w) -> (a == firstVert && b == firstNbr)) edges ++ removeEdges firstVert neighbours edges

selectFirst :: (Char, Char, w) -> Char
selectFirst (a, _, _) = a

selectSecond :: (Char, Char, w) -> Char
selectSecond (_, a, _) = a

firstNodes :: [(Char, Char, w)] -> [Char]
firstNodes [] = []
firstNodes (x:xs) = selectFirst x:firstNodes xs

secondNodes :: [(Char, Char, w)] -> [Char]
secondNodes [] = []
secondNodes (x:xs) = selectSecond x:secondNodes xs


