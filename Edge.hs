module Edge where

import Node

ifinity = -1

data Edge = NoEdge
          |  Edge Node Node Int Bool deriving (Show, Eq)

-- Functions to get attributes ======================================

-- Returns node origin to Edge
origin :: Edge -> Node
origin NoEdge = EmptyNode
origin (Edge origin destiny weight visited) = origin


-- Returns node destiny to Edge
destiny :: Edge -> Node
destiny NoEdge = EmptyNode
destiny (Edge origin destiny weight visited) = destiny

-- Returns weight to Edge
weight :: Edge -> Int
weight NoEdge = ifinity
weight (Edge origin destiny weight visited) = weight

-- Returns if Edge was visited
visited :: Edge -> Bool
visited NoEdge = False
visited (Edge origin destiny weight visited) = visited

-- Functions to update attributes ===================================

update_weight :: Edge -> Int -> Edge
update_weight NoEdge _ = NoEdge
update_weight edge weight = Edge (origin edge) (destiny edge) weight (visited edge)

visit :: Edge -> Bool -> Edge
visit NoEdge _ = NoEdge
visit edge visited = Edge (origin edge) (destiny edge) (weight edge) visited

-- Functions to find edges ===================================

edge_to :: Node -> Node -> [Edge] -> Edge
edge_to EmptyNode _ _ = NoEdge
edge_to _ EmptyNode _ = NoEdge
edge_to node_origin node_destiny [] = (Edge node_origin node_destiny ifinity False)
edge_to node_origin node_destiny (x:y) | node_origin == node_destiny = NoEdge
                                       | node_origin == (origin x) && node_destiny == (destiny x) = x
                                       | otherwise = edge_to node_origin node_destiny y

find_equal :: Edge -> [Edge] -> Edge
find_equal _ [] = NoEdge
find_equal NoEdge _ = NoEdge
find_equal edge (x:y) | (origin edge) == (origin x) && (destiny edge) == (destiny x) = x
                      | otherwise = find_equal edge y


-- Functions to remove edges in array ===================================

remove_edge :: Edge -> [Edge] -> [Edge]
remove_edge NoEdge (x:y) = []
remove_edge _ [] = []
remove_edge edge (x:y) | edge == x = y
                       | otherwise = [x] ++ (remove_edge edge y)

remove_edge_to :: Edge -> [Edge] -> [Edge]
remove_edge_to NoEdge (x:y) = []
remove_edge_to _ [] = []
remove_edge_to edge (x:y) | (destiny edge) == (destiny x) = y
                       | otherwise = [x] ++ (remove_edge_to edge y)

remove_edges_from :: Edge -> [Edge] -> [Edge]
remove_edges_from NoEdge _ = []
remove_edges_from _ [] = []
remove_edges_from edge (x:y) | (destiny edge) == (origin x) = remove_edges_from edge y
                             | otherwise = [x] ++ (remove_edges_from edge y)


-- Functions to update edges in array ===================================

