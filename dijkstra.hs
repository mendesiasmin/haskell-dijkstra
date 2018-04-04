module Dijkstra where

ifinity = -1

data Node = EmptyNode
        | Node Char ([Distance]) deriving (Eq, Show)

data Distance = NoDistance | Distance Char Int Bool deriving (Eq, Show)

data Graph = EmptyGraph
           | Graph ([Node]) deriving (Show)

data Costs = NoCosts
           | Costs Node [Distance] deriving (Show)


-- Ajustar para pegar os valores de entrada do usuario
dijkstra = do let distances = initialize_distances node_a (nodes graph)
              calc distances


calc :: [Distance] -> [Distance]
calc [] = []
calc distances = do let minor = minimum_cost distances
                    if minor == NoDistance then distances
                    else calc (visit_minor minor distances)

visit_minor :: Distance -> [Distance] -> [Distance]
visit_minor destiny distances = do let new_distances = (remove_distance destiny distances) ++ [visit_node destiny]
                                   update destiny new_distances

------------------------------------------------------------------------------------------------------------

-- *********************************************************************************************************
-- update :: Distance -> [Distance] -> [Distance]
-- update NoDistance _ = []
-- update origin [] = []
-- update origin distances = do let childrens = distances_to (find_node origin (nodes graph))
--                              let new_distances = remove_distances childrens distances
--                              new_distances ++ (update_distances origin childrens)
--
-- update_distances _ [] = []
-- update_distances origin (x:y) = do let new = update_distance origin x
--                                    [new] ++ (update_distances origin y)

-- *********************************************************************************************************

update :: Distance -> [Distance] -> [Distance]
update NoDistance _ = []
update origin [] = []
update origin distances = do let childrens = distances_to (find_node origin (nodes graph))
                             let new_distances = remove_distances childrens distances
                             new_distances ++ (update_distances origin childrens distances)

update_distances :: Distance -> [Distance] -> [Distance] -> [Distance]
update_distances _ [] _ = []
update_distances origin (x:y) distances = do let actual = find (destiny_name x) distances
                                             let new = update_distance origin x actual
                                             [new] ++ (update_distances origin y distances)

remove_distances :: [Distance] -> [Distance] -> [Distance]
-- remove_distances [] _ = []
remove_distances [] distances = distances
remove_distances _ [] = []
remove_distances (x:y) distances = do let new_distances = remove_distance x distances
                                      remove_distances y new_distances

-- Visit node, return visited attr with true
visit_node :: Distance -> Distance
visit_node (Distance name value visited) = Distance name value True

-- Way to node, returns visited attr with false
way_to :: Distance -> Int -> Distance -> Distance
way_to NoDistance _ _ = NoDistance
way_to (Distance name value visited) cost NoDistance = Distance name (value + cost) False
way_to (Distance name value visited) cost (Distance actual_name actual_cost actual_visited)
  | actual_visited = Distance actual_name actual_cost True
  | actual_cost /= ifinity && actual_cost <= (value + cost) = Distance name actual_cost False
  | otherwise = Distance name (value + cost) False

-- Check isf all nodes was visited
all_visited :: [Distance] -> Bool
all_visited (x:y) | (visited x) = (all_visited y)
                  | otherwise = False


update_distance :: Distance -> Distance -> Distance -> Distance
update_distance origin node actual =  way_to node (distance origin) actual

-- Return node of minimum cust
minimum_cost :: [Distance] -> Distance
minimum_cost [] = NoDistance
minimum_cost (x:y) | (distance x) == ifinity || (visited x) = (minimum_cost y)
                   | otherwise = (minor_distance x (minimum_cost y))

minor_distance :: Distance -> Distance -> Distance
minor_distance a b | distance a == ifinity && distance b == ifinity = NoDistance
                   | distance a == ifinity = b
                   | distance b == ifinity = a
                   | (distance a) < (distance b) = a
                   | otherwise = b

-- Initializes costs
initialize_costs :: Node -> Graph -> Costs
initialize_costs EmptyNode graph = NoCosts
initialize_costs origin EmptyGraph = NoCosts
initialize_costs origin graph = Costs origin (initialize_distances origin (nodes graph))

-- Initializes all distances for the calculation with infinity
initialize_distances :: Node -> [Node] -> [Distance]
initialize_distances _ [] = []
initialize_distances EmptyNode (x:y) = []
initialize_distances origin (x:y) = [distance_to origin x] ++ (initialize_distances origin y)

-- cost_to :: Node -> Node -> Int
-- distance_to EmptyNode _ = ifinity
-- distance_to _ EmptyNode = ifinity
-- distance_to :: Node -> Node -> Distance
distance_to origin destiny | origin == destiny = Distance (name origin) 0 True
                           | otherwise = do let result = (find (name destiny) (distances_to origin))
                                            if result == NoDistance then (Distance (name destiny) ifinity False)
                                            else (Distance (name destiny) (distance result) False)

-- find_childrens -> [Distance] -> [Distance] -> [Distance]
find_childrens _ [] = []
find_childrens [] _ = []
find_childrens (x:y) distances = [(find (destiny_name x) distances)] ++ (find_childrens y distances)

-- Return Distance to node
find :: Char -> [Distance] -> Distance
find wanted [] = NoDistance
find wanted (x:y) | wanted == (destiny_name x) = x
                  | otherwise = find wanted y

-- Como fazer sobrecarga?
-- Return node of distance
find_node :: Distance -> [Node] -> Node
find_node wanted [] = EmptyNode
find_node NoDistance _ = EmptyNode
find_node wanted (x:y) | (destiny_name wanted) == (name x) = x
                       | otherwise = find_node wanted y

-- find_node_in_graph :: Distance -> [Nodes] -> Node
find_node_in_graph wanted [] = EmptyNode
find_node_in_graph wanted (x:y) | (destiny_name wanted) == (name x) = x
                                | otherwise = find_node_in_graph wanted y

-- Return node id
name :: Node -> Char
name (Node element distances) = element

-- Return distace of node orgin to node destiny
-- distance_into :: Distance -> Distance -> Int
-- This graph is instace variable ADJUSTMENT
-- Esta funcao era melhores receber os dois nos e pegar a distancia
distance_into origin destiny = find (destiny_name destiny) (distances_to (find_node_in_graph origin (nodes graph)))

-- Return node distances
distances_to :: Node -> [Distance]
distances_to EmptyNode = []
distances_to (Node element dado) = dado

-- Return destiny in Distance
-- Como retornar um char vazio?
destiny_name :: Distance -> Char
-- destiny_name NoDistance = ''
destiny_name (Distance destiny value visited) = destiny

-- Return valeu of Distance
distance :: Distance -> Int
distance NoDistance = ifinity
distance (Distance destiny value visited) = value

-- Return if the node was visited
visited :: Distance -> Bool
visited NoDistance = False
visited (Distance destiny value visited) = visited

-- Returns nodes in graph
nodes :: Graph -> [Node]
nodes EmptyGraph = []
nodes (Graph nodes) = nodes

-- Returns distaces in Costs
distances_costs :: Costs -> [Distance]
distances_costs NoCosts = []
distances_costs (Costs node distances) = distances

-- O que o operador : faz?
remove_distance :: Distance -> [Distance] -> [Distance]
remove_distance NoDistance (x:y) = []
remove_distance _ [] = []
remove_distance distance (x:y) | (destiny_name distance) == (destiny_name x) = y
                               | otherwise = [x] ++ (remove_distance distance y)

-- Test inputs
ab = Distance 'b' 20 False
ad = Distance 'd' 80 False
ag = Distance 'g' 90 False
bf = Distance 'f' 10 False
cf = Distance 'f' 50 False
ch = Distance 'h' 20 False
cd = Distance 'd' 10 False
dc = Distance 'c' 10 False
dg = Distance 'g' 20 False
eg = Distance 'g' 30 False
eb = Distance 'b' 50 False
fc = Distance 'c' 10 False
fd = Distance 'd' 40 False
ga = Distance 'a' 20 False
ia = Distance 'a' (-1) False
ib = Distance 'b' (-1) False

node_a = Node 'a' [ab, ad, ag]
node_b = Node 'b' [bf        ]
node_c = Node 'c' [cf, ch, cd]
node_d = Node 'd' [dc, dg    ]
node_e = Node 'e' [eg, eb    ]
node_f = Node 'f' [fc, fd    ]
node_g = Node 'g' [ga        ]
node_h = Node 'h' [          ]

graph = Graph [ node_a, node_b, node_c, node_d, node_e, node_f, node_g ]
