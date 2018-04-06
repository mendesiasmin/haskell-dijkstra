module Dijkstra where

import Node
import Edge
import Input

type Graph = [Edge]
type Cost = [Edge]

-- Initializes all wights for the calculation with infinity
initialize :: Node -> [Node] -> Graph -> Cost
initialize _ [] _= []
initialize _ _ [] = []
initialize EmptyNode _ _ = []
initialize node_origin (x:y) graph = do let edge = edge_to node_origin x graph
                                        if edge /= NoEdge then [edge] ++ (initialize node_origin y graph)
                                        else (initialize node_origin y graph)

less_between :: Edge -> Edge -> Edge
less_between a b | weight a == ifinity && weight b == ifinity = NoEdge
                 | weight a == ifinity = b
                 | weight b == ifinity = a
                 | (weight a) < (weight b) = a
                 | otherwise = b

-- Return node of minimum cust
minimum_cost :: [Edge] -> Edge
minimum_cost [] = NoEdge
minimum_cost (x:y) | (weight x) == ifinity || (visited x) = (minimum_cost y)
                   | otherwise = (less_between x (minimum_cost y))

-- Update weights from node -----------------------------------------------------------------

find_cost_to :: Node -> Cost -> Edge
find_cost_to EmptyNode _ = NoEdge
find_cost_to node [] = NoEdge
find_cost_to node (x:y) | node == (destiny x) = x
                        | otherwise = find_cost_to node y

update_cost :: Edge -> Edge -> Cost -> Edge
update_cost NoEdge _ _ = NoEdge
update_cost _ NoEdge _ = NoEdge
update_cost _  _ [] = NoEdge
update_cost edge_graph edge_origin costs = do let edge_cost = find_cost_to (destiny edge_graph) costs
                                              if visited edge_cost then edge_cost
                                              else do let cost = (weight edge_origin) + (weight edge_graph)
                                                      if (weight edge_cost) /= ifinity && (weight edge_cost) <= cost then edge_cost
                                                      else update_weight edge_cost cost

valid_update_cost :: Edge -> Edge -> Cost -> Cost
valid_update_cost edge_graph edge_origin costs = do let new_edge = update_cost edge_graph edge_origin costs
                                                    if new_edge /= NoEdge then [new_edge]
                                                    else []

remove_cost :: Edge -> Cost -> Cost
remove_cost NoEdge _ = []
remove_cost _ [] = []
remove_cost edge (x:y) | (destiny edge) == (destiny x) = y
                       | otherwise = [x] ++ (remove_cost edge y)

update_weights :: Edge -> Graph -> Cost -> Cost
update_weights NoEdge _ costs = costs
update_weights _ [] costs = costs
update_weights _ _ [] = []
update_weights edge (x:y) costs | (destiny edge) == (origin x) = do let new_costs = (remove_cost x costs) ++ (valid_update_cost x edge costs)
                                                                    update_weights edge y new_costs
                                | otherwise = update_weights edge y costs

------------------------------------------------------

visit_minor :: Edge -> Cost -> Graph -> [Edge]
visit_minor minor costs graph = do let new_costs = (remove_cost minor costs) ++ [visit minor True]
                                   update_weights minor graph new_costs

calc :: Cost -> Graph -> Cost
calc [] _ = []
calc _ [] = []
calc edges graph = do let minor = minimum_cost edges
                      if minor == NoEdge then edges
                      else calc (visit_minor minor edges graph) graph

dijkstra :: Node -> [Node] -> Graph -> Cost
dijkstra node nodes graph = do let costs = initialize node nodes graph
                               calc costs graph
