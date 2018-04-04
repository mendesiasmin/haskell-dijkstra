module Node where

import Distance

data Node = EmptyNode
        | Node Char ([Distance]) deriving (Eq, Show)


-- Functions to get attributes ========================================

-- Return node id
node_name :: Node -> Char
node_name (Node element distances) = element

-- Return node distances
distances_to :: Node -> [Distance]
distances_to EmptyNode = []
distances_to (Node element distances) = distances


