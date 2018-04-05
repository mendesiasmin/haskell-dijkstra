module Node where


data Node = EmptyNode
          | Node String deriving (Show, Eq)


-- Functions to get attributes ======================================

-- Return node id
node_name :: Node -> String
node_name (Node name) = name

