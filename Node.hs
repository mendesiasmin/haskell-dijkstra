module Node where


data Node = EmptyNode
          | Node String deriving (Eq)

instance Show Node where
  show (Node name) = show name

-- Functions to get attributes ======================================

-- Return node id
node_name :: Node -> String
node_name (Node name) = name
