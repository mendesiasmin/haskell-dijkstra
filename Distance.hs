module Distance where

ifinity = -1

data Distance = NoDistance
              | Distance Char Int Bool deriving (Eq, Show)


-- Functions to get attributes ========================================

-- Return node name in Distance
-- Como retornar um char vazio?
distance_name :: Distance -> Char
distance_name NoDistance = ' '
distance_name (Distance destiny value visited) = destiny

-- Return distance valeu of Distance
distance :: Distance -> Int
distance NoDistance = ifinity
distance (Distance destiny value visited) = value

-- Return if the node was visited
visited :: Distance -> Bool
visited NoDistance = False
visited (Distance destiny value visited) = visited

-- Update Attributes ==================================================

-- Check Distance as visited
visit :: Distance -> Distance
visit (Distance name value visited) = Distance name value True

