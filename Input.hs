module Input where

import Node
import Edge

node_a = Node "a"
node_b = Node "b"
node_c = Node "c"
node_d = Node "d"
node_e = Node "e"
node_f = Node "f"
node_g = Node "g"
node_h = Node "h"

ab = Edge node_a node_b 20 False
ad = Edge node_a node_d 80 False
ag = Edge node_a node_g 90 False
bf = Edge node_b node_f 10 False
cf = Edge node_c node_f 50 False
ch = Edge node_c node_h 20 False
cd = Edge node_c node_d 10 False
dc = Edge node_d node_c 10 False
dg = Edge node_d node_g 20 False
eg = Edge node_e node_g 30 False
eb = Edge node_e node_b 50 False
fc = Edge node_f node_c 10 False
fd = Edge node_f node_d 40 False
ga = Edge node_g node_a 20 False

nodes = [ node_a,
          node_b,
          node_c,
          node_d,
          node_e,
          node_f,
          node_g,
          node_h ]

graph = [ ab,
          ad,
          ag,
          bf,
          cf,
          ch,
          cd,
          dc,
          dg,
          eg,
          eb,
          fc,
          fd,
          ga ]
