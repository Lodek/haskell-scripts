
data Node v = Node v deriving (Show, Eq)
data Edge v = Edge (Node v, Int) deriving (Show, Eq)
data Graph v = [(Node v, [Edge v])] deriving (Show, Eq)

singleton :: (Show v) => v -> Graph v
singleton v = Graph [(Node v, [])]

connectNodes :: (Show v) => Graph v -> Node v -> Node v -> Int -> Graph v
connectNodes (Graph edges) n1 n2 cost =
  where n1_al = 

-- updateEdges :: Graph v -> Node v
