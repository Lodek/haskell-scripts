
data Graph a = Node a [Graph a] | EmptyGraph deriving (Show, Eq, Read)

addSingleton :: a -> Graph a -> Graph a
addSingleton a (Node n nodes) = 
