module BinaryTree
  (Tree(..),
   singleton,
   insert,
   elem',
   ) where
   
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: (Ord a) => a -> Tree a
singleton a = Node a EmptyTree EmptyTree

insert :: (Ord a) => Tree a -> a -> Tree a
insert (EmptyTree) a = singleton a
insert (Node n left right) a
  | a == n = Node n left right
  | a < n = Node n (insert left a) right
  | a > n = Node n left (insert right a)
  
elem' :: (Ord a) => Tree a -> a -> Bool
elem' (EmptyTree) a = False
elem' (Node n left right) a
  | a == n = True
  | a < n = elem' left a
  | a > n = elem' right a
 
