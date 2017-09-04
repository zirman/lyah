module MyTree where

data Tree a =
  EmptyTree |
  Node (Tree a) a (Tree a)
  deriving (Show)

singleton :: a -> Tree a
singleton x = Node EmptyTree x EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a EmptyTree = singleton a
treeInsert a node@(Node left b right)
  | a < b = Node (treeInsert a left) b right
  | a > b = Node left b (treeInsert a right)
  | a == b = node

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem a EmptyTree = False
treeElem a (Node left b right)
  | a < b = treeElem a left
  | a > b = treeElem a right
  | a == b = True
