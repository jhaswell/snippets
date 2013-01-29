

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show
