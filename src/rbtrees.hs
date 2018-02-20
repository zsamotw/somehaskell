module RBTrees where

--red balck tree

data Color = R | B deriving Show

data Tree a = Empty | Node Color a (Tree a) (Tree a) deriving Show

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x (Node _ y left right)
  | x == y = True
  | x > y = member x right
  | x < y = member x left

insert :: (Ord a) => a -> Tree a -> Tree a
insert x tree = makeBlack $ ins tree
  where ins Empty = Node R x Empty Empty
        ins (Node color y left right)
          | x == y = Node color y left right
          | x < y = balance color y (ins left) right
          | x > y = balance color y left (ins right)
        makeBlack (Node _ y left right) = Node B y left right

balance :: Color -> a -> Tree a -> Tree a -> Tree a
balance B z (Node R y (Node R x l r) rr) rrr = Node R y (Node B x l r) (Node B z rr rrr)
balance B z (Node R y ll (Node R x l r)) rrr = Node R x (Node B z ll l) (Node B y r rrr)
balance B z lll (Node R x (Node R y l r) rr) = Node R y (Node B z lll l) (Node B x r rr)
balance B z lll (Node R x ll (Node R y l r)) = Node R x (Node B z lll ll) (Node B y l r)
balance color x left right = Node color x left right
