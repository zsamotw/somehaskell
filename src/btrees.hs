module BTrees where

-- bst tree

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


get :: Tree a -> Maybe a
get Empty = Nothing
get (Node x _ _) = Just x


insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node b left right)  = if x < b then Node b ( insert x left ) right
                                else if x > b then Node b left ( insert x right )
                                else Node x left right


elem' :: (Ord a) => a -> Tree a -> Bool
elem' x Empty = False
elem' x (Node b left right)
  | x == b = True
  | x < b = elem' x left
  | x > b = elem' x right
