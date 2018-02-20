type Reviev = String


data BookInfo = Book {isbn :: Int, title :: String, authors :: [String]} deriving Show

data BetterReview = BetterReview BookInfo Reviev deriving Show

bookId (Book isbn _ _) = isbn

type Name = String
type Age = Int

data Human = Man Name Age | Woman Name Age deriving Show

checkHuman :: Human -> String
checkHuman (Man n a)= "man"
checkHuman (Woman n a )= "woman"

checkHuman2 :: Human -> String
checkHuman2 human =
  case human of
    Man n a -> n
    Woman n a -> n

data MyList a = Cons a (MyList a) | Nil deriving Show

fromList :: [a] -> MyList a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

data Btree a = Empty | NonEmpty a (Btree a) (Btree a) deriving Show

max' :: Btree Int -> Int
max' (NonEmpty _ treeA treeB) = 1 + (max (max' treeA) (max' treeB))
max' Empty = 0
  
data Btree2 a = Tree a (Maybe (Btree2 a)) (Maybe (Btree2 a)) deriving Show

