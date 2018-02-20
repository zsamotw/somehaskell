{-# LANGUAGE TypeOperators #-}
data Colors = Black | White

class Equality a where
  isEqual :: a -> a -> Bool
  isEqual a b = not (notEqual a b)
  
  notEqual :: a -> a -> Bool
  notEqual a b = not (isEqual a b)

instance Equality Colors where
  isEqual Black Black = True
  isEqual White White = True
  isEqual _ _         = False

instance Show Colors where
  show Black = "is Black"
  show White = "is White"

class Strange a where
  cross :: a -> a

instance Strange Colors where
  cross White = Black
  cross Black = White

data Person = Person{name :: String, age :: Int} deriving Show
