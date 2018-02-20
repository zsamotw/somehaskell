{-# LANGUAGE OverloadedStrings #-}
import Data.Char

myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

myTake :: Int -> [a] -> [a]
myTake n xs = if  n > 0 || null xs
              then head xs : myTake(n - 1)(tail xs)
              else []
 -- | n > 0 || null xs = head xs : myTake (n - 1)(tail xs)
 -- | n = 0 []

myOneLast :: [a] -> a
myOneLast xs = if length xs == 2 then head xs else myOneLast $ tail xs 

myNot False = True
myNot True = False

lLength :: [a] -> Int
lLength [] = 0
lLength (x:xs) = 1 + lLength xs

threeth :: [a] -> Maybe a
threeth (_:_:x:_) = Just x
three3th _         = Nothing

fullName name surname = let ful = name ++ surname in print ful
fullName2 name surname = print ful where ful = name ++ surname ++ "!!"

pluralise :: [Int] -> String -> [String]
pluralise counts word = map plural counts
  where plural 1 = "one" ++ word
        plural 2 = "two" ++ word
        plural n = show n ++ "hallo" ++ word


bar = let a = 1
          b = 2
      in a + b

foo = a + b
      where a = 1
            b = 2

fromMaybe defval wrapped =
  case wrapped of
    Nothing -> defval
    Just v -> v

setName name
  | name == "tomasz" = "TOMASZ"
  | name == "anna" = "ANNA"
  | otherwise = "other name"

--exercises

mean :: [Int]  -> Int
mean xs =
  let sum :: [Int] -> Int
      sum (x:xs) = x + sum xs
      sum [] = 0
      length :: [Int] -> Int
      length (x:xs) = 1 + length xs
      length [] = 0
  in sum xs + length xs

pali :: [a] -> [a]
pali xs = let reverse :: [a] -> [a]
              reverse (x:xs) = reverse xs ++ [x]
              reverse [] = []
          in xs ++ (reverse xs)

isPali :: (Eq a) => [a] -> Bool
isPali xs = let check :: (Eq a) => [a] -> [a] -> Bool
                check (x:xs) (y:ys) = if x == y then check xs ys else False
                check [] [] = True
                check _ _ = False
             in check xs (reverse xs)

interspac ::  String -> [String] -> String
interspac s (x:xs) = x ++ s ++ interspac s xs
interspac s [] = ""

lsort :: [[a]] -> [[a]]
lsort []     = []
lsort (x:xs) = insert x (lsort xs)
  where insert :: [a] -> [[a]] -> [[a]]
        insert x (y:ys) = if length x > length y then y : (insert x ys) else x : (insert y ys)
        insert x [] = [x]

--create emal
x = foldr (++) ("@mail.com")(map (\el -> if fst el == 1 then (take 3 (snd el)) else (take 2 (snd el)))(zip [1,2..] (words "Tomasz Wiech")))

asInt str = loop 0 str

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

--as int as fold

asInt_Fold str =
  case head str of
   '-' -> negate (foldl step zero xs)
     where zero = 0
           xs = tail str
           step acc el = acc * 10 + digitToInt el
   _ ->  foldl step zero xs
     where zero = 0
           xs = str
           step acc el = acc * 10 + digitToInt el


makeTomasz xs = helper "Tomasz" xs
  where helper s (x:xs1) = s : helper s xs1
        helper _ []      = []


myFilter p xs = foldr step [] xs
  where step el acc | p el = el : acc
                    | otherwise = acc

append xs ys = foldr (:) ys xs

concat' :: [[a]] -> [a]
concat' xxs = foldr step [] xxs
  where step el acc = el ++ acc

takeWhile' :: (Eq a) => (a -> Bool) -> [a] -> [a]
takeWhile' p xs = foldr step [] xs
  where step el acc | p el = []
                    | otherwise = el : acc

tails :: [a] -> [[a]]
tails xs@(x:xs') = xs : tails xs'
tails [] = []

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f(g x)

countUppWords = length . filter (isUpper . head) . words

encode :: String -> Int -> [Int]
encode xs shift = map (+ shift) ints
  where ints = map ord xs

decode :: [Int] -> Int -> String
decode xs shift = chars
  where ints = map (+ (negate shift)) xs
        chars = map chr ints


--visit card
visitcard name surname phone city = show stars ++ "'\n'" ++ first ++ "'\n'" ++ second ++ show stars
  where first = name ++ " "++ surname ++ " " ++ "*"
        second = city ++ " " ++ phone ++ " " ++ "*"
        stars = replicate (length longer) '*'
        longer = if length first > length second then first else second

while ::  Bool -> Int -> Int
while p body = if p then while p body else body
