module Agh where

--find armstrongs

findArmstrong :: [Int] -> [Int]
findArmstrong numbers = filter armstrong numbers

armstrong :: Int -> Bool
armstrong x = x == x' where
  listOfDigits = nToList x
  x' = sum $ map (^ (length listOfDigits))(listOfDigits)


nToList :: Int -> [Int]
nToList x = loop x [] where
  loop :: Int -> [Int] -> [Int]
  loop n res
    | n < 10 = n : res
    | otherwise = let i = n `mod` 10
                      next = (n - i) `div` 10
                  in loop next (i:res)

listToN :: [Int] -> Int
listToN xs = foldl addDigit 0 xs where
  addDigit acc d = 10*acc + d

--palindroms
palindroms :: [Int] -> [(Bool, Int)]
palindroms = map wayToPalindrome

wayToPalindrome :: Int -> (Bool, Int)
wayToPalindrome x
  | isPali x = (True, x)
  | isPali (addToMyPali x) = (True, addToMyPali x)
  | otherwise = wayToPalindrome (addToMyPali x) where
      isPali :: Int -> Bool
      isPali n = nToList n == reverse (nToList n)
      addToMyPali :: Int -> Int
      addToMyPali n = n + listToN (reverse (nToList n))

--sito erostatesa

erostates :: Int -> [Int]
erostates n = take n (ero [2..]) where
  ero :: [Int] -> [Int]
  ero [] = []
  ero (x:xs) = x : (ero $ filter (\el -> (el `mod` x) /= 0) xs )


