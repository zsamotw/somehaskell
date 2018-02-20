module Agh where

import System.IO
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Function (on)
import Data.Char


--find armstrongs

findArmstrong :: [Int] -> [Int]
findArmstrong = filter armstrong

armstrong :: Int -> Bool
armstrong x = x == x' where
  listOfDigits = nToList x
  x' = sum $ map (^ (length listOfDigits)) listOfDigits


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

-- Erostates

erostates :: Int -> [Int]
erostates n = take n (ero [2..]) where
  ero :: [Int] -> [Int]
  ero [] = []
  ero (x:xs) = x : (ero $ filter (\el -> (el `mod` x) /= 0) xs )


-- words counters

-- only with grouping elems in list
process :: String -> [(String, Int)]
process content = take 20 $ List.sortBy (flip compare `on` snd) $ map (\el -> (head el, length el)) $ List.group $ List.sort $  words $ map toLower content

-- with map
countWords :: String -> Map.Map String Int
countWords content = foldl mapper Map.empty $ words content

mapper :: Map.Map String Int -> String -> Map.Map String Int
mapper map str = Map.insert k v map where
  k = str
  v = case Map.lookup k map of
        Nothing -> 1
        Just x -> x + 1

-- main code for process file
main = do
       inh <- openFile "text.txt" ReadMode
       content <- hGetContents inh
       let res = process content
       print res
       hClose inh














