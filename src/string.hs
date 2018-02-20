module Strings where

import Data.Char as Ch

firstLastsq :: String -> String
firstLastsq s =
  procedure part1 part2
  where (par1, par2) = splitAt ((length s) `div` 2) s
        shorter :: String -> String -> (String, String)
        shorter s1 s2
          | length s1 > length s2 = (init s1, s2)
          | length s1 < length s2 = (s1, tail s2)
          | otherwise = (s1, s2)
        (part1, part2) = shorter par1 par2
        procedure :: String -> String -> String
        procedure str1 str2 = if str1 == str2 then str1 else procedure (init str1) (tail str2)

sumDigits :: String -> Int
sumDigits s =
  procedure s 0
  where procedure :: String -> Int -> Int
        procedure [] sum = sum
        procedure (s:st) sum
          | Ch.isDigit s = procedure st (sum + Ch.digitToInt s)
          | otherwise = procedure st sum
