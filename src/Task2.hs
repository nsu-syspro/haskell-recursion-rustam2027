{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (reverse, map, sum, toDigits)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n f l = (n - (sum (map (normalize n) (doubleLast (map f l))) `mod` n)) `mod` n

normalize :: Int -> Int -> Int
normalize n m | m < n = m
              | otherwise = m - n + 1

doubleLast :: [Int] -> [Int]
doubleLast = reverse . doubleFirst . reverse

doubleFirst :: [Int] -> [Int]
doubleFirst [] = []
doubleFirst [x] = [2*x]
doubleFirst (x:y:xs) = 2*x : y : doubleFirst xs

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt 'a' = 10
digitToInt 'A' = 10
digitToInt x = digitToInt (pred x) + 1

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n = luhnDec (toDigits (n `div` 10)) == fromInteger (n `mod` 10)

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

first :: [a] -> a
first [] = undefined
first (x:_) = x

last :: [Char] -> Char
last = first . reverse

tail :: [a] -> [a]
tail [] = undefined
tail (_:xs) = xs

cutLast :: [Char] -> [Char]
cutLast = reverse . tail . reverse

validateHex :: [Char] -> Bool
validateHex l = digitToInt (last l) == luhnModN 16 digitToInt (cutLast l) 
