{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = (x - lastDigit x) `div` 10

toDigits :: Integer -> [Integer]
toDigits x = if (x < 1) then [] else toDigits (dropLastDigit x) ++ [lastDigit x] 

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = if null xs then [] else x : (2* head xs) : doubleEveryOther (tail xs)

sumDigits :: [Integer] -> Integer
sumDigits (x:xs) = if null xs then sum (toDigits x) else sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = if ((sumDigits (doubleEveryOther (reverseList (toDigits x)))) `mod` 10 == 0) then True else False

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f n = if n == 0 then (f) else f . (pow f (n-1))

g :: Integer -> Integer
g n = if n == 0 then 0 else n - g(g(n-1))

h :: Integer -> Integer
h n = if n ==0 then 0 else  n - h(h(h(n-1)))

d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

-- powerSet :: Set a => [a] -> Set a
powerSet = error "powerSet not yet defined"
