module Recursion where

factorial :: Integer -> Integer
factorial (1) = 1
factorial a = a * factorial (a-1)
-- factorial a
--     | a > 1 = a * factorial (a-1)
--     | otherwise = 1

fibonacci :: Integer -> Integer
fibonacci (0) = 1
fibonacci (1) = 1
fibonacci a = fibonacci (a-1) + fibonacci (a-2)