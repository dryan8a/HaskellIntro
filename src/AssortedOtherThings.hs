module AssortedOtherThings where

import              Prelude



--Divide doesn't work unless type is specified
solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head (foldl fold [] (words expression))
    where   fold (x:y:xs) "*" = (x * y) : xs
            fold (x:y:xs) "+" = (x + y) : xs
            fold (x:y:xs) "-" = (x - y) : xs
            fold xs num = read num : xs

solveRPNInt :: String -> Integer
solveRPNInt expression = head (foldl fold [] (words expression))
    where   fold (x:y:xs) "*" = (x * y) : xs
            fold (x:y:xs) "/" = (y `div` x) : xs
            fold (x:y:xs) "+" = (x + y) : xs
            fold (x:y:xs) "-" = (x - y) : xs
            fold xs num = read num : xs

