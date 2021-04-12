module BasicFunctions where

-- Takes in an int and doubles it
double :: Int -> Int
double = (*2) -- point free function
-- double x = x * 2 (also valid but less cool)

doubleAndAddOne :: Int -> Int 
doubleAndAddOne = (+1) . double

-- Takes in three integers and sums them together
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- Double the integer if the boolean is true, otherwise return the integer as normal
doubleIfTrue :: Int -> Bool -> Int
doubleIfTrue x p 
    | p         = x * 2
    | otherwise = x

-- You can guess what this one does
absoluteVal :: Int -> Int
absoluteVal x 
    | x < 0     = -x
    | otherwise = x
-- note the built in function abs
-- absoluteVal = abs