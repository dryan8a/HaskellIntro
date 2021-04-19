module Lists where

import           Prelude                 hiding ( elem
                                                , filter
                                                , foldl
                                                , foldr
                                                , map
                                                , take
                                                , takeWhile
                                                , zip
                                                )

list :: [String]
list = ["Hello", " ", "World"]

-- Returns true if the value is an element of the list (LINQ Contains)
elem :: (Eq a) => a -> [a] -> Bool
elem _ []       = False 
elem x (y : ys)
    | x == y    = True 
    | otherwise = elem x ys



-- Calls the function on each element of the input list and returns the outputs as a list (LINQ Select)
map :: (a -> b) -> [a] -> [b]
map _ []          = []
map func (x : xs) = func x : map func xs



-- Returns all elements where the picate returns true (LINQ Where)
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x : xs)
    | p x    = x : filter p xs
    | otherwise =     filter p xs



-- Return a list of the first n elements (LINQ Take)
take :: Int -> [a] -> [a]
take _ []       = []
take (0) _      = []
take a (x : xs) = x : take (a-1) xs

-- Returns elements until one fails the picate (LINQ TakeWhile)
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []  = []
takeWhile p (x : xs)
    | p x    = x : takeWhile p xs
    | otherwise = []

-- Note: There are similar functions drop and dropWhile, though you don't need to do them (LINQ Skip and SkipWhile)



-- Zips two lists into a list of tuples
-- If the lists are different sizes, ignore all elements in the longer list after the last element of the shorter one
zip :: [a] -> [b] -> [(a, b)]
zip [] []             = []
zip _  []             = []
zip [] _              = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys

-- Note: There is a similar function zipWith which takes a function for how to combine the two values (LINQ Zip)




-- Runs through the list from left to right applying the function to the current value and some aggregate value (LINQ Aggregate)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _    a []       = a
foldl func a (x : xs) = foldl func (func a x) xs

-- Same thing as foldl but right to left
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _    a []       = a
foldr func a (x : xs) = func x (foldr func a xs)

-- Think: What might be some advantages or disadvantages of foldr vs foldl



-- Returns the first element that satisfies the picate, or Nothing if one does not exist (LINQ First)
find :: (a -> Bool) -> [a] -> Maybe a
find _ []       = Nothing
find p (x : xs) 
    | p x    = Just x
    | otherwise = find p xs



-- Sort a list of values using their Ord comparison using quicksort
-- For simplicity's sake, do not worry about space complexity
quicksort :: (Ord a) => [a] -> [a] -- solution looks nice but it isn't efficient
quicksort [] = []
quicksort (pivot:xs) = quicksort lowerElements ++ [pivot] ++ quicksort upperElements
    where
        lowerElements = filter (<pivot) xs
        upperElements = filter (>=pivot) xs

-- Sort a list of values using their Ord comparison using mergesort
-- For simplicity's sake, do not worry about space complexity
-- mergesort :: (Ord a) => [a] -> [a]
-- mergesort [] = []
-- mergesort [x] = [x]
-- mergesort xs = let
--                     len = (length xs) `div` 2
--                 in merge (mergesort (take (len) xs)) (mergesort (drop (len) xs)
--     where  
--         merge ys [] = ys
--         merge 
     
