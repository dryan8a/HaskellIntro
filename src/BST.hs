module BST where

data BST a = Node a (BST a) (BST a)
            | Empty 

-- Returns the left subtree if it exists
left :: BST a -> Maybe (BST a)
left Empty = Nothing 
left (Node _ Empty _) = Just Empty
left (Node _ l _) = Just l

-- Returns the right subtree if it exists
right :: BST a -> Maybe (BST a)
right Empty = Nothing 
right (Node _ _ Empty) = Just Empty 
right (Node _ _ r) = Just r

-- Returns the value of the node if it exists
value :: BST a -> Maybe a
value Empty = Nothing 
value (Node val _ _) = Just val

-- Returns the empty BST
empty :: BST a
empty = Empty

-- Takes a value and returns a BST containing just that value
singleton :: a -> BST a
singleton a = Node a Empty Empty

-- Takes a comparable value and a BST and adds that value to the BST
insert :: (Ord a) => a -> BST a -> BST a
insert a Empty = singleton a
insert a (Node v l r) 
    | a < v     = Node v (insert a l) r
    | otherwise = Node v l (insert a r)

-- Takes a comparable value and a BST and checks if that value is in the BST
elem' :: (Ord a) => a -> BST a -> Bool
elem' _ Empty = False 
elem' a (Node v l r)
    | a < v = elem' a l
    | a > v = elem' a r
    | otherwise = True

-- Takes a comparable value and removes the first instance of that value it finds in the BST
-- If it does not find that value, return it as is
delete :: (Ord a) => a -> BST a -> BST a
delete _ Empty = Empty
delete a (Node v l r) 
    | a < v = Node (delete a l) v r
    | a > v = Node l v (delete a r)
    | otherwise = 

-- Takes a list of comparable values and creates a valid BST from it
fromList :: (Ord a) => [a] -> BST a
fromList = undefined

-- Flattens the BST into a list
toList :: BST a -> [a]
toList = undefined
