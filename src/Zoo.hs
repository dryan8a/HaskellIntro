module Zoo where

data Animal = Cat String Int 
            | Giraffe Int

-- Either "Meow" or "..."
makeSound :: Animal -> String
makeSound (Cat _ _)    = "Meow"
makeSound (Giraffe _)  = "..." 

-- Giraffes don't have names
getName :: Animal -> Maybe String
getName (Cat n _)   = Just n
getName (Giraffe _) = Nothing

-- Increase chonkiness or neck length by the amount inputted
feed :: Animal -> Int -> Animal
feed (Cat n a) b   = Cat n (a + b)
feed (Giraffe a) b = Giraffe (a + b)

-- This is a battle zoo!
-- The battle function takes two animals and returns the one who would win in a fight.
-- Giraffes always beat cats.
-- If they are both giraffes or cats, the winner is the one with the least chonkiness or the longest neck.
battle :: Animal -> Animal -> Animal
battle (Cat c a) (Cat d b)
    | a < b     = Cat c a
    | otherwise = Cat d b
battle (Giraffe a) (Cat _ _)    = Giraffe a
battle (Cat _ _) (Giraffe a)    = Giraffe a
battle (Giraffe a) (Giraffe b) 
    | a > b     = Giraffe a
    | otherwise = Giraffe b 
