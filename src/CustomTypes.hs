module CustomTypes where

-- Either Spades, Clubs, Hearts, or Diamonds
data Suit = Spades
          | Clubs
          | Hearts
          | Diamonds
          deriving (Show)

-- Either a NumberCard with an Int representing the card value or a face card: an Ace, Jack, Queen or King
data Rank = NumberCard Int
          | Jack
          | Queen
          | King
          | Ace
          deriving (Show)

-- Either a PlayingCard with a Rank and a Suit or a Joker
data Card = PlayingCard Rank Suit | Joker

instance Show Card where
    show (PlayingCard (NumberCard n) suit)  = show n ++ " of " ++ show suit
    show (PlayingCard rank suit)            = show rank ++ " of " ++ show suit
    show Joker                              = "Joker"

-- True if the playing card is a Joker
isJoker :: Card -> Bool
isJoker Joker   = True
isJoker _       = False

-- True if the playing card is Red (Heart/Diamond)
isRed :: Card -> Bool
isRed (PlayingCard _ Hearts)    = True 
isRed (PlayingCard _ Diamonds)  = True
isRed _                         = False

-- True if the playing card is a Jack, a Queen, a King, or an Ace
isFaceCard :: Card -> Bool
isFaceCard (PlayingCard (NumberCard _) _)   = False 
isFaceCard _                                = True

-- Return the value of the card (Ace=1, Number Card=Value, Jack=11, Queen=12, King=13)
getValue :: Card -> Int
getValue (PlayingCard (NumberCard a) _) = a
getValue (PlayingCard Jack _)           = 11
getValue (PlayingCard Queen _)          = 12
getValue (PlayingCard King _)           = 13
getValue (PlayingCard Ace _)            = 1
getValue _                              = 0

-- Print out a formatted version of the card. For example,
-- Joker -> "Joker"
-- PlayingCard (NumberCard 5) Spade -> "5 of Spades"
-- PlayingCard Ace            Heart -> "Ace of Hearts"
showSuit :: Suit -> String 
showSuit Spades     = "Spades"
showSuit Clubs      = "Clubs"
showSuit Diamonds   = "Diamonds"
showSuit _          = "Hearts" 

showRank :: Rank -> String 
showRank Jack               = "Jack"
showRank Queen              = "Queen"
showRank King               = "King"
showRank Ace                = "Ace"
showRank (NumberCard num) = show num

showCard :: Card -> String
-- Option 1 (requires creation of showSuit and showRank functions)
-- showCard (PlayingCard rank suit)                = showRank rank ++ " of " ++ showSuit suit
--Option 2 (requires initializing instance of show)
showCard = show
