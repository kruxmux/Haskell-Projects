module Cards where

import Test.QuickCheck
import System.Random

-- A card has a rank and belongs to a suit.

data Card = Card { rank :: Rank, suit :: Suit }
            deriving (Eq, Show)
---------------------------------------------------
-- This part is for quickCheck. You are not required to
-- understand this part (yet).

instance Arbitrary Card where
  arbitrary = do
    suit <- arbitrary
    rank <- arbitrary
    return (Card rank suit)

-- All the different suits.

data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

instance Arbitrary Suit where
  arbitrary = elements [ Hearts, Spades, Diamonds, Clubs ]

-- A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.

data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

instance Arbitrary Rank where
  arbitrary = frequency [ (4, elements [Jack,Queen,King,Ace])
                        , (9, do n <- choose (2, 10)
                                 return (Numeric n))
                        ]
instance Arbitrary Deck where
  arbitrary = do cs <- arbitrary
                 return (Deck cs)

-- A generator of infinite lists of numbers from 0.0 to 1.0
-- Needed for assignment F

data Rand = Rand [Double]
            deriving (Show)

instance Arbitrary Rand where
  arbitrary = do
       infList <- infiniteListOf $ choose (0.0, 1.0)
       return (Rand infList)


-- end of quickCheck part.
---------------------------------------------------------------

-- A hand of cards
type Hand = [Card]

-- A Deck of cards is defined as a new data type
-- so we don't accidentally mix up a deck and a hand

data Deck = Deck {cards :: [Card]}
            deriving (Eq, Show)

-- The size of a hand. Note that we could have used the function length.
size :: Num a => Hand -> a
size []           = 0
size (card:hand)  = 1 + size hand
