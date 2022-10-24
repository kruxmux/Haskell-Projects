-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes =
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape (x, y) = S (replicate y (replicate x Nothing))


-- ** A02
-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (S list) = (length (list !! 0), length list)


-- ** A03
-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S rs) = length (filter (/= Nothing) (concat rs))


-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S (r:rs)) = (length (r:rs) >= 1)
                      && (length r >= 1)
                      && isRect (r:rs) (length (r:rs)-1)


isRect :: [Row] -> Int -> Bool
isRect list 0     = True
isRect list index = length (list !! index) == length (list !! (index-1))
                    && isRect list (index-1)

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = oneof [ return Black
                , return Red
                , return Green
                , return Yellow
                , return Blue
                , return Purple
                , return Cyan
                , return Grey ]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape =
  do c <- choose(1,6)
     return (allShapes !! c)


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S list) = (S (transpose reversedList))
  where reversedList = reverse list


-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (x, y) shape = shiftVert y (shiftHori x shape)

------- Vertical -------
shiftVert :: Int -> Shape -> Shape
shiftVert n (S list) = S (shiftVert' n list)

shiftVert' :: Int -> [Row] -> [Row]
shiftVert' 0 list = list
shiftVert' n list = replicate (length (head list)) Nothing : shiftVert' (n-1) list


------- Horizontal -------
shiftHori :: Int -> Shape -> Shape
shiftHori n (S list) = S (shiftHori' n list)

shiftHori' :: Int -> [Row] -> [Row]
shiftHori' n list = map (appendNoth n) list

appendNoth :: Int -> Row -> Row
appendNoth 0 list = list
appendNoth n list = Nothing : appendNoth (n-1) list


-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (a,b) shape = padHori a (padVert b shape)

------- Vertical -------
padVert :: Int -> Shape -> Shape
padVert n (S list) = S (reverse (padVert' n list))

padVert' :: Int -> [Row] -> [Row]
padVert' 0 list = reverse list
padVert' n list = replicate (length (head list)) Nothing : padVert' (n-1) list

------- Horizontal -------
padHori :: Int -> Shape -> Shape
padHori n (S list) = S (padHori' n list)

padHori' :: Int -> [Row] -> [Row]
padHori' n list = map (++ (replicate n Nothing)) list


-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (x,y) shape
   | (x >= first) && (y >= second)   = padShape (x-first,y-second) shape
   | x >= first                      = padHori (x-first) shape
   | y >= second                     = padVert (y-second) shape
   |otherwise                        = shape
      where first  = fst(shapeSize shape)
            second = snd(shapeSize shape)

-- * Comparing and combining shapes

-- ** A11

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps (S list1) (S list2) = overlaps' list1 list2

overlaps' :: [Row] -> [Row] -> Bool
overlaps' _ [] = False
overlaps' [] _ = False
overlaps' (r:rs) (w:ws) = zipOverlaps (zip r w) || overlaps' rs ws

zipOverlaps :: [(Square,Square)] -> Bool
zipOverlaps [] = False
zipOverlaps ((a,b):rest) = (a /= Nothing && b /= Nothing) || zipOverlaps rest

-- ** A12
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S []) _ = S []
zipShapeWith f _ (S []) = S []
zipShapeWith f (S list1) (S list2) = S (zipWith (zipWith f) list1 list2)


-- ** A13
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.

combine :: Shape -> Shape -> Shape
combine s1 s2 = zipShapeWith combine' shape1 shape2
   where shape1 = padShapeTo (shapeSize s1) s2
         shape2 = padShapeTo (shapeSize s2) s1

combine' :: Square -> Square -> Square
combine' (Just square1) (Nothing) = Just square1
combine' (Nothing) (Just square2) = Just square2
combine' (Nothing) (Nothing)      = Nothing
combine' _ _                      = error "Combine: overlapping shapes"
