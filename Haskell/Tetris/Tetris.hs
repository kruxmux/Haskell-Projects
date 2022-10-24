-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Test.QuickCheck
--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (v,p) w _) = prop_Shape p && wellSize == shapeSize w


-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls (S list) = S (addWalls' list)

-- | Using the packed up shape to create walls in the rows
addWalls' :: [Row] -> [Row]
addWalls' list = addTopBot (map (addSides) list)

-- | Adds walls to the sides
addSides :: Row -> Row
addSides list = [Just Black] ++ list ++ [Just Black]

-- | Adds walls to the top and bottom
addTopBot :: [Row] -> [Row]
addTopBot list = add ++ list ++ add
  where add = [ (replicate (length (head list)) (Just Black)) ]


-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls (combine (shiftShape v p) w)


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = [allShapes !! shapeIndex x | x <- rs]
    shapeIndex x  = (floor (x * fromIntegral(length allShapes)))


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris MoveDown t  = tick t
stepTetris MoveLeft t  = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece (1) t)
stepTetris Rotate t    = Just (0, rotatePiece t)
stepTetris _ t         = tick t

-- | Move the shape in the tetris well, according to the given vector
move :: Vector -> Tetris -> Tetris
move x (Tetris (v,p) w s) = Tetris ((vAdd v x),p) w s

-- | Controls whether it's ok to use move. i.e there will be no colliding shapes
movePiece :: Int -> Tetris -> Tetris
movePiece n tetris | collision(move (n,0) tetris) = tetris
                   | otherwise                    = move (n,0) tetris

-- | Moves a shape down a step in the well, if it is not at the bottom
tick :: Tetris -> Maybe (Int,Tetris)
tick tetris | collides  = dropNewPiece tetris
            | otherwise = Just (0, (move (0,1) tetris))
  where collides = collision tetris

-- | Checks if a falling shape is at the edges/bottom. And if the shape overlaps with something in the well
collision :: Tetris -> Bool
collision (Tetris (v,p) w _) = (0 > fst(v))
                            || (fst(shapeSize p) + fst(v)) > fst(wellSize)
                            || (snd(shapeSize p) + snd(v)) >= snd(wellSize)
                            || overlaps (place (vAdd v (0,1),p)) w -- Add one with vAdd to see if the next tick will make it overlap

-- | Updates the falling shape with its' rotated version
rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w s) = Tetris (v, rotateShape p) w s

-- | Controls whether it's ok to use rotate. i.e there will be no colliding shapes
rotatePiece :: Tetris -> Tetris
rotatePiece tetris | collision (rotate tetris) = tetris
                   | otherwise                 = rotate tetris

-- | Places piece in well / creates a new piece / checks collision which could end game / removes filled rows
dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v,p) w (s:ss))  | collision newTetris = Nothing
                                      | otherwise           = Just (n, newTetris)
  where newTetris    = Tetris (startPosition,s) newWell ss
        (n, newWell) = clearLines (combine (place (v,p)) w)


-- | Seperates the list of rows by their truth value, and return the non completed rows along with the amount of rows removed.
clearLines :: Shape -> (Int,Shape)
clearLines c = (lineDiff, shiftShape (0,lineDiff) (S [b | (a,b) <- (clearLines' c) , a == False]))
  where lineDiff = length [b | (a,b) <- (clearLines' c) , a == True]

-- | Zip rows together with their corresponding isComplete value, which tells us if a row is full or not
clearLines' :: Shape -> [(Bool, Row)]
clearLines' (S r) = zip (map (isComplete) r) r

-- | Checks if a row is filled with non empty squares
isComplete :: Row -> Bool
isComplete r = length r == (length(filter (/= Nothing) r))
