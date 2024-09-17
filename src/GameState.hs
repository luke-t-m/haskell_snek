{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR))
import Data.Maybe (isJust)

import Data.List (unfoldr)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East
-- True
-- True
-- True
-- True


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint board seed = ((c, r), seed'')
  where (c, seed') = uniformR (1, width board) seed
        (r, seed'') = uniformR (1, height board) seed'

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}

-- really? << DON'T USE, BUGGY AND DELETES LINES IN SOURCE??
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 0
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> let (p, seed) = makeRandomPoint board_info (randomGen game_state1)
-- >>> p
-- >>> makeRandomPoint board_info seed



-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake p snek = snakeHead snek == p || isJust (S.findIndexL (==p) (snakeBody snek))

{-
This is a test for inSnake. It should return 
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

addPoints :: Point -> Point -> Point
addPoints (a, b) (c, d) = (a + c, b + d)

movementToPoint :: Movement -> Point
movementToPoint North = (-1, 0)
movementToPoint South = (1, 0)
movementToPoint East = (0, 1)
movementToPoint West = (0, -1)

wrap :: Integral a => a -> a -> a
wrap n ceil = mod (n + ceil -1) ceil + 1

wrapPoint :: Point -> BoardInfo -> Point
wrapPoint (c, r) board = (wrap c (width board), wrap r (height board))


-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead board state = wrapPoint (addPoints cur vec) board
  where vec = movementToPoint $ movement state
        cur = snakeHead $ snakeSeq state

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- True
-- True
-- True


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple board state = newApple' board state (randomGen state)
  where
    newApple' :: BoardInfo -> GameState -> StdGen -> (Point, StdGen)
    newApple' b s seed
      | p == applePosition state || inSnake p (snakeSeq state) = newApple' b s seed'
      | otherwise = (p, seed')
      where (p, seed') = makeRandomPoint b seed

{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 


snakeLast :: SnakeSeq -> Point
snakeLast snek
  | null $ snakeBody snek = snakeHead snek
  | otherwise = x
    where (_ :|> x) = snakeBody snek

moveSnake :: SnakeSeq -> Point -> SnakeSeq
moveSnake snek nHead = SnakeSeq nHead (snekhead :<| nSnekbod)
  where snekhead = snakeHead snek
        snekbod = snakeBody snek
        nSnekbod = S.take (length snekbod - 1) snekbod

lengthenSnake :: SnakeSeq -> Point -> SnakeSeq
lengthenSnake snek nHead = SnakeSeq nHead (snekhead :<| snekbod)
  where snekhead = snakeHead snek
        snekbod = snakeBody snek

move :: BoardInfo -> GameState -> (Board.RenderMessage , GameState)
move board state
  | nSnekhead == applePos = (appleMsg, appleState)
  | inSnake nSnekhead snek = (Board.GameOver, state)
  | otherwise = (hungryMsg, hungryState)
  where snek = snakeSeq state
        dir = movement state
        seed = randomGen state
        applePos = applePosition state

        snekhead = snakeHead snek
        nSnekhead = nextHead board state
        nEmpty = (snakeLast snek, Board.Empty)
        (nApplePos, seed') = newApple board state
        nApple = (nApplePos, Board.Apple)
        rMsg = [(snekhead, Board.Snake), (nSnekhead, Board.SnakeHead)]
        appleMsg = Board.RenderBoard $ nApple : rMsg
        hungryMsg = Board.RenderBoard $ nEmpty : rMsg
        hungryState = GameState (moveSnake snek nSnekhead) applePos dir seed
        appleState = GameState (lengthenSnake snek nSnekhead) nApplePos dir seed'

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,3),Empty),((1,1),Snake),((1,4),SnakeHead)]
-- RenderBoard [((2,4),Apple),((1,1),Snake),((2,1),SnakeHead)]
-- RenderBoard [((1,3),Empty),((1,1),Snake),((4,1),SnakeHead)]
