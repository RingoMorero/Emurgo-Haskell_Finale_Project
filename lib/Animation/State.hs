module Animation.State where

import Control.Monad.Trans.State.Strict (get, put) -- | Combinators to obtain current state and to put a new state (used with StateT)
                                                   -- | In this version, sequencing of computations is strict (but computations are not 
                                                   -- |   strict in the state unless you force it with seq or the like).
                                                   -- |   For a lazy version with the same interface, see Control.Monad.Trans.State.Lazy.
import Control.Monad.Trans.Reader (ask)            -- | Combinator to obtain the environment (used with ReaderT)
import Control.Monad.Trans.Class (lift)            -- | To unwrap Monad Transformers ("lift" from the "Monad Stack")
import System.IO (hReady                           -- | hReady :: Handle -> IO Bool. Checks whether there is input available from hdl
                  , Handle(..)
                  , stdin                          -- | stdin :: Handle. Manages the input during initialization
                  , hSetEcho                       -- | hSetEcho :: Handle -> Bool -> IO ()
                  , hSetBuffering                  -- | hSetBuffering :: Handle -> BufferMode -> IO ().
                  , BufferMode(NoBuffering))       -- | NoBuffering: output is written immediately, and never stored in the buffer

import Animation.Env (Env(..))
import Animation.Type ( Animation
                      , Brick(..)
                      , GameStatus(..)
                      , UserInput(..)
                      )

data Direction
    = Positive
    | Negative
    | Neutral

directionFromInt :: Int -> Direction      -- | Used to define the random position and direction of the Ball
directionFromInt 0 = Neutral
directionFromInt 1 = Positive
directionFromInt 2 = Negative
directionFromInt _ = error "Boooooo....."

directionToMultiplier :: Direction -> Int -- | Used to define the new position and direction of the Ball
directionToMultiplier Positive =  1
directionToMultiplier Negative = -1
directionToMultiplier Neutral  =  0

data St =
    St
        { position     :: (Int, Int)               -- | Ball position
        , direction    :: (Direction, Direction)   -- | Ball direction
        , bXPosition   :: Int                      -- | Base position
        , bricks       :: [Brick]                  -- | from Type: data Brick = Brick {brickPosition::(Int,Int), life::Int}
        , points       :: Int                      -- | No explanation required
        , status       :: GameStatus               -- | from Type: Paused / Playing / Stopped / LevelComplete / Restarted
        
}

-- | Allocation of the list of reduced positions in the game
-- | A reduced position is a 'x' value divided by the brick length
-- | Positions in this function are a list of 'x positions'. This means that
-- | given width = 50 then positions 49, 50, 51, 52,... correspond to points (49,0), (50,0), (1,1), (2,1),...
bricksInPlace :: Int -> [Int] -> Int -> Int -> [Brick]
bricksInPlace width positions life bricklength = map (\x -> Brick (findPosition (bricklength*x) width 0) life) positions
           where findPosition x width level = if x < width then (x,level) else findPosition (x - width) width (level + 1)

defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) 0 [] 0 Stopped     -- | Used to start the game

-- | Management of the input of the user in a handy way. Pendant to solve reaction delay when holding key.
getKey :: IO [Char]
getKey = reverse <$> getKey' ""                             -- | The Functor unwraps the 'IO' context to apply the function
  where getKey' chars = do                                  -- | No explanation required
          char <- getChar                                   -- | No explanation required
          more <- hReady stdin                              -- | stdin :: Handle. Manages the input during initialization
                                                            -- | hReady :: Handle -> IO Bool. Checks whether there is input available from hdl
                                                            -- | so, more :: Bool
          (if more then getKey' else return) (char:chars)   -- | Checks whether there is more input

getUserInput :: IO (Maybe UserInput)
getUserInput = go Nothing
        where go a = do
                hSetBuffering stdin NoBuffering       -- | No Buffer allowed
                                                      -- | NoBuffering: output is written immediately, and never stored in the buffer
                                                      -- | hSetBuffering :: Handle -> BufferMode -> IO (). Disable mode of buffering 
                hSetEcho stdin False                  -- | hSetEcho :: Handle -> Bool -> IO (). 
                                                      -- |   Set the echoing status to False
                ready <- hReady stdin                 -- | ready = True if there is an input
                if not ready then return Nothing   
                else do                               -- | If there is an input, do this
                      hSetBuffering stdin NoBuffering -- | No Buffer allowed
                      string <- getKey                -- | Unwrap the 'IO' from the 'IO String'
                      let condition = (==) (head string) in -- | takes the first input from the String
                             if condition 'a' || condition 'A'                  then return (Just MoveLeft)    -- | No explanation required
                        else if condition 'd' || condition 'D'                  then return (Just MoveRight)   -- | No explanation required
                        else if condition 'p' || condition 'P'                  then return (Just Pause)       -- | No explanation required
                        else if condition 'q' || condition 'Q'                  then return (Just Stop)        -- | No explanation required
                        else if condition 's' || condition 'S'                  then return (Just Start)       -- | No explanation required
                        else if condition 'r' || condition 'R' || condition ' ' then return (Just Restart)     -- | No explanation required
                        else return Nothing                                                                    -- | No input available

next :: Animation Env St ()               -- | type Animation env st a = 'ReaderT env (StateT st IO) a'
next = do
    env    <- ask                         -- | Assign information from the environment to 'env'. This is fixed information.
                                          -- | It is not necessary to 'lift' ask because we are in the ReaderT environment
    input  <- lift $ lift getUserInput    -- | Assign the 'getUserInput' to 'input'
                                          -- | 'getUserInput' :: IO, so it is necessary to 'lift' it twice to the ReaderT level
    prevSt <- lift get                    -- | Obtains the current state and assign it to 'prevSt'
                                          -- | 'get' is combinator for StateT, so it is necessary to 'lift' it once to the ReaderT level
                                          
    lift (put (nextInternal env input prevSt))  -- | 'put' is combinator for StateT, so it is necessary to 'lift' it once

nextInternal :: Env -> Maybe UserInput -> St -> St                                                       -- | First 'St' is the 'prevSt' state. Second 'St' will be the new state
nextInternal (Env (width, height) velocity baselength bricklength _ _ _ )                                -- | This is fixed information from environment
             userInput                                                                                   -- | from getUserInput
             prevSt@(St (prevX, prevY) (prevXDir, prevYDir) prevBXPos prevBricks prevPoints prevStatus)  -- | save the State as prevSt
             =
    
 -- | Management of next state according to GameStatus and UserInput
   
    case prevStatus of                                                     -- | No explanation required
        Paused        -> case userInput of                                 -- | No explanation required
                           Just Start -> prevSt {status = Playing}         -- | No explanation required
                           Just Stop  -> prevSt {status = Stopped}         -- | No explanation required
                           _          -> prevSt                            -- | NO explanation required
        Stopped       -> case userInput of                                 -- | No explanation required
                           Just Restart -> prevSt {status = Restarted}     -- | No explanation required
                           _            -> prevSt                          -- | No explanation required
        LevelComplete -> case userInput of                                 -- | No explanation required
                           Just Restart -> prevSt {status = Restarted}     -- | No explanation required
                           _            -> prevSt                          -- | No explanation required
        Playing       -> if prevBricks /= [] then                          -- | If there are any brick left
                            case userInput of                              -- | No explanation required
                               Just Stop     -> prevSt {status = Stopped}  -- | No explanation required
                               Just Pause    -> prevSt {status = Paused }  -- | No explanation required
                               Just MoveLeft ->                            -- | Base should move to the left
                                 St 
                                   { position   = (newX, newY)             -- | Calculates the new position for the Ball
                                   , direction  = (newXDir, newYDir)       -- | Calculates the new position for the Ball
                                   , bXPosition = newBXPosition (-2)       -- | Calculates the new position for the Base
                                   , bricks     = newBricks                -- | Update bricks. Check for collision
                                   , points     = newPoints                -- | No explanation required
                                   , status     = newStatus                -- | Checks whether the Ball got the bottom limit
                                   }
                               Just MoveRight ->                           -- | Base should move to the right
                                 St 
                                   { position   = (newX, newY)             -- | Calculates the new position for the Ball
                                   , direction  = (newXDir, newYDir)       -- | Calculates the new position for the Ball
                                   , bXPosition = newBXPosition (2)        -- | Calculates the new position for the Base
                                   , bricks     = newBricks                -- | Update bricks. Check for collision
                                   , points     = newPoints                -- | No explanation required
                                   , status     = newStatus                -- | Checks whether the Ball got the bottom limit
                                   }
                               _              -> 
                                 St                                        -- | Base should stay on same position
                                   { position   = (newX, newY)             -- | Calculates the new position for the Ball
                                   , direction  = (newXDir, newYDir)       -- | Calculates the new position for the Ball
                                   , bXPosition = prevBXPos                -- | Keeps the previous position for the Base
                                   , bricks     = newBricks                -- | Update bricks. Check for collision
                                   , points     = newPoints                -- | No explanation required
                                   , status     = newStatus                -- | Checks whether the Ball got the bottom limit
                                   }
                         else prevSt {status = LevelComplete }             -- | if no brick are left, then level complete!

-- TODO: the Restart GameStatus should be indicated !!!!!!!         
    where

 -- | New_Unbounded tells us which would be the position of the ball if there is no bound (Very handy)
                                                   -- | directionToMultiplier :: Direction -> Int - (Positive = 1, Negative = -1, Neutral = 0)
                                                   -- | prevXDir :: Direction - (Positive, Negative, Neutral)
    newXUnbounded          = prevX + directionToMultiplier prevXDir * velocity   -- | Explained by Gerard on title
                                                                                 -- | TODO: if velocity > 1, then it could jump bricks
    newYUnbounded          = prevY + directionToMultiplier prevYDir * velocity   -- | Explained by Gerard on title
                                                                                 -- | TODO: if velocity > 1, then it could jump bricks
                                                
 -- | Detection of collision with the base
    baseCollision          = prevBXPos <= newX                                   -- | Check that Ball is right from the left border of the Base
                           && prevBXPos + baselength >= newX                     -- | Check that Ball is left from the right border of the Base
                           && newYUnbounded >= height - 2                        -- | Check taht Ball is not below the Base
                                                                                 -- | TODO: change the comparative sign to '='
 
 -- | Auxiliary functions to consider the length of a brick, not just their position
 -- | completePositions returns a list of occupied positions given a list of Bricks
    addPositions (u,v) brl = zip [u .. (u + brl - 1)] $ take brl $ repeat v                     -- | Explained by Gerard
    completePositions      = foldl (\x y -> x ++ addPositions (brickPosition y) bricklength) [] -- | Explained by Gerard
    
 -- | Identification of the coordinate that will be impacted according to ball direction for three 
 -- | different cases: Collision with top or botton (brickCollisionY), collision with one side (brickCollisionX)   
 -- | or collision with a corner (cornerCollision)
    targetX                = ( newXUnbounded + directionToMultiplier prevXDir, newY)   -- | Calculates position (x,y) to check collision on one side
    targetY                = ( newX, newYUnbounded + directionToMultiplier prevYDir)   -- | Calculates position (x,y) to check collision on top or bottom
    cornerTarget           = ( newXUnbounded + directionToMultiplier prevXDir          -- | Calculates position (x,y) to check collision with a corner
                             , newYUnbounded + directionToMultiplier prevYDir )
    brickCollisionY        = elem targetY $ completePositions prevBricks         -- | = True if the Ball hits the Brick on top or bottom
    brickCollisionX        = elem targetX $ completePositions prevBricks         -- | = True if the Ball hits the Brick on one side of it
    cornerCollision        = not brickCollisionX && not brickCollisionY          -- | = True if the Ball hits the Brick with one corner
                           && elem cornerTarget (completePositions prevBricks)
    
 -- | Identification of the block that will be hit.
    targetBlockY           = identify targetY      prevBricks                    -- | Brick that is hit by the Ball, if any, on top or bottom
    targetBlockX           = identify targetX      prevBricks                    -- | Brick that is hit by the Ball, if any, on side
    targetBlockC           = identify cornerTarget prevBricks                    -- | Brick that is hit by the Ball, if any, on a corner

 -- | Filters the only brick that is hit by the ball given a target position and a list of Bricks.
    identify target        = head . filter (\u -> snd target == snd (brickPosition u)                 -- | Ball and Brick have same 'y-axis' position
                                               && fst target -  fst (brickPosition u) < bricklength   -- | Compares Ball 'x-axis' position to right side of Brick
                                               && fst target -  fst (brickPosition u) >= 0          ) -- | Compares Ball 'x-axis' position to left side of Brick
    
 -- | Update positions and directions for next state
    
    newX =                                         -- | New position of the Ball on the x-axis
        case prevXDir of                           -- | Checks the former x-position of the Ball
            Neutral  ->     newXUnbounded          -- | 'newXUnbounded' calculates the new position of the Ball on x-axis
            Positive -> min newXUnbounded width    -- | it does not allow the Ball to get out of the playing area (right limit)
                                                   -- | 'width' is # columns from playing area
            Negative -> max newXUnbounded 0        -- | It does not allow the Ball to get out of the playing area (left limit)

    newY =                                         -- | New position of the Ball on the y-axis
        case prevYDir of                           -- | Checks the former y-position of the Ball
            Neutral  ->     newYUnbounded          -- | 'newYUnbounded' calculates the new position of the Ball on y-axis
            Positive -> min newYUnbounded height   -- | it does not allow the ball to get out of the playing area (bottom limit)
                                                   -- | 'height' is # rows from playing area
            Negative -> max newYUnbounded 0        -- | It does not allow the Ball to get out of the playing area (top limit)

    newXDir =                                      -- | New direction of the Ball on the x-axis
        case prevXDir of                           -- | Checks the former direction of the Ball
            Neutral  -> Neutral                    -- | If Neutral, keeps Neutral
            Positive ->                            -- | Ball goes from left to righ
                if newXUnbounded >= width          -- | Ball gets outisde the right limit from playing area
                   || brickCollisionX              -- | Ball hits a brick
                   || cornerCollision              -- | Ball hits the corner of the playing area
                then Negative                      -- | Ball changes direction on x-axis
                else Positive                      -- | Ball changes direction on x-axis
            Negative ->                            -- | Ball goes from righ to left
                if newXUnbounded <= 0              -- | Ball gets outisde the left limit from playing area
                   || brickCollisionX              -- | Ball hits a brick
                   || cornerCollision              -- | Ball hits the corner of the playing area
                then Positive                      -- | Ball changes direction on x-axis
                else Negative                      -- | Ball keeps direction on x-axis

    newYDir =                                      -- | New direction of the Ball on the y-axis
        case prevYDir of                           -- | Checks the former direction of the Ball
            Neutral  -> Neutral                    -- | If Neutral, keeps Neutral
            Positive ->                            -- | Ball goes from bottom to top
               if brickCollisionY                  -- | Ball hits a brick
               || cornerCollision                  -- | Ball hits the corner of the playing area
               || baseCollision                    -- | Ball hits the Base
               then Negative                       -- | Ball changes direction
               else Positive                       -- | Ball keeps direction
            Negative ->                            -- | Ball goes from top to bottom
                if newYUnbounded <= 0              -- | It does not allow the Ball to get out of the playing area (bottom limit)
                   || brickCollisionY              -- | Ball hits a brick
                   || cornerCollision              -- | Ball hits the corner of the playing area
                then Positive                      -- | Ball changes direction on y-axis
                else Negative                      -- | Ball keeps direction on y-axis
    
 -- | Position control of the base limited by the width
    
    newBXPosition i = let newBxPos = prevBXPos + i          -- | Calculates new position for the Base
                       in if newBxPos + baselength > width  -- | Base gets out of right limit 
                          then prevBXPos                    -- | Mantains the previous position
                          else if newBxPos <= 0             -- | Base gets out of left limit 
                               then 0                       -- | Mantains the previous position
                               else newBxPos                -- | Defines new position for the Base
    
 -- | Update status in case the player is unable to bounce back the ball
    newStatus = if newY /= height then Playing else Stopped -- | Ball gets the bottom limit. So, Game Over
 
 -- | Update the score in case of any brick collision
    newPoints = (+) prevPoints $ fromEnum $ brickCollisionY || brickCollisionX || cornerCollision  -- | Adds 1 if at least one collision is made

 -- | Update the bricks state according to collisions. Brick disappears if life = 0
    
    newBricks   -- | Case 1: Collision in Y axis AND X axis (Two bricks at the same time)
              | brickCollisionX && brickCollisionY                      -- | If Brick is hit, check for life to define its existence
                                = changeBricks targetBlockY $ changeBricks targetBlockX prevBricks
             
                -- | Case 2: Collision in Y axis
              | brickCollisionY = changeBricks targetBlockY prevBricks  -- | If Brick is hit, check for life to define its existence
             
                -- | Case 3: Collision in X axis
              | brickCollisionX = changeBricks targetBlockX prevBricks  -- | If Brick is hit, check for life to define its existence
             
                -- | Case 4: Collision with a corner
              | cornerCollision = changeBricks targetBlockC prevBricks  -- | If Brick is hit, check for life to define its existence
             
                -- | Case 5: No collision
              | otherwise = prevBricks                                  -- | If no Brick is hit, no action
 
 -- | Update of the life of the bricks
                                                                                                      -- | data Brick = Brick { brickPosition :: (Int, Int), life :: Int }
    changeBricks x bricks = let brickTail  = filter ((/=) (brickPosition x) . brickPosition) bricks   -- | Checks brick positions not hit by the Ball
                                brickHurt  = Brick (brickPosition x) (life x - 1)                     -- | Reduces life on one unit to the Brick on that position
                             in if life x > 0 then brickHurt : brickTail else brickTail               -- | If the Brick did not reach life == 0, then add to the Brick List