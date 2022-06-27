module Main where

import Animation
    ( Animation
    , Direction(..)     -- | from State: data Direction = Positive | Negative | Neutral
    , Env(..)           -- | from Env: defaultEnv :: Env = Env {size::(Int, Int), velocity::Int, baselength::Int, bricklength::Int, numOfBricks::Int, posOfBricks::[Int], lifes::Int}
    , St(..)            -- | St {position::(Int, Int), direction::(Direction, Direction), bXPosition::Int, bricks::[Brick], points::Int, status::GameStatus}
    , defaultEnv        -- | Env { size= (50, 10), velocity=1, baselength= 10, bricklength= 3, numOfBricks= 0, posOfBricks= [], lifes= 1 }
    , defaultSt         -- | St (0, 0) (Neutral, Neutral) 0 [] 0 Stopped  
    , next              -- | Brings next state
    , render            -- | Updates drawing
    , runAnimation      -- | From 'Type.hs' - runAnimation :: env -> st -> Animation env st a -> IO a
    , directionFromInt  -- | 0 = Neutral; 1 = Positive; 2 = Negative ; _ = error "Boooooo....."
    , bricksInPlace     -- | Allocation of the list of reduced positions in the game
    )
  
import Control.Concurrent (threadDelay)             -- | To create delay on the computation
import System.Random (randomRIO)                    -- | To create randomness
import Data.List (nubBy)                            -- | Used to create the Brick positions
import Control.Monad.Trans.State.Strict (put, get)  -- | Combinators to obtain current state and to put a new state (used with StateT)
import Control.Monad.Trans.Reader (ask)             -- | Combinator to obtain the environment (used with ReaderT)
import Control.Monad.Trans.Class (lift)             -- | To unwrap Monad Transformers ("lift" from the "Monad Stack")


--import Type (GameStatus(..))
import Animation.Type (GameStatus(..))

--ghci> :t lift
--  :: (Control.Monad.Trans.Class.MonadTrans t, Monad m) => m a -> t m a

putInitialState :: Animation Env St ()  -- | type Animation env st a = ReaderT env (StateT st IO) a                                        
putInitialState = do
    (Env (width, height) _ baselength bricklength _ _ lifes) <- ask         -- | Brings information from module: e.g.: Env (Env (50, 10) 1 10 3 0 [] 1)
                                                                            -- | Env (size,velocity,baselength,bricklength,numOfBricks,posOfBricks,lifes)
                                                                            -- | It is not necessary to 'lift' ask because we are in the ReaderT environment 
    posX <- lift $ lift $ randomRIO (div width  3, (*) 2 $ div width  3)    -- | Randomly defines Ball position on 'x-axis' in the 33%-66% of the width. (→1)
    posY <- lift $ lift $ randomRIO (div height 3, (*) 2 $ div height 3)    -- | (→1) Randomly defines Ball position on 'y-axis' in the 33%-66% of the height. (→1)
    dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)          -- | (→1) Randomly defines Ball direction on 'x-axis' (Direction datatype: 1 is Positive, 2 is Negative). (→1)
    dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)          -- | (→1) Randomly defines Ball direction on 'y-axis' (Direction datatype: 1 is Positive, 2 is Negative). (→1)
                                                                            -- | (1→)'randomRIO' :: (Int,Int) -> IO (Int), so it is necessary to 'lift' it twice to the ReaderT level

 -- | Creation of a random number of blocks limited by a desired maximum number. 
    randNumBlocks  <- let 
                        maxDesiredBlocks = div (width * (height - 4)) (bricklength * 4) -- | e.g.: if width==50 & height==10 & bricklength==3 , then result == 50
                      in lift $ lift $ randomRIO (0, maxDesiredBlocks)                  -- | Creates a random number between 0 and maxDesiredBlocks
                                                                                        -- | 'randomRIO' :: (Int,Int) -> IO (Int), so it is necessary to 'lift' it twice to the ReaderT level

--  IS IT NECESSARY THIS LINE?? OR IT IS REPEATED ???
 -- | Creation of a random number of blocks limited by a desired maximum number.
--    randNumBlocks  <- let maxDesiredBlocks = div (width * (height - 4)) (bricklength * 4) in lift $ lift $ randomRIO (0, maxDesiredBlocks)

 -- | Creation of a list of DIFFERENT positions. The range of available positions has to be divided by the bricklength so
 -- | we can introduce the bricklength space afterwards in order to get bricks not to overlap
    randRedDistBlocks <- fmap (nubBy (==)) $ sequence $ replicate randNumBlocks $ randomRIO (1, div (width * (height - 4)) bricklength :: Int)
    lift $ put $ St (posX, posY) (dirX, dirY) (div (width - baselength) 2) (bricksInPlace width randRedDistBlocks lifes bricklength) 0 Paused

 -- | Management of the animation. Restarted state interrupts it.
animate :: Animation Env St ()          -- | type Animation env st a = ReaderT env (StateT st IO) a / "Stack of Monads": Reader (State (IO))
animate = do
    render                              -- | From Render.hs - render :: Animation Env St () - No need to lift
    event <- lift get                   -- | Obtains St {position::(Int, Int), direction::(Direction, Direction), bXPosition::Int, bricks::[Brick], points::Int, status::GameStatus}
                                        -- | 'get' is combinator for StateT, so it is necessary to 'lift' it once to the ReaderT level
    case (status event) of              -- | Checks for the value
        Restarted -> putInitialState    -- | If Restarted, so start over
        _         -> next               -- | Performs next function, from state 
    lift $ lift $ threadDelay 500000    -- | Set the delay required to perform calculations
                                        -- | Since 'threadDelay' :: Int -> IO (), we need to 'lift' it twice to the ReaderT level
    animate                             -- | Calls itself into an infinite loop


mainAnimation :: Animation Env St ()    -- | type Animation env st a = ReaderT env (StateT st IO) a / "Stack of Monads": Reader (State (IO))
mainAnimation = do
    putInitialState                     -- | Defines random position & direction of the Ball and randomly creates the Bricks
    animate                             -- | Call animate, which is infinite loop of states

main :: IO ()
main = do
    runAnimation defaultEnv defaultSt mainAnimation    -- | Calls 'runAnimation' with default values

