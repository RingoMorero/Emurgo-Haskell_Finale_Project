module Animation.Type where

 
import Control.Monad.Trans.Reader (ReaderT(..))                  -- | 'ReaderT' adds a read-only environment to the given monad
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT) -- | 'evalStateT' evaluates a state computation with the given initial 
                                                                 -- |   state and return the final value, discarding the final state.


type Animation env st a = ReaderT env (StateT st IO) a    -- | "Stack of Monads": Reader (State (IO))

data Object = Ball Int                                    -- | Ball 'location on the x axis' --> column
            | Base Int Int                                -- | Base 'Pad length' 'Pad location on the x axis'

data Brick =
  Brick
    { brickPosition :: (Int, Int)                         -- |{ brickPosition :: (PosX, PosY)
    , life          :: Int                                -- |if 0, it is destroyed 1st time the Ball touches the brick
    }
  deriving (Show, Eq)   -- I added the 'Show' to play with the code

data GameStatus = Paused                                  -- | No explanation required
                | Playing                                 -- | No explanation required
                | Stopped                                 -- | No explanation required
                | LevelComplete                           -- | No explanation required
                | Restarted                               -- | No explanation required
                deriving (Show)                           -- | No explanation required

data UserInput = MoveLeft                                 -- | No explanation required
               | MoveRight                                -- | No explanation required
               | Pause                                    -- | No explanation required
               | Stop                                     -- | No explanation required
               | Start                                    -- | No explanation required
               | Restart                                  -- | No explanation required

runAnimation :: env -> st -> Animation env st a -> IO a
runAnimation env st action = evalStateT (runReaderT action env) st
      -- | 'evalStateT' evaluates a state computation with the given initial 
      -- |   state and return the final value, discarding the final state.
