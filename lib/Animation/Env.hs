module Animation.Env where

import Animation.Type (UserInput(..))

                                            -- |This creates the Environment for the game to be played. The initial Setup
data Env =
    Env
        { size               :: (Int, Int)  -- | ('columns' , 'rows') (width, height) - Playing area of the game
        , velocity           :: Int         -- | velocity of the Ball. The "jump" of the Ball in each State
        , baselength         :: Int         -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , bricklength        :: Int         -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , numOfBricks        :: Int         -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , posOfBricks        :: [Int]       -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , lifes              :: Int         -- | (1 + # of times) the Ball has to hit the brick to destroy it.
        }

defaultEnv :: Env
defaultEnv =                            -- | Set global values for the game 
    Env { size               = (50, 10) -- | ('columns' , 'rows') - Playing area of the game
        , velocity           = 1        -- | velocity of the Ball. The "jump" of the Ball in each State
        , baselength         = 10       -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , bricklength        = 3        -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , numOfBricks        = 0        -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , posOfBricks        = []       -- | IT DOES NOT REQUIRE COMMENTS. CODE IS CLEAR ENOUGH
        , lifes              = 1        -- | (1 + # of times) the Ball has to hit the brick to destroy it.
        }
        
