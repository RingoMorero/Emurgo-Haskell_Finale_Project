module Animation.Render where


import Control.Monad.Trans.State.Strict (get)   -- | Combinator to obtain current state(used with StateT)
import Control.Monad.Trans.Reader (ask)         -- | Combinator to obtain the environment (used with ReaderT)
import Control.Monad.Trans.Class (lift)         -- | To unwrap Monad Transformers ("lift" from the "Monad Stack")

import Animation.Env (Env(..))
import Animation.State (St(..))
import Animation.Type (Animation,Object(..),Brick(..), GameStatus(..))

render :: Animation Env St ()           -- | type Animation env st a = 'ReaderT env (StateT st IO) a'
render = do
    val <- renderVal                    -- | renderVal :: Animation Env St String. No need to lift
    lift (lift (putStrLn val))          -- | putStrLn :: String -> IO (), so it is necessary to 'lift' it twice to the ReaderT level

renderVal :: Animation Env St String    -- | type Animation env st a = 'ReaderT env (StateT st IO) a'
renderVal = do
    env <- ask                          -- | Obtains values from Environment
                                        -- | 'ask' is on the 'ReaderT' level (no need to lift)
    st <- lift get                      -- | Obtains values from State
                                        -- | 'get' is combinator for StateT, so it is necessary to 'lift' it once to the ReaderT level
    return (renderInternal env st)      

-- | brings the information to render the game
renderInternal :: Env -> St -> String
renderInternal env st = makeBox (size        env) -- | Brings the size of the playing area. From 'defaultEnv' in 'Env.hs'
                                (baselength  env) -- | From 'defaultEnv' in 'Env.hs'
                                (bXPosition  st ) -- | Base position
                                (position    st ) -- | Ball position
                                (bricklength env) -- | From 'defaultEnv' in 'Env.hs'
                                (bricks      st ) -- | Bricks still on existence. Brick = Brick {brickPosition::(Int,Int), life::Int}
                                (status      st ) -- | GameStatus
                                (points      st ) -- | Score

-- | Definition of how each line is going to be rendered according to what is there.
-- | Oprions are:
-- |    1) Nothing 
-- |    2) Just bricks 
-- |    3) Just the ball
-- |    4) Just the base
-- |    5) The ball and bricks
-- |    6) The ball and the base
-- |    The base and bricks is not an option

-- | Creates each line of the game
makeLine :: Char          -- | Initial & end char
         -> Char          -- | Intermediates char
         -> Int           -- | Number of columns for intermediates char
         -> Maybe Object  -- | Ball. e.g. (Just (Ball 5))    --> (Just (Ball 'column'))
         -> Maybe Object  -- | Base e.g. (Just (Base 10 14)) --> Just (Base 'length' 'column')
         -> [Brick]       -- | e.g.: [Brick (17,6) 1, Brick (7,3) 0] --> [Brick (PosX, PosY) life]
         -> Int           -- | Length of the brick. e.g.: 3
         -> String        -- | The whole line
makeLine endChar innerChar i mb mba bricks bricklength =                            -- | 'endChar' is for "initial" and "end" char
    let positions = [0 .. i]                                                        -- | 'i' is the number of columns, the width or length of game playing area
        renderPixel x =                                                             -- | 'x' is 0,1,2,3..i
            case mb of                                                              -- | Checks whether the Ball is on that position
                Nothing -> case mba of                                              -- | Checks whether the Base is on that position
                               Nothing           -> printBlock x                    -- | There is no Base nor Ball on that position, then printBlock (Brick or 'innerChar')
                               Just (Base bl ba) -> if x `elem` [ba..(ba+bl)]       -- | Checks whether the Base is in the position
                                                                                    -- | 'bl' ==> base length and 'ba' ==> base position.
                                                    then ':'                        -- |   prints ':' (part of the Base) on that position
                                                    else innerChar                  -- |   otherwise prints the 'innerChar' (no Ball, no Base, no Bricks)
                Just (Ball b) -> case mba of                                        -- | If there is a Ball on the position, checks for the Base
                               Nothing           -> if x == b                       -- | If no Base, checks position of the Ball
                                                    then 'O'                        -- |   prints 'O' for the Ball 
                                                    else printBlock x               -- | If not Ball on that position, then printBlock (Brick or 'innerChar')
                               Just (Base bl ba) -> if x == b                       -- | Base is on the line. Checks if the Ball position is in the 'column #',
                                                    then 'O'                        -- |   prints 'O'
                                                    else if x `elem` [ba..(ba+bl)]  -- | If the 'column #' is part of the Base, 
                                                         then ':'                   -- |   print ':'
                                                         else innerChar             -- | No Ball nor Base on the 'column #'
     in [endChar] ++ map renderPixel positions ++ [endChar]                         -- | It represents the whole line
     
     -- | Finding if a brick should be the owner of a pixel considering its position and length
     -- | If True, it paints it according to the life of the brick
     
     where brickXPositions = map (fst . brickPosition) bricks                                            -- | Takes the 'x' position for each brick --> e.g.:[5,2,6,8]
           printBlock x    = if x `elem` foldl (\u v  -> u ++ [v..(v+bricklength-1)]) [] brickXPositions -- | Checks for the bricks position. Gives 'True' is there is a brick on that column
                             then if (life $ pixelOwner x) > 0 then '=' else '-'                         -- | Prints the brick type accordingly to the life
                             else innerChar                                                              -- | If no brick on that column, print 'innerChar'
           pixelOwner x    = head $ filter (\u -> x - fst (brickPosition u) < bricklength   
                                               && x - fst (brickPosition u) >= 0 ) bricks                -- | Checks for columns with bricks

makeBox :: (Int, Int)   -- | (# of Columns, # of Rows)
        -> Int          -- | length of the Base
        -> Int          -- | x position of the Base
        -> (Int, Int)   -- | (x position of the Ball, y position of the Ball)
        -> Int          -- | length of the bricks
        -> [Brick]      -- | e.g.: [Brick (17,6) 1, Brick (7,3) 0] --> [Brick (PosX, PosY) life]
        -> GameStatus   -- | i.e.: Paused or Playing or Stopped or LevelComplete or Restarted
        -> Int          -- | Cumulative score
        -> String
makeBox (numCols, numRows) baseL baseX (ballX, ballY) bricklength bricks status points =
    unlines 
        (["            BRICK BREAKER VIDEOGAME"] ++ [" "]
        ++ case status of                                                                           -- | GameStatus, i.e.: Paused or Playing or Stopped or LevelComplete or Restarted
              LevelComplete -> [celebratrionCartoon]                                                -- | When level is completed
              _             -> [   makeLine '-' '-' numCols Nothing Nothing [] bricklength ]        -- | Prints the top line of the playing area. No Base. No Ball.
                              ++   mappedPositions                                                  -- | Prints the inner lines of the playing area
                              ++ [ makeLine '-' '-' numCols Nothing Nothing [] bricklength ]        -- | Prints the top line of the playing area. No Base. No Ball.
                              ++ ["Status: " ++ show status                                         -- | GameStatus, i.e.: Paused or Playing or Stopped or LevelComplete or Restarted
                                 ++ if ballY /= numRows then   " | Score: " ++ show points          -- | If the Ball is not below the Base, print the score
                                    else  " | ***** GAME OVER ***** | Your Score is " ++ show points-- | If the Ball gets below the Base, print the 'GAME OVER' 
                                 ]
                              ++ -- Render menu according to status
                                 [ case status of                                                   -- | GameStatus, i.e.: Paused or Playing or Stopped or Restarted
                                     Stopped       -> "Press (R) to Restart"
                                     Paused        -> "Press (S) to Play | Controls: (A) Left / (D) Right"
                                     Playing       -> "(P) Pause / (Q) Stop / (A) Left / (D) Right"
                                     _             -> ""
                                 ]
                           -- | Uncomment these lines for debugging purposes 
--                            ++ [  "BaseX: " ++ show (baseX + div baseL 2) 
--                               ++ " | Ball: (" ++ show ballX ++ "," ++ show ballY ++ ")"
--                               ++ " | BallOverBase: "   ++ show (ballX >= baseX && (ballX <= (baseX + baseL)))
--                               ]
        )
    where   positions = [0 .. numRows]                  -- | Defines the values for 'y' in 'lineMaker'
            mappedPositions = map lineMaker positions   -- | Prints the left and right vertical limits of the playing area.

         -- | Painting lines depending on the position of the ball and position of the base given a Y position         
            lineMaker y =                                                                                                        -- | Here 'y' represent the 'row' being printed
              let brickYPositions = filter ((==) y . snd . brickPosition) bricks                                                 -- | Checks for bricks in each row
               in if y == ballY                                                                                                  -- | If True, means that the row 'y' contains the Ball
                    then if y == numRows - 1                                                                                     -- | 'numRow - 1' is the row where the Base is located. 
                         then makeLine '|' ' ' numCols (Just (Ball ballX)) (Just (Base baseL baseX)) brickYPositions bricklength -- | Prints the Ball and the Base
                         else makeLine '|' ' ' numCols (Just (Ball ballX)) Nothing                   brickYPositions bricklength -- | Prints the Ball and Bricks (if any)
                    else if y == numRows - 1                                                                                     -- | Implies that the Row does not contains the Ball
                         then makeLine '|' ' ' numCols Nothing             (Just (Base baseL baseX)) brickYPositions bricklength -- | Prints the Base
                         else makeLine '|' ' ' numCols Nothing             Nothing                   brickYPositions bricklength -- | Prints just the Bricks (if any)
            celebratrionCartoon =                                                                                                -- | Prints the celebration drawing
                                    "                        .-."
                                ++"\n                _.--¨¨¨¨.o/         .-.-._"
                                ++"\n             __'   .¨¨¨; {        _J ,__  `.       Level Complete"
                                ++"\n            ; o`.-.`._.'J;       ; /  `- /  ;"
                                ++"\n            `--i`¨. `¨ .';       `._ __.'   |     ¡CONGRATULATIONS!"
                                ++"\n                `  `¨¨¨   `         `;      :"
                                ++"\n                 `.¨-.     ;     ____/     /     Your Score: " ++ show points ++ " points"
                                ++"\n                   `-.`     `-.-'    `¨-..'"
                                ++"\n     ___              `;__.-'¨           `."
                                ++"\n  .-{_  `--._         /.-¨                 `-."
                                ++"\n /    ¨¨T    ¨¨---...'  _.-¨¨   ¨¨¨-.         `."
                                ++"\n;       /                 __.-¨¨.    `.         `,             _.."
                                ++"\n `     /            __.-¨¨       '.    `          `.,__      .'L' }"
                                ++"\n  `---¨`-.__    __.¨    .-.       j     `.         :   `.  .' ,' /"
                                ++"\n            ¨¨¨¨       /   `     :        `.       |     F' `   ;"
                                ++"\n                      ;     `-._,L_,-¨¨-.   `-,    ;     `   ; /"
                                ++"\n                       `.       |        `-._  `.__/_        `/"
                                ++"\n                         `     _;            `  _.'  `-.     /"
                                ++"\n                          `---¨ `.___,,      ;¨¨        `  .'"
                                ++"\n                                    _/       ;           `¨"
                                ++"\n      Bring me                   .-¨     _,-' "
                                ++"\n     more bricks!               {       ¨¨;            Next Level - Press SPACE"
                                ++"\n                                 ;-.____.'`."
                                ++"\n      I am not done yet!          `.  ` '.  :"
                                ++"\n                                    `  : : /"
                                ++"\n                                     `':/ `"
                                

                                {-     "\n                /`_.----._"
                                    ++"\n              .¨ _,=<'¨=. ¨,/|   Hey you did great."
                                    ++"\n             /,-'    ¨=.`.   (   You're almost as good as Sulley!"
                                    ++"\n            //         ` |    `"
                                    ++"\n           /,    _.,.   |      `    (|"
                                    ++"\n         ,¨ |   ,`'v/', |       `  _)("
                                    ++"\n        /   |   !>(¨)<|/         ` c_ `"
                                    ++"\n     _-/     `  '=,Z``7           . C. `"
                                    ++"\n _,-¨ V  /    '-._,>*¨     `      |   ` `"
                                    ++"\n `  <¨|  )` __ __ ____ _    Y     |    ` `"
                                    ++"\n  ` ` |   >._____________.< |     ¨-.   ` `"
                                    ++"\n   ` `|   ` `/`/`/`/`/`/ /' /    =_  '-._) `"
                                    ++"\n    ` (    `            /         |¨*=,_   /"
                                    ++"\n     ` `    `_/`/`/`/`_/         /      ¨¨¨"
                                    ++"\n     _).^     ¨******¨          /"
                                    ++"\n    (()!|`                     /"
                                    ++"\n     *==¨ ¨,                 ,¨"
                                    ++"\n            ¨,_            ,¨"
                                    ++"\n               `¨*<==> ,=*¨"
                                    ++"\n                ` ` / /"
                                    ++"\n            _____>_V /"
                                    ++"\n           f  .-----¨"
                                    ++"\n           |  `    ` `"
                                    ++"\n           |   `    ` '-."
                                    ++"\n           J    `    `   `"
                                    ++"\n          (  ` ` ` _.-J   )"
                                    ++"\n           `V)V)=*.','  ,'"
                                    ++"\n    jjs        (V(V)(V)/"-}
