module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Model
import System.Random
import View

viewData =
  ViewData
    { tileWidth = 32
    , tileHeight = 32
    , gridWidth = 10
    , gridHeight = 10
    , snakeHeadColor = blue
    , snakeTailColor = black
    , foodColor = red
    }

initialSnake =
  Snake
    { snakeHeadPos = (3, 3)
    , snakeTailPos = [(2, 3), (1, 3), (0, 3)]
    , snakeHeadDir = DirRight
    }

mkRandomFoodList :: StdGen -> [Food]
mkRandomFoodList g = Food (x, y) : (mkRandomFoodList g'')
  where
    (x, g') = randomR (0, gridWidth viewData - 1) g
    (y, g'') = randomR (0, gridHeight viewData - 1) g'

mkInitialBoard :: StdGen -> Board
mkInitialBoard g =
  Board
    initialSnake
    (mkRandomFoodList g)
    (gridWidth viewData)
    (gridHeight viewData)

-- "steps-per-second"
sps :: Int
sps = 5

main :: IO ()
main = do
  gen <- newStdGen
  play
    (InWindow "Snake" (getWindowSize viewData) (10, 10))
    white
    sps
    (mkInitialBoard gen)
    (drawBoard viewData)
    handleEvent
    (const stepBoard)

handleEvent :: Event -> Board -> Board
handleEvent e b =
  case e of
    EventKey (SpecialKey KeyUp) Down _ _ -> b {boardSnake = chdir DirUp}
    EventKey (SpecialKey KeyDown) Down _ _ -> b {boardSnake = chdir DirBottom}
    EventKey (SpecialKey KeyLeft) Down _ _ -> b {boardSnake = chdir DirLeft}
    EventKey (SpecialKey KeyRight) Down _ _ -> b {boardSnake = chdir DirRight}
    _ -> b
  where
    chdir = changeSnakeDir (boardSnake b)
