module Model where

import System.Random

type PositionInGrid = (Int, Int)

data Direction
  = DirUp
  | DirLeft
  | DirRight
  | DirBottom
  deriving (Eq)

data Snake =
  Snake
    { snakeHeadPos :: PositionInGrid
    , snakeTailPos :: [PositionInGrid]
    , snakeHeadDir :: Direction
    }

-- Future work: possibly other foods (with special properties).
data Food =
  Food
    { foodPos :: PositionInGrid
    }

data Board =
  Board
    { boardSnake :: Snake
    , boardFood :: [Food]
    , boardWidth :: Int
    , boardHeight :: Int
    }

currentFood :: Board -> Food
currentFood = head . boardFood

isSnakeOutsideBoard :: Board -> Bool
isSnakeOutsideBoard b = sx < 0 || sx > bw || sy < 0 || sy > bh
  where
    (sx, sy) = snakeHeadPos $ boardSnake b
    (bw, bh) = (boardWidth b, boardHeight b)

isSnakeEatingFood :: Board -> Bool
isSnakeEatingFood b = snakeHeadPos bs == foodPos bf
  where
    (bs, bf) = (boardSnake b, currentFood b)

isSnakeEatingItself :: Board -> Bool
isSnakeEatingItself b = snakeHeadPos bs `elem` snakeTailPos bs
  where
    bs = boardSnake b

snakePositions :: Board -> [PositionInGrid]
snakePositions b = h' : t'
  where
    h' = snakeHeadPos bs
    t' = snakeTailPos bs
    bs = boardSnake b

moveSnakeAccordingToItsDirection :: Snake -> Snake
moveSnakeAccordingToItsDirection s = s {snakeHeadPos = h', snakeTailPos = t'}
  where
    ((hx, hy), dir) = (snakeHeadPos s, snakeHeadDir s)
    h'
      | dir == DirUp = (hx, hy - 1)
      | dir == DirLeft = (hx - 1, hy)
      | dir == DirRight = (hx + 1, hy)
      | dir == DirBottom = (hx, hy + 1)
    t' = (hx, hy) : (removeLast $ snakeTailPos s)

removeLast :: [a] -> [a]
removeLast = reverse . drop 1 . reverse

moveSnake :: Board -> Board
moveSnake b = b {boardSnake = bs'}
  where
    bs' = moveSnakeAccordingToItsDirection $ boardSnake b

stepBoard :: Board -> Board
stepBoard b
  | isSnakeEatingFood b' = newFood $ growSnake b
  | isSnakeEatingItself b' || isSnakeOutsideBoard b' = restartSnake b
  | otherwise = b'
  where
    b' = moveSnake b

newFood :: Board -> Board
newFood b = b {boardFood = dropWhile overlapsSnake (boardFood b)}
  where
    overlapsSnake f = foodPos f `elem` (snakePositions b)

growSnake :: Board -> Board
growSnake b = b {boardSnake = bs {snakeHeadPos = h', snakeTailPos = t'}}
  where
    bs = boardSnake b
    h' = foodPos $ currentFood b
    t' = snakeHeadPos bs : (snakeTailPos bs)

restartSnake :: Board -> Board
restartSnake b = b {boardSnake = Snake h t DirRight}
  where
    h = (3, 3)
    t = [(2, 3), (1, 3), (0, 3)]

changeSnakeDir :: Snake -> Direction -> Snake
changeSnakeDir s d = s {snakeHeadDir = d}
