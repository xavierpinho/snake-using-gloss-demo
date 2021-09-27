module View where

import Graphics.Gloss
import Model

data ViewData =
  ViewData
    { tileWidth :: Int
    , tileHeight :: Int
    , gridWidth :: Int
    , gridHeight :: Int
    , snakeHeadColor :: Color
    , snakeTailColor :: Color
    , foodColor :: Color
    }

windowWidth :: ViewData -> Int
windowWidth v = tileWidth v * gridWidth v

windowHeight :: ViewData -> Int
windowHeight v = tileHeight v * gridHeight v

getWindowSize :: ViewData -> (Int, Int)
getWindowSize v = (windowWidth v, windowHeight v)

-- | Translates a picture into a grid position
translate' :: ViewData -> PositionInGrid -> Picture -> Picture
translate' v (gx, gy) = Translate x' y'
  where
    tw = fromIntegral $ tileWidth v
    th = fromIntegral $ tileHeight v
    ww = fromIntegral $ windowWidth v
    wh = fromIntegral $ windowHeight v
    x' = fromIntegral gx * tw - ww / 2 + tw / 2
    y' = wh / 2 - th / 2 - fromIntegral gy * th

drawSquare :: ViewData -> PositionInGrid -> Picture
drawSquare v (gx, gy) = translate' v (gx, gy) $ rectangleSolid tw th
  where
    tw = fromIntegral $ tileWidth v
    th = fromIntegral $ tileHeight v

drawSnake :: ViewData -> Snake -> Picture
drawSnake v s = Pictures (h : t)
  where
    h = Color (snakeHeadColor v) $ drawSquare v (snakeHeadPos s)
    t = map (Color (snakeTailColor v) . drawSquare v) (snakeTailPos s)

drawFood :: ViewData -> Food -> Picture
drawFood v f = Color (foodColor v) $ drawSquare v (foodPos f)

drawBoard :: ViewData -> Board -> Picture
drawBoard v b = Pictures [s, f]
  where
    s = drawSnake v (boardSnake b)
    f = drawFood v (currentFood b)
