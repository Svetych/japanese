 -- Отрисовка игрового поля --
module Graphic where

import Graphics.Gloss.Interface.Pure.Game
import Type
import Field
import Check

-- загружаемый файл
filePath :: FilePath
filePath = "field.txt"

-- размер клетки в пикселях
cellSize :: Int
cellSize = 30

-- отступ в пискселях
indent :: Int
indent = 10

-- размер поля с числами
lineSize :: Field -> (Field -> [[Int]]) -> Int
lineSize f x = maximum ( map length (x f))

-- пеерсчитать размер в клетках
getSize :: Int -> Int
getSize x = x * cellSize

-- ширина экрана в пикселях
screenWidth :: Field -> Int
screenWidth f = getSize (width f + lineSize f horline) + 2*indent

-- высота экрана в пикселях
screenHeight :: Field -> Int
screenHeight f = getSize (height f + lineSize f verline) + 2*indent

-- обновить поле (заглушка, тк поле изменяется только после обработки события)
update :: Float -> Field -> Field
update _ f = f

-- отрисовать поле
drawGame :: Field -> Picture
drawGame f = Translate (x) (y) (Pictures [drawGrid f, drawLines f]) --, drawNums f
           where
           x = - fromIntegral (screenWidth f)  / 2
           y = - fromIntegral (screenHeight f) / 2

-- отрисовать сетку
drawGrid :: Field -> Picture
drawGrid f = Pictures (hl ++ vl)
           where
           hl = map (\y -> Line [(i,y), (i+dx,y)]) [l*c + j | l <- [0..h]]
           vl = map (\x -> Line [(x,j), (x,dy+j)]) [l*c + i | l <- [0..w]]
           i = fromIntegral (indent + (getSize(lineSize f horline)))
           j = fromIntegral indent
           dx = fromIntegral (getSize(width f))
           dy = fromIntegral (getSize(height f))
           w = fromIntegral (width f)
           h = fromIntegral (height f)
           c = fromIntegral cellSize

-- отрисовать поля для чисел
drawLines :: Field -> Picture
drawLines f = Pictures (hl ++ vl)
           where
           hl = map (\y -> Line [(i,y), (i+dx,y)]) [l*c + i | l <- [0..h]]
           vl = map (\x -> Line [(x,i+w*c), (x,dy+i+w*c)]) [l*c + i + dx| l <- [0..w]]
           i = fromIntegral indent
           dx = fromIntegral (getSize(lineSize f horline))
           dy = fromIntegral (getSize(lineSize f verline))
           w = fromIntegral (width f)
           h = fromIntegral (height f)
           c = fromIntegral cellSize

-- отрисовать числа
--drawNums :: Field -> Picture
--drawNums f = Pictures (hl ++ vl)
--           where
--           hl = map (\y -> Text [(i,y), (i+dx,y)]) [l*c + i | l <- [0..h]]
--           vl = map (\x -> Text [(x,i+w*c), (x,dy+i+w*c)]) [l*c + i + dx| l <- [0..w]]
--           i = fromIntegral indent
--           dx = fromIntegral (getSize(lineSize f horline))
--           dy = fromIntegral (getSize(lineSize f verline))
--           w = fromIntegral (width f)
--           h = fromIntegral (height f)
--           c = fromIntegral cellSize

drawNum :: [Int] -> [Float] -> [Float] -> Picture
drawNum l x y = Pictures (myzip (Translate) x y (map (scale compr compr . Text . show) l))
              where compr = 0.15

myzip :: (a->b->c->d) -> [a] -> [b] -> [c] -> [d]
myzip f (a:as) (b:bs) (c:cs) = (f a b c) : myzip f as bs cs
myzip _ _ _ _ = []

  -- То, что пойдет в модуль по обработке событий:
  
-- изменение состояния поля (пока заглушка)
handleEvent :: Event -> Field -> Field
handleEvent _ f = f

-- программа, что запускает игру и отрисовку поля
run :: IO ()
run = do
  filecontent <- readFile filePath
  let board = readField (lines filecontent)
  play (display board) bgColor fps board drawGame handleEvent update
  where
    display f = InWindow "Japanese Crosswords" ((screenWidth f), (screenHeight f)) (0, 0)
    bgColor = white
    fps = 60

--  case checkInput filecontent of
 --   Nothing -> putStrLn "Parse error"
--    Just cfg -> do
--      play ...
