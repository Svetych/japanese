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
drawGame f = Translate (x) (y) (Pictures [drawGrid f, drawLines f, drawNums f]) --
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
           vl = map (\x -> Line [(x,i+h*c), (x,dy+i+h*c)]) [l*c + i + dx| l <- [0..w]]
           dx = fromIntegral (getSize(lineSize f horline))
           dy = fromIntegral (getSize(lineSize f verline))
           w = fromIntegral (width f)
           h = fromIntegral (height f)
           c = fromIntegral cellSize
           i = fromIntegral indent

-- отрисовать числа
drawNums :: Field -> Picture
drawNums f = Pictures [hl, vl]
           where
           hl = drawNum (makeX wh 1 hor) (makeY h (-c) hor) (makeList hor)
           vl = drawNum (makeY w c ver) (makeX wv (-1) ver) (makeList ver)
           wh = fromIntegral (getSize(lineSize f horline -1)) + i
           wv = fromIntegral (getSize (height f)) + i
           hor = horline f   
           ver = verline f
           h = fromIntegral (getSize (height f -1))
           w = fromIntegral (getSize (lineSize f horline))
           i = fromIntegral (indent) * 1.5
           c = fromIntegral cellSize
           
           makeList :: [[Int]] -> [Int]
           makeList [] = []
           makeList x = head x ++ makeList (tail x)
           
makeX :: Float -> Float -> [[Int]] -> [Float]
makeX _ _ [] = []
makeX m k x = [m - k*(l - j)*c | j <- [1..l]] ++ (makeX m k (tail x))
          where
          l = fromIntegral (length (head x))
          c = fromIntegral cellSize

makeY :: Float -> Float -> [[Int]] -> [Float]
makeY _ _ [] = []
makeY h c x = replicate l (i + h) ++ (makeY (h+c) c (tail x))
          where
          l = length (head x)
          i = fromIntegral (indent) * 1.5

drawNum :: [Float] -> [Float] -> [Int] -> Picture
drawNum x y l = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text . show) l))
              where compr = 0.12

-- закрашиваем клетку
drawMark :: Float -> Float -> State -> Picture
drawMark x y m | m == Filled = Translate (x) (y) (Polygon [(1,  2), (1,  a+1), (a, a+1), (a, 2)])
               | m == Pointed = Translate (x) (y) (Pictures [(drawMark 0 0 Empty), (Line [(1, 2), (a, a+1)]), (Line [(1, a+1), (a, 2)])])
               | m == Empty = Translate (x) (y) (Color (white) (Polygon [(1, 2), (1,  a+1), (a, a+1), (a, 2)]))
               where a = fromIntegral (cellSize - 2)


  -- То, что пойдет в модуль по обработке событий:
  
-- изменение состояния поля (пока заглушка)
handleEvent :: Event -> Field -> Field
handleEvent _ f = f

-- программа, что запускает игру и отрисовку поля
run :: IO ()
run = do
  filecontent <- readFile filePath
  let board = readField (lines filecontent)
--  print (drawNums board)
  play (display board) bgColor fps board drawGame handleEvent update
  where
    display f = InWindow "Japanese Crosswords" ((screenWidth f), (screenHeight f)) (0, 0)
    bgColor = white
    fps = 60

--  case checkInput filecontent of
 --   Nothing -> putStrLn "Parse error"
--    Just cfg -> do
--      play ...
