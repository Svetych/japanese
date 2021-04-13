 -- Игровой процесс --
module Gameplay where
import Graphics.Gloss.Interface.Pure.Game
import Type
import Field
import Check
import Graphic 

-- файл с головоломкой
filePath :: FilePath
filePath = "field.txt"

-- файл с сохранением
fileSave :: FilePath
fileSave = "save.txt"

-- обновить поле (поменять таймер)
update :: Float -> Field -> Field
update _ f = (nextSec f)
  
-- изменение состояния поля
handleEvent :: Event -> Field -> Field 
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) f = changeMode f                                                        
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) f | x >= 0 && x < width f && y >= 0 && y < height f = changeField f y x
                                                               | otherwise = f
                                                                 where
                                                                   x = mouseToCoordX mouse f
                                                                   y = mouseToCoordY mouse f
handleEvent _ f = f  

-- Получить координаты клетки под мышкой
mouseToCoordX :: Point -> Field -> Int
mouseToCoordX (x, y) f = (floor (x + fromIntegral (screenWidth f) / 2) - (indent + getSize(lineSize f horline))) `div` cellSize

mouseToCoordY :: Point -> Field -> Int
mouseToCoordY (x, y) f = (height f) - (floor (y + fromIntegral (screenHeight f) / 2) - indent) `div` cellSize -1

-- программа, что запускает игру и отрисовку поля
run :: IO ()
run = do
  filecontent <- readFile filePath
  saves <- readFile fileSave
  if ((checkInput (lines filecontent)) == False || (checkSave (lines saves)) == False) then do
        putStrLn "Error"
    else do
        let board = readField (lines filecontent) (lines saves) 0 1 0
        play (display board) bgColor fps board drawGame handleEvent update
          where
            display f = InWindow "Japanese Crosswords" ((screenWidth f), (screenHeight f)) (0, 0)
            bgColor = white
            fps = 1

--  case checkInput filecontent of
--   Nothing -> putStrLn "Parse error"
--    Just cfg -> do
--      play ...
