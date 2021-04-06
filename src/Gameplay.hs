 -- Игровой процесс --
module Gameplay where
import Graphics.Gloss.Interface.Pure.Game
import Type
import Field
import Check
import Graphic 

-- загружаемый файл
filePath :: FilePath
filePath = "field.txt"

-- обновить поле (заглушка, тк поле изменяется только после обработки события)
update :: Float -> Field -> Field
update _ f = f 
  
-- изменение состояния поля
handleEvent :: Event -> Field -> Field 
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) f | mode f == Point = f {mode = Fill}
                                                        | mode f == Fill = f {mode = Point}                                                        
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) f | x >= 0 && x < width f && y >= 0 && y < height f = changeField f x y
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
  if ((checkInput (lines filecontent)) == False) then do
        putStrLn "Error"
    else do
        let board = readField (lines filecontent)
        print $ width board 
        print $ height board 
        play (display board) bgColor fps board drawGame handleEvent update
          where
            display f = InWindow "Japanese Crosswords" ((screenWidth f), (screenHeight f)) (0, 0)
            bgColor = white
            fps = 60

--  case checkInput filecontent of
 --   Nothing -> putStrLn "Parse error"
--    Just cfg -> do
--      play ...
