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