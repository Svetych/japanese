 -- Игровой процесс --
module Gameplay where
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
import Type
import Field
import Check
import Graphic 
import Menu

-- файл с головоломкой
filePath :: Menu -> FilePath
filePath m = (checkMenu (selected m))

-- обновить поле (поменять таймер)
fieldUpdate :: Float -> Field -> Field
fieldUpdate _ f = (nextSec f)

-- заглушка, реагирует только на событие
menuUpdate :: Float -> Menu -> Menu
menuUpdate _ m = m

-- изменение состояния поля
handleGame :: Event -> Field -> Field 
handleGame (EventKey (SpecialKey KeySpace) Down _ _) f = changeMode f                                                        
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) f | x >= 0 && x < width f && y >= 0 && y < height f = changeField f y x
                                                              | otherwise = f
                                                                where
                                                                   x = mouseToCoordX mouse f
                                                                   y = mouseToCoordY mouse f
handleGame _ f = f  

-- Получить координаты клетки под мышкой
mouseToCoordX :: Point -> Field -> Int
mouseToCoordX (x, y) f = (floor (x + fromIntegral (screenWidth f) / 2) - (indent + getSize(lineSize f horline))) `div` cellSize

mouseToCoordY :: Point -> Field -> Int
mouseToCoordY (x, y) f = (height f) - (floor (y + fromIntegral (screenHeight f) / 2) - indent) `div` cellSize -1

-- изменение состояния меню
handleMenu :: Event -> Menu -> Menu 
handleMenu (EventKey (MouseButton LeftButton) Down _ mouse) m | y >= 0 && y < h = changeMenu m y
                                                              | otherwise = m
                                                                where
                                                                   h = length (games m)
                                                                   y = mouseToNum mouse m
handleMenu _ m = m 

mouseToNum :: Point -> Menu -> Int
mouseToNum (_, y) m = ((floor (y + fromIntegral (heightM m) / 2) - indent)) `div` cellSize

-------------
-- приложение
data App = App {menu :: Menu, field :: Field, err :: Bool}

makeApp :: Menu -> App
makeApp m = App {menu = m, field = makeField, err = False}

-- отрисовка
drawApp :: App -> Picture
drawApp a | err a = drawErr
          | kostyl (menu a) == 1 = delMenu (menu a)
          | regime (field a) == 2 = Pictures [delField (field a), drawMenu (menu a)]
          | otherwise = case (selected (menu a)) of
                        Nothing -> drawMenu (menu a)
                        Just fp -> drawGame (field a)

-- обработка
changeReg :: Event -> App -> App
changeReg (EventKey (MouseButton LeftButton) Down _ _) a = makeFKostyl a
changeReg _ a = a 

handleEvent :: Event -> App -> App
handleEvent eve a | kostyl (menu a) == 1 = makeMKostyl a
                  | regime (field a) == 1 = changeReg eve a
                  | otherwise = case (selected (menu a)) of
                                Nothing -> a {menu = handleMenu eve (menu a)}
                                Just fp -> a {field = handleGame eve (field a)}

-- обновление
appUpdate :: Float -> App -> App
appUpdate f a = case (selected (menu a)) of
                Nothing -> a{menu = menuUpdate f(menu a)}
                Just fp -> a{field = fieldUpdate f (field a)}

-- смена режимов работы приложения
makeMKostyl :: App -> App
makeMKostyl a = a{field = readField filecontent, menu = m, err = e}
             where
             m = reduK (menu a)
             filecontent = lines (unsafePerformIO (readFile (filePath (menu a))))
             e = case (checkInput filecontent) of
                   False -> True
                   True -> False
makeFKostyl :: App -> App
makeFKostyl a = a{field = f, menu = readMenu, err = False}
              where f = makeField {regime = 2}

-- надпись об ошибке
drawErr :: Picture
drawErr = Pictures [table, title] 
          where
          table = Color (greyN 0.9) (Polygon [(-100,  -25), (-100,  25), (100, 25), (100, -25)])
          title = Color (red) (scale compr compr (Text "ERROR!"))
          compr = 0.4

-- программа, что запускает игру и отрисовку поля
run :: IO ()
run = do
  let app = makeApp (readMenu)
  play FullScreen white 1 app drawApp handleEvent appUpdate
