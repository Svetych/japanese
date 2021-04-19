  -- Меню --
module Menu where

import System.FilePath.Posix
import Graphics.Gloss.Interface.Pure.Game
import Graphic

-- менюшка
data Menu = Menu
    { games :: [String] -- список названий файлов
    , selected :: Maybe String -- выбранная головоломка
    , widthM :: Int -- ширина
    , heightM :: Int -- высота
    , kostyl :: Int -- остановка работы
    }

-- загружаемые файлы (списком)
files :: [FilePath]
files = ["./base/time.txt", "./base/house.txt", "./base/cup.txt", "./base/lambda.txt", "./base/rose.txt"]

-- получаем менюшку
readMenu :: Menu
readMenu = Menu
    { games = files
    , selected = Nothing
    , widthM = 2 * indent + (1 + (length files)) * cellSize
    , heightM = 2 * indent + (1 + (length files)) * cellSize
    , kostyl = 0
    }

-- для выбора файла
selectMenu :: Menu -> String -> Menu
selectMenu m f = m {selected = Just f}

-- взять i-ый файл
chooseMenu :: Menu -> Int -> String
chooseMenu m i = (games m) !! i

-- поменять режимы
changeMenu :: Menu -> Int -> Menu
changeMenu m i = menu{kostyl = 1}
                 where menu = selectMenu m (chooseMenu m i)

-- отрисовать меню
drawMenu :: Menu -> Picture
drawMenu m = Translate (x) (y) (Pictures [menuLines m, menuNames m, menuTitle m])
           where
           x = - fromIntegral (widthM m)  / 2
           y = - fromIntegral (heightM m) / 2

-- отрисовать линии
menuLines :: Menu -> Picture
menuLines m = Pictures (map (\y -> Line [(i,y), (w-i,y)]) [l*c + i | l <- [0..h]])
            where
            i = fromIntegral indent
            w = fromIntegral (widthM m)
            h = fromIntegral (length (games m))
            c = fromIntegral cellSize

-- отрисовать названия
menuNames :: Menu -> Picture
menuNames m = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text) txt))
            where
            x = [i | l <- [1..h]]
            y = [i + l*c | l <- [0..h-1]]
            txt = map (takeBaseName) (games m)
            h = fromIntegral (length (games m))
            i = fromIntegral (indent + 5)
            c = fromIntegral cellSize
            compr = 0.15

-- отрисовать заголовок
menuTitle :: Menu -> Picture
menuTitle m = Translate (i) (y) (Color (red) (scale compr compr (Text "Japanese crosswords")))
          where
          y = fromIntegral ((heightM m) - indent - cellSize + 5)
          i = fromIntegral (indent * 2)
          compr = 0.12

-- удалить меню
delMenu :: Menu -> Picture
delMenu m = Color (white) (Polygon [(0,  0), (0,  h), (h, h), (h, 0)])
          where h = fromIntegral (heightM m)

-- убрать костыль
reduK :: Menu -> Menu
reduK m = m {kostyl = 0}
