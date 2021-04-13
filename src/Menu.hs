  -- Меню --
module Menu where

-- менюшка
data Menu = Menu
    { games :: [String] -- список названий файлов
    , selected :: Maybe String -- выбранная головоломка
    , width :: Int -- ширина
    , height :: Int -- высота
    }

-- загружаемые файлы (списком)
files :: FilePath
files = ["./base/time.txt", "./base/house.txt", "./base/cup.txt", "./base/lambda.txt", "./base/rose.txt"]

-- получаем менюшку
readMenu :: Menu
readMenu = Menu
    { games = files
    , selected = Nothing
    , width = 800
    , height = 800
    }

-- для выбора файла
selectMenu :: Menu -> String -> Menu
selectMenu m f = m{selected = f}

-- взять i-ый файл
chooseMenu :: Menu -> Int -> String
chooseMenu m i = (games m) !! i
