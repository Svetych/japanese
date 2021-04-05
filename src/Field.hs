 -- Взаимодействие с игровым полем --
module Field where

import Type

 -- считывание игровой сетки из txt файла
readField :: [String] -> Field
readField x = Field
    { gamegrid = makeGrid x
    , jumble = makeJumble x
    , width = length (head x)
    , height = length x
    , horline = makeLine x
    , verline = makeLine (transpose x)
    , mode = Fill
    }

 -- сделать из строки массив цифр
makeNum :: String -> [Int]
makeNum = map (read . pure :: Char -> Int)

 -- сделать из файла поле
makeGrid :: [String] -> Grid
makeGrid x = map (fillLine)(map (makeNum) x)
           where fillLine = map (makeCell)

makeCell :: Int -> Cell
makeCell 1 = Cell {current = Empty, expected = Filled}
makeCell 0 = Cell {current = Empty, expected = Empty}

 -- посчитать в файле беспорядки
makeJumble :: [String] -> Int
makeJumble x = foldr (+) 0 (map (countFilled) x)
             where countFilled :: String -> Int
                   countFilled x = foldr (+) 0 (makeNum x)

 -- проверить поле: jumble = 0 => победа
checkJumble :: Field -> Bool
checkJumble f = (jumble f) == 0

 -- сделать массив цифр (сбоку/сверху от сетки)
makeLine :: [String] -> [[Int]]
makeLine x = map countLines (map (makeNum) x)

countLines :: [Int] -> [Int]
countLines [] = []
countLines (0:x:s) = countLines (x:s)
countLines (x:0:s) = [x] ++ countLines (s)
countLines (x:y:s) = countLines ((x+y):s)
countLines (x:[]) | x > 0     = [x]
                  | otherwise = []

 -- транспонирование матрицы
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

 -- воздействие на поле (изменяем состяние gamegrid[i][j] и поле jumble)
changeField :: Field -> Int -> Int -> Field
changeField f i j = f{gamegrid = (take i (gamegrid f)) ++ (fst pair), jumble = (jumble f) + (snd pair)}
                 where pair = (changeGrid (gamegrid f) (mode f) i j)

changeGrid :: Grid -> Mode -> Int -> Int -> (Grid, Int)
changeGrid [[]] _ _ _= ([[]], 0)
changeGrid (x:xs) mode i j | i == 0 = ([(take j x) ++ (fst pair)] ++ xs, (snd pair))
                           | otherwise = changeGrid xs mode (i - 1) j
                           where pair = (changeCell x mode j)

changeCell :: [Cell] -> Mode -> Int -> ([Cell], Int)
changeCell [] _ _ = ([], 0)
changeCell (x:xs) mode j | j == 0 = ([(fst pair)] ++ xs, (snd pair))
                         | otherwise = changeCell xs mode (j - 1)
                         where pair = (changeState x (convertMode mode))

changeState :: Cell -> State -> (Cell, Int)
changeState c st | current c == st = case expected c of
                                     Filled -> (c{current = Empty}, 1)
                                     Empty -> (c{current = Empty}, -1)
                 | otherwise = case expected c of
                               Filled -> (c{current = st}, -1)
                               Empty -> (c{current = st}, 1)


 -- воздействие на поле (изменяем режим mode)
changeMode :: Field -> Mode -> Field
changeMode f m = f{mode = m}
