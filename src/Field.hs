-- Взаимодействие с игровым полем --
module Field where

import Type

 -- считывание игровой сетки из txt файла
readField :: [String] -> [String] -> Int -> Int -> Int -> Field
readField x y j n t = Field
    { gamegrid = makeGrid x y
    , jumble = makeJumble x j
    , width = length (head x)
    , height = length x
    , horline = makeLine x
    , verline = makeLine (transpose x)
    , mode = Fill
    , numberfield = n
    , timer = t
    }

 -- сделать из строки массив цифр
makeNum :: String -> [Int]
makeNum = map (read . pure :: Char -> Int)

 -- сделать из файла поле
makeGrid :: [String] -> [String] -> Grid
makeGrid x y = map (fillLine) (zip (map (makeNum) x) (map (makeNum) y))
           where fillLine = map (makeCell)

makeCell :: (Int, Int) -> Cell
makeCell (1, x) case expected of
                0 -> Cell {current = Empty, expected = Filled}
                1 -> Cell {current = Filled, expected = Filled}
                2 -> Cell {current = Pointed, expected = Filled}
makeCell (0, x) case expected of
                0 -> Cell {current = Empty, expected = Empty}
                1 -> Cell {current = Filled, expected = Empty}
                2 -> Cell {current = Pointed, expected = Empty}

 -- посчитать в файле беспорядки
makeJumble :: [String] -> Int -> Int
makeJumble x 0 = foldr (+) 0 (map (countFilled) x)
             where countFilled :: String -> Int
                   countFilled x = foldr (+) 0 (makeNum x)
makeJumble _ j = j

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
                 where pair = (changeGrid (gamegrid f) (mode f) (jumble f) i j)

changeGrid :: Grid -> Mode -> Int -> Int -> Int -> (Grid, Int)
changeGrid [[]] _ _ _ _= ([[]], 0)
changeGrid [] _ _ _ _= ([[]], 0)
changeGrid (x:xs) mode jum i j | i == 0 = ([(take j x) ++ (fst pair)] ++ xs, (snd pair))
                               | otherwise = changeGrid xs mode jum (i - 1) j
                                 where pair = (changeCell x mode jum j)

changeCell :: [Cell] -> Mode -> Int -> Int -> ([Cell], Int)
changeCell [] _ _ _ = ([], 0)
changeCell (x:xs) mode jum j | j == 0 = ([(fst pair)] ++ xs, (snd pair))
                             | otherwise = changeCell xs mode jum (j - 1)
                              where pair = (changeState x (convertMode mode) jum)

changeState :: Cell -> State -> Int -> (Cell, Int)
changeState c st jum | jum == 0 = (c, 0)
                     | current c == st && st == Pointed = (c{current = Empty}, 0)
                     | current c == st = case expected c of
                                     Filled -> (c{current = Empty}, 1)
                                     Empty -> (c{current = Empty}, -1)
                     | st == Filled = case expected c of
                                  Filled -> (c{current = st}, -1)
                                  Empty -> (c{current = st}, 1)
                     | current c == Filled = case expected c of
                                         Filled -> (c{current = st}, 1)
                                         Empty -> (c{current = st}, -1)
                     | otherwise = (c{current = st}, 0)


 -- воздействие на поле (изменяем режим mode)
changeMode :: Field -> Field
changeMode f | mode f == Point = f {mode = Fill}
             | mode f == Fill = f {mode = Point}

-- увеличить таймер на 1 секунду
nextSec :: Field -> Field
nextSec f = f {timer = (curTime) + 1}
          where curTime = (timer f)
