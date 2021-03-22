data State = Filled | Empty -- состояние ячейки (закрашена или пустая)

data Mode = Fill | Point -- ставим в поле точки или закрашиваем

data Cell = Cell {cur :: State, exp :: State} -- ячейка (текущее, правильное)

type Grid = [[Cell]] -- игровое поле - матрица из ячеек
 
-- игровое поле
data Field = Field 
    { gamegrid :: Grid -- игровая сетка
    , jumble :: Int -- количество беспорядков
    , width :: Int -- ширина игрового поля в клетках
    , height :: Int -- высота игрового поля в клетках 
    , horline :: [[Int]] -- цифры сбоку от сетки
    , verline :: [[Int]] -- цифры сверху от сетки
    , mode :: Mode
    }

readField :: [String] -> Field --считывание игровой сетки из txt файла
readField x = Field
    { gamegrid = makeGrid x
    , jumble = makeJumble x
    , width = length (head x)
    , height = lenght x
    , horline = makeLine x
    , verline = makeLine (transpose x)
    , mode = Fill
    }

makeGrid :: [String] -> Grid -- сделать из файла поле

 -- посчитать в файле беспорядки
makeJumble :: [String] -> Int
makeJumble x = foldr (+) 0 (map (countFilled) x)

countFilled :: String -> Int
countFilled x = foldr (+) 0 (makeNum x)

 -- сделать из строки массив цифр
makeNum :: String -> [Int]
makeNum = map (read . pure :: Char -> Int)

 -- сделать массив цифр сбоку/сверху от сетки
makeLine :: [String] -> [[Int]]
makeLine x = map countLines (map (makeNum) x)

countLines :: [Int] -> [Int]
countLines [] = []
countLines (0:x:s) = countLines (x:s)
countLines (x:0:s) = [x] ++ countLines (s)
countLines (x:y:s) = countLines ((x+y):s)
countLines (x:[]) | x > 0     = [x]
                  | otherwise = []

 -- транспонирование матрицы строк
transpose :: [String] -> [String]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

changeСell :: Field -> Int -> Int -> Field -- воздействие на поле (изменяем состяние gamegrid[i][j] и поле jumble)

checkJumble :: Field -> bool -- проверить поле: jumble = 0 => победа

-- функция нахождения координат [i] [j]

--main
main ::  IO()
main = do
  filecontent <- readFile "field.txt"
  gamefield <- readField lines filecontent
