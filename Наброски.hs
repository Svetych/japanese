data State = Filled | Empty -- состояние ячейки

data Mode = Cross | Point

data Cell = {Cur State, Exp State} -- ячейка (текущее, правильное)

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

makeGrid :: [String] -> Grid -- сделать из файла поле

makeNum :: String -> [Int] -- сделать из строки массив цифр

makeHorline :: [String] -> [[Int]] -- сделать массив цифр сбоку от сетки

makeVerline :: [String] -> [[Int]] -- сделать массив цифр сверху от сетки

changeСell :: Field -> Int -> Int -> Field -- воздействие на поле (изменяем состяние gamegrid[i][j] и поле jumble)

checkJumble :: Field -> bool -- проверить поле: jumble = 0 => победа

-- функция нахождения координат [i] [j]

--main
main ::  IO()
main = do
  filecontent <- readFile "field.txt"
  readField lines filecontent
