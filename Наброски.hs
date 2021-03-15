data State = Filled | Empty -- состояние ячейки

data Cell = (Maybe State, Maybe State) -- ячейка (текущее, правильное)
data Grid = [[Cell]]
 
-- игровое поле
data Field = Field 
    { gamegrid :: Grid -- игровая сетка
    , jumble :: Int -- количество беспорядков
    , width :: Int -- ширина игрового поля в клетках
    , height :: Int -- высота игрового поля в клетках 
    , horline :: [[Int]] -- цифры сбоку от сетки
    , verline :: [[Int]] -- цифры сверху от сетки
    }

readField :: [String] -> Field --считывание игровой сетки из txt файла

makeNum :: String -> [Int] -- сделать из строки массив цифр

makeHorline :: [String] -> [[Int]] -- сделать массив цифр сбоку от сетки

makeVerline :: [String] -> [[Int]] -- сделать массив цифр сверху от сетки

changecell :: Field -> Int -> Int -> Field -- воздействие на поле (изменяем состяние gamegrid[i][j] и поле jumble)

checkjumble :: Field -> bool -- проверить поле: jumble = 0 => победа


--main
main ::  IO()
main = do
  filecontent <- readFile "field.txt"
  readField lines filecontent
  


