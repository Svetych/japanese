  -- Типы данных --
module Type where

data State = Filled | Pointed | Empty deriving (Show, Eq) -- состояние ячейки (закрашена или пустая)

data Mode = Fill | Point deriving (Show, Eq) -- ставим в поле точки или закрашиваем
convertMode :: Mode -> State
convertMode Fill = Filled
convertMode Point = Pointed

data Cell = Cell {current :: State, expected :: State} deriving Show -- ячейка (текущее, правильное)

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
