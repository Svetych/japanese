  -- Это main --

import Type
import Field

checkWin :: Field -> String
checkWin f | checkJumble f = "Winning!"
           | otherwise     = "Not yet, try harder."

--main
main ::  IO()
main = do
  filecontent <- readFile "field.txt"
  let board = readField (lines filecontent)
  print "Horizontal lines:"
  print (horline board)
  print "Vertical lines:"
  print (verline board)
  print (checkWin (changeField (changeField (changeField (changeField (changeField (changeField (changeField (changeField board 0 1) 0 2) 1 0) 1 2) 2 0) 2 1) 2 2) 0 0))

