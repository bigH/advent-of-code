import Data.List

start = [1..3014387] `zip` repeat 1

next :: [(Int, Int)] -> [(Int, Int)]
next elves = remainder ++ map collapse (pairs collapsible)
  where len = length elves
        numCollpasible = len - (len `rem` 2)
        (collapsible, remainder) = splitAt numCollpasible elves

pairs :: [a] -> [[a]]
pairs [] = []
pairs (a : []) = error $ "Never expect odd number of elems."
pairs (a : b : t) = [a, b] : pairs t

collapse :: [(Int, Int)] -> (Int, Int)
collapse ((e, a) : (_, b) : []) = (e, a + b)
collapse pairs                  = error $ "Never more or less than 2 in pair: " ++ show pairs

answer = until ((== 1) . length) next start

main = do putStrLn "Elf that gets all the Gifts:"
          print $ fst $ head answer
