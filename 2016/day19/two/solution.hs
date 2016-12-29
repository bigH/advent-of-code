import Data.List

num = 3014387

start elves = end ++ begin
  where (begin, end) = splitAt (length elves `quot` 2) elves

begin as = if odd num
             then case as of
                    (a:b:c) -> remainder ++ b : reduced
                      where (reduced, remainder) = kill c
                    (a:[]) -> [a]
             else reduce as

answer = until ((== 1) . length) reduce $ begin $ start $ [1 .. num]

reduce :: [Int] -> [Int]
reduce (a:[]) = [a]
reduce (a:b:[]) = [b]
reduce (a:b:c:[]) = [c]
reduce as = remainder ++ reduced
  where (reduced, remainder) = kill as

kill :: [Int] -> ([Int], [Int])
kill ([]) = ([], [])
kill (a:[]) = ([], a:[])
kill (a:b:[]) = ([], a:b:[])
kill (a:b:c:t) = (c : reduced, remainder)
  where (reduced, remainder) = kill t

main = do putStrLn "Elf that gets all the Gifts:"
          print $ head answer
