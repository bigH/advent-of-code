import Data.List

input = "3113322113"

next :: String -> String
next = concatMap digits . group

digits :: [Char] -> String
digits group = (show $ length group) ++ take 1 group

main = do putStrLn "Length of Coded:"
          print $ length $ (iterate next input) !! 40
