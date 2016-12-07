import Data.List

lengthEscaped :: String -> Int
lengthEscaped s = 2 + (lengthEscaped' s)

lengthEscaped' :: String -> Int
lengthEscaped' (                        []) = 0
lengthEscaped' ('\\' :                  []) = error "Escape \\ followed by nothing."
lengthEscaped' ('\\' : '\\' :         rest) = 4 + lengthEscaped' rest
lengthEscaped' ('\\' : '\"' :         rest) = 4 + lengthEscaped' rest
lengthEscaped' ('\\' : 'x'  : _ : _ : rest) = 5 + lengthEscaped' rest
lengthEscaped' ('\"' :                rest) = 2 + lengthEscaped' rest
lengthEscaped' (_    :                rest) = 1 + lengthEscaped' rest

main = do input <- readFile "../input"
          putStrLn "Total Chars of Input:"
          let total = sum $ map length $ lines input
          print total
          putStrLn ""
          putStrLn "Total Chars of Escaped Data:"
          let totalEscaped = sum $ map lengthEscaped $ lines input
          print totalEscaped
          putStrLn ""
          putStrLn "Difference:"
          print $ totalEscaped - total
