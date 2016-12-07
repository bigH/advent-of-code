import Data.List

lengthUnescaped :: String -> Int
lengthUnescaped = lengthUnescaped' . drop 1 . reverse . drop 1 . reverse

lengthUnescaped' :: String -> Int
lengthUnescaped' s
  | length s == 0    = 0
  | take 1 s == "\\" = 1 + (lengthUnescaped' $ removeEscapedChar s)
  | otherwise        = 1 + (lengthUnescaped' $ drop 1 s)

removeEscapedChar :: String -> String
removeEscapedChar ('\\' : []) = error "Escape \\ followed by nothing."
removeEscapedChar ('\\' : '\\' :         rest) = rest
removeEscapedChar ('\\' : '\"' :         rest) = rest
removeEscapedChar ('\\' : 'x'  : _ : _ : rest) = rest
removeEscapedChar ('\\' : rest) = error "Unknown escape type \\ followed by '" ++ rest ++ "'"

main = do input <- readFile "../input"
          putStrLn "Total Chars of Input:"
          let total = sum $ map length $ lines input
          print $ total
          putStrLn ""
          putStrLn "Total Chars of Data:"
          let totalUnescaped = sum $ map lengthUnescaped $ lines input
          print $ totalUnescaped
          putStrLn ""
          putStrLn "Difference:"
          print $ total - totalUnescaped 
