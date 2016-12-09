import Data.List

numbers s = map toInt $ filter (/= "") $ groupedDigits s

toInt s = read s :: Int

chars = "1234567890-"

groupedDigits :: String -> [String]
groupedDigits "" = []
groupedDigits s  = digits : (groupedDigits $ remainingDigits)
  where (digits, nonDigits) = span (`elem` chars) s
        remainingDigits = dropWhile (not . (`elem` chars)) nonDigits

main = do input <- readFile "../input"
          putStrLn "Sum of JSON Numbers in Input:"
          print $ sum $ numbers input
