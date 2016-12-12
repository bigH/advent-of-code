import Data.List
import Data.Char

asInt :: String -> Int
asInt s = read s :: Int

decompress :: String -> String
decompress "" = ""
decompress s @ (h:t)
  | h /= '('  = [h] ++ decompress t
  | otherwise = repeats ++ decompress remainder
    where (chars, inInstruction) = span isDigit t
          (times, closeInstruction) = span isDigit $ drop 1 inInstruction
          rest = drop 1 closeInstruction
          numChars = asInt chars
          numTimes = asInt times
          (repeater, remainder) = splitAt numChars rest
          repeats = concat $ replicate numTimes repeater

main = do input <- readFile "../input"
          putStrLn "Decompressed Length"
          print $ length $ decompress $ strip input
