import Data.List
import Data.Char
import Data.String.Utils

asInt :: String -> Int
asInt s = read s :: Int

-- TOO SLOW
-- decompress :: String -> String
-- decompress "" = ""
-- decompress s @ (h:t)
  -- | h /= '('  = [h] ++ decompress t
  -- | otherwise = repeats ++ decompress remainder
    -- where (chars, inInstruction) = span isDigit t
          -- (times, closeInstruction) = span isDigit $ drop 1 inInstruction
          -- rest = drop 1 closeInstruction
          -- numChars = asInt chars
          -- numTimes = asInt times
          -- (repeater, remainder) = splitAt numChars rest
          -- repeats = concat $ replicate numTimes $ decompress repeater

decompress :: String -> Int
decompress "" = 0
decompress s @ (h:t)
  | h /= '('  = 1 + decompress t
  | otherwise = repeats + decompress remainder
    where (chars, inInstruction) = span isDigit t
          (times, closeInstruction) = span isDigit $ drop 1 inInstruction
          rest = drop 1 closeInstruction
          numChars = asInt chars
          numTimes = asInt times
          (repeater, remainder) = splitAt numChars rest
          repeats = numTimes * decompress repeater

main = do input <- readFile "../input"
          putStrLn "Decompressed Length"
          print $ decompress $ strip input
