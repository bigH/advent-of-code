import Data.Maybe
import Data.List
import Data.String.Utils

data Instruction = SwapPositions Int Int |
                   SwapLetters Char Char |
                   RotateLeft Int        |
                   RotateRight Int       |
                   RotateOnLetter Char   |
                   Reverse Int Int       |
                   Move Int Int          deriving Show

seed = "abcdefgh"

parse :: String -> Instruction
parse = build . split " "

asInt :: String -> Int
asInt = read

infixr 0 &
(&) :: a -> (a -> b) -> b
x & y = y x

run :: String -> Instruction -> String
run s (SwapPositions  x y)
  | x < y     = take x s ++ [s !! y] ++ take (y - x - 1) (drop (x + 1) s) ++ [s !! x] ++ drop (y + 1) s
  | otherwise = take y s ++ [s !! x] ++ take (x - y - 1) (drop (y + 1) s) ++ [s !! y] ++ drop (x + 1) s
run s (SwapLetters    x y) = run s $ SwapPositions xPos yPos
                               where xPos = fromJust $ findIndex (== x) s
                                     yPos = fromJust $ findIndex (== y) s
run s (RotateLeft       x) = drop y s ++ take y s
                               where y = x `rem` length s
run s (RotateRight      x) = drop y s ++ take y s
                               where y = (2 * len - x) `rem` len
                                     len = length s
run s (RotateOnLetter   x) = run s $ RotateRight $ amount & (+ 1) `when` (>= 5)
                               where amount = 1 + (fromJust $ findIndex (== x) s)
run s (Reverse        x y) = take x s ++ (reverse $ take (y - x + 1) $ drop x s) ++ drop (y + 1) s
run s (Move           x y)
  | x < y     = take x s ++ (take (y - x) $ drop (x + 1) s) ++ [s !! x] ++ drop (y + 1) s
  | otherwise = take y s ++ [s !! x] ++ (take (x - y) $ drop y s) ++ drop (x + 1) s

when :: (a -> a) -> (a -> Bool) -> a -> a
when f p a = if p a then f a else a

build :: [String] -> Instruction
build ("swap" : "position" : x : "with" : "position" : y : [])            = SwapPositions  (asInt x) (asInt y)
build ("swap" : "letter" : x : "with" : "letter" : y : [])                = SwapLetters    (head  x) (head  y)
build ("rotate" : "left" : x : _ : [])                                    = RotateLeft     (asInt x)
build ("rotate" : "right" : x : _ : [])                                   = RotateRight    (asInt x)
build ("rotate" : "based" : "on" : "position" : "of" : "letter" : x : []) = RotateOnLetter (head  x)
build ("reverse" : "positions" : x : "through" : y : [])                  = Reverse        (asInt x) (asInt y)
build ("move" : "position" : x : "to" : "position" : y : [])              = Move           (asInt x) (asInt y)

main = do input <- readFile "../input"
          putStrLn "Scrambled Password:"
          print $ foldl run seed $ map parse $ lines input
