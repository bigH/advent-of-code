import Data.String.Utils
import Data.List

data Pixel = W | O deriving (Eq, Show)
data Axis = X | Y deriving (Eq, Show)
data Instruction = Rect Int Int    |
                   Rotate Axis Int Int deriving (Eq, Show)

asInt :: String -> Int
asInt s = read s :: Int

count :: Pixel -> Int
count W = 1
count O = 0

toDisplay :: Pixel -> Char
toDisplay W = 'X'
toDisplay O = ' '

rotate :: Int -> [a] -> [a]
rotate n list = chopped ++ shifted
  where (shifted, chopped) = splitAt (length list - (n `rem` length list)) list

parse :: String -> Instruction
parse s = case split " " s of
            ("rotate" : "row"    : ('y':'=':a) : "by" : b : []) -> Rotate Y (asInt a) (asInt b)
            ("rotate" : "column" : ('x':'=':a) : "by" : b : []) -> Rotate X (asInt a) (asInt b)
            ("rect"   : xAndY                             : []) -> case split "x" xAndY of
                                                                     (x : y : []) -> Rect (asInt x) (asInt y)
                                                                     _            -> error $ "unexpected x and y: " ++ xAndY
            _ -> error $ "unexpected input: " ++ s

apply :: [[Pixel]] -> Instruction -> [[Pixel]]
apply pixels (Rect     a b) = newTop ++ bottom
  where (top, bottom) = splitAt b pixels
        newTop = map ((replicate a W ++) . drop a) top
apply pixels (Rotate X a b) = transpose $ apply (transpose pixels) $ Rotate Y a b
apply pixels (Rotate Y a b) = begin ++ [transformed] ++ end
  where (begin, row, end) = breakAround a pixels
        transformed = rotate b row

breakAround :: Int -> [a] -> ([a], a, [a])
breakAround a xs = (begin, middle, end)
  where (begin, rest) = splitAt a xs
        middle = head rest
        end = tail rest

display :: String -> [[Pixel]]
display input = foldl apply start commands
  where commands = map parse $ lines input

start = replicate 6 $ replicate 50 O

main = do input <- readFile "../input"
          putStrLn "Display:"
          mapM_ putStrLn $ map (map toDisplay) $ display input
