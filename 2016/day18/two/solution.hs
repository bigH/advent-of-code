import Data.List

input = "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"

data Tile = X | O deriving (Show, Eq)
type Row = [Tile]

start = map decode input

decode '.' = O
decode '^' = X

sliding :: Int -> [a] -> [[a]]
sliding x as
  | length as <= x = [as]
  | otherwise      = take x as : sliding x (tail as)

calculate :: [Tile] -> Tile
calculate (X : X : O : []) = X
calculate (O : X : X : []) = X
calculate (X : O : O : []) = X
calculate (O : O : X : []) = X
calculate (a : b : c : []) = O
calculate tiles = error $ "unexpected; as = " ++ concatMap show tiles

next :: Row -> Row
next row = map calculate slider
  where padded = [O] ++ row ++ [O]
        slider = sliding 3 padded

countSafe :: Tile -> Int
countSafe X = 0
countSafe O = 1

main = do putStrLn "Total Traps in 400000 Rows:"
          print $ sum $ map sum $ map (map countSafe) $ take 400000 $ iterate next start
