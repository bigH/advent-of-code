import Data.List

data Reindeer = Reindeer String Int Int Int deriving (Eq, Show)

reindeer = [Reindeer "Vixen" 19 7 124,
            Reindeer "Rudolph" 3 15 28,
            Reindeer "Donner" 19 9 164,
            Reindeer "Blitzen" 19 9 158,
            Reindeer "Comet" 13 7 82,
            Reindeer "Cupid" 25 6 145,
            Reindeer "Dasher" 14 3 38,
            Reindeer "Dancer" 3 16 37,
            Reindeer "Prancer" 25 6 143]

tupled :: (a -> b) -> (a -> c) -> a -> (b, c)
tupled f g a = (f a, g a)

first  (a, _, _, _) = a
second (_, a, _, _) = a
third  (_, _, a, _) = a
fourth (_, _, _, a) = a

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> f a == f b)

points :: Int -> [(Reindeer, Either Int Int, Int, Int)] -> [(Reindeer, Either Int Int, Int, Int)]
points 0 state = state
points n state = points (n - 1) $ increment winners updated
  where updated = map tick state
        winners = map first $ last $ groupOn third $ sortOn third $ updated

tick (r @ (Reindeer _ speed sprintTime restTime), Left  n, d, s)
  | n >= sprintTime = (r, Right       1, d        , s)
  | otherwise       = (r, Left  (n + 1), d + speed, s)
tick (r @ (Reindeer _ speed sprintTime restTime), Right n, d, s)
  | n >= restTime   = (r, Left        1, d + speed, s)
  | otherwise       = (r, Right (n + 1), d        , s)

increment :: [Reindeer] -> [(Reindeer, Either Int Int, Int, Int)] -> [(Reindeer, Either Int Int, Int, Int)]
increment r s = remaining ++ map (updateFourth (+ 1)) matching
  where (matching, remaining) = partition ((`elem` r) . first) s

updateFourth f (a, b, c, d) = (a, b, c, f d)

finalPoints :: [Reindeer] -> [(Reindeer, Int)]
finalPoints reindeer = map (tupled first fourth) $ points 2503 state
  where state = map start reindeer

start r = (r, Left 0, 0, 0)

main = do putStrLn "Winner Points:"
          print $ snd $ head $ reverse $ sortOn snd $ finalPoints reindeer
