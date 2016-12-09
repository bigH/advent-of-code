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

distance :: Int -> Reindeer -> Int
distance t r @ (Reindeer name speed sprintTime restTime)
  | t >= sprintTime + restTime = speed * sprintTime + distance (t - (sprintTime + restTime)) r
  | t >= sprintTime            = speed * sprintTime
  | otherwise                  = speed * t

mapTo :: (a -> b) -> a -> (a, b)
mapTo f k = (k, f k)

main = do putStrLn "Winner Distance:"
          print $ snd $ head $ reverse $ sortOn snd $ map (mapTo (distance 2503)) reindeer
