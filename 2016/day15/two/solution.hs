import Data.List

input :: [(Int, Int)]
-- input = [(5, 4),
--          (2, 1)]
input = [(13, 10),
         (17, 15),
         (19, 17),
         ( 7,  1),
         ( 5,  0),
         ( 3,  1),
         (11,  0)]

nums :: [Int]
nums = [1 ..]

disk :: Int -> (Int, Int) -> [Int]
disk i (a, b) = iterate (+ a) (a - (b + i))

tupled :: (a -> b -> c) -> (a, b) -> c
tupled f (a, b) = f a b

indexed :: [(Int, (Int, Int))]
indexed = nums `zip` input

times :: [[Int]]
times = map (tupled disk) indexed

heads :: [[a]] -> [a]
heads = map head

headsEqual :: Eq a => [[a]] -> Bool
headsEqual = (== 1) . length . group . map head

answer :: [[Int]] -> Int
answer t = if headsEqual t then head $ head t
                           else answer remaining
  where maxHead = maximum $ heads t
        remaining = map (dropWhile (< maxHead)) t

main = do putStrLn "First Time to Throw:"
          print $ answer times
