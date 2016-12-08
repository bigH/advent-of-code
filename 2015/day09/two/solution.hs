import Data.String.Utils
import Data.Maybe
import Data.List

parse :: String -> [(String, String, Int)]
parse = takeTokens . split " "

takeTokens :: [String] -> [(String, String, Int)]
takeTokens (f:_:t:_:d:[]) =
  let dist = read d :: Int
  in [ (f, t, dist),
       (t, f, dist) ]

mapTo :: (a -> b) -> a -> (a, b)
mapTo f k = (k, f k)

first (a, _, _) = a
third (_, _, a) = a

hop :: [(String, String, Int)] -> [String] -> Int
hop distances (f:t:[]) = third $ fromJust $ find (matchingHop f t) distances

matchingHop :: String -> String -> (String, String, Int) -> Bool
matchingHop f1 t1 (f, t, _) = (t == t1) && (f == f1)

sliding :: Int -> [a] -> [[a]]
sliding x as
  | length as <= x = [as]
  | otherwise      = take x as : sliding x (tail as)

distance :: [(String, String, Int)] -> [String] -> ([String], Int)
distance distances = mapTo $ sum . map (hop distances) . sliding 2

shortestPath :: [String] -> ([String], Int)
shortestPath inputs = head $ reverse $ sortOn snd $ map (distance distances) $ permutations cities
  where distances = concatMap parse inputs
        cities = map head $ group $ sort $ map first distances

main = do distances <- readFile "../input"
          putStrLn "Shortest Path:"
          print $ snd $ shortestPath $ lines distances
