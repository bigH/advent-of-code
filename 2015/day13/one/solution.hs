import Data.List
import Data.String.Utils

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)

asInt :: String -> Int
asInt s = read s :: Int

parse :: String -> (String, String, Int)
parse s = case split " " $ init s of
            (a : _ : "gain" : x : _ : _ : _ : _ : _ : _ : b : []) -> (a, b, asInt x)
            (a : _ : "lose" : x : _ : _ : _ : _ : _ : _ : b : []) -> (a, b, negate $ asInt x)
            (_) -> error $ "Unexpected input \"" ++ s ++ "\""

rotate :: Int -> [a] -> [a]
rotate n list = chopped ++ shifted
  where (shifted, chopped) = splitAt (len - bound n len) list
        len = length list

bound i j
  | (i > 0) && (i < j) = i
  | i < 0              = bound (i + j) j
  | i >= j             = bound (i - j) j

first (a, _, _) = a
second (_, a, _) = a
third (_, _, a) = a

pairings :: [String] -> [(String, String)]
pairings people = (zip people $ rotate 1 people) ++ (zip people $ rotate (negate 1) people)

mapTo :: (a -> b) -> a -> (a, b)
mapTo f k = (k, f k)

score :: [(String, String, Int)] -> (String, String) -> Int
score input pair = third $ head $ filter (matches pair) input

matches (a, b) = ((== a) . first) .&&. ((== b) . second)

best :: [(String, String, Int)] -> ([String], Int)
best input = head $ reverse $ sortOn snd $ scores
  where originalPeople = map head $ group $ sort $ map first input
        people = originalPeople ++ ["Me"]
        addingMe = input ++ concatMap scoresForMe originalPeople
        totalScore = sum . map (score addingMe) . pairings
        scores = map (mapTo totalScore) $ permutations people
        scoresForMe p = [("Me", p, 0), (p, "Me", 0)]

main = do input <- readFile "../input"
          putStrLn "Best Net Happiness Change:"
          print $ snd $ best $ map parse $ lines input
