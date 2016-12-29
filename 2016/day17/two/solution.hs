import Data.Hash.MD5
import Data.List (partition)

passcode = "qljzarfv"

start = (0, 0)
goal = (3, 3)

type Coordinate = (Int, Int)
type Direction = Char
type Path = [Direction]

valid (x, y) = (x >= 0) &&
               (y >= 0) &&
               (x <= 3) &&
               (y <= 3)

hashed :: String -> String
hashed = md5s . Str

move :: Coordinate -> Direction -> Coordinate
move (a, b) 'L' = (a - 1, b)
move (a, b) 'R' = (a + 1, b)
move (a, b) 'U' = (a, b - 1)
move (a, b) 'D' = (a, b + 1)

(+-) :: [a] -> a -> [a]
as +- a = as ++ [a]

nexts :: [Path] -> [Path]
nexts []            = []
nexts (path : rest) = next path ++ nexts rest

next :: Path -> [Path]
next path = map (path +-) choices
  where directions = "UDLR"
        location = travel path
        hash = hashed (passcode ++ path)
        zipped = directions `zip` hash
        valids = filter (valid . move location . fst) zipped
        choices = map fst $ filter ((`elem` opens) . snd) valids

opens = ['b' .. 'f']

reachers :: [Path] -> [Path]
reachers [] = []
reachers paths = good ++ reachers rest
  where news = nexts paths
        (good, rest) = partition reaching news

reaching = (== goal) . travel

travel :: Path -> Coordinate
travel = foldl move start

main = do putStrLn "Longest Reacher:"
          print $ length $ last $ reachers [""]
