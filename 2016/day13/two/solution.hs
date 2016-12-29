import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

data Direction = N | S | E | W

type Coordinate = (Int, Int)
type Path = [Coordinate]

goal = (31, 39)

zero = (1, 1)

space :: Coordinate -> Bool
space (x, y) = even $ bits $ x*x + 3*x + 2*x*y + y + y*y + 1358

bits :: Int -> Int
bits i = length $ filter (== '1') $ showIntAtBase 2 intToDigit i ""

visits :: Int -> [Coordinate] -> [Path] -> [Coordinate]
visits n visited possibilities = if (length $ head possibilities) == n
                                   then visited
                                   else visits n newlyVisited newPaths
                                     where newPaths = mergePaths $ map (nexts visited) possibilities
                                           newlyVisited = visited ++ map head newPaths

mergePaths :: [[Path]] -> [Path]
mergePaths [] = []
mergePaths (h:t) = filter (not . (headVisitedIn t)) h ++ mergePaths t

headVisitedIn :: [[Path]] -> Path -> Bool
headVisitedIn others (h:_) = any (any ((== h) . head)) others

reached :: Path -> Bool
reached []    = False
reached (h:_) = h == goal

nexts :: [Coordinate] -> Path -> [Path]
nexts visited path = map (: path) coords
  where coords = next visited (head path)

bounds :: Coordinate -> Bool
bounds (x, y) | x < 0 = False
              | y < 0 = False
              | otherwise = True

next :: [Coordinate] -> Coordinate -> [Coordinate]
next visited (x, y) = filter (not . (`elem` visited)) $
                      filter bounds $
                      filter space [(x + 1, y),
                                    (x - 1, y),
                                    (x, y + 1),
                                    (x, y - 1)]

main = do putStrLn "Total Visited Points in Paths of `length` 50:"
          print $ (length $ visits 51 [zero] [[zero]])
