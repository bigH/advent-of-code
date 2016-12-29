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

paths :: [Coordinate] -> [Path] -> [Path]
paths visited possibilities | any reached possibilities = filter reached possibilities
                            | otherwise                 = paths newlyVisited newPaths
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

next :: [Coordinate] -> Coordinate -> [Coordinate]
next visited (x, y) = filter (not . (`elem` visited)) $
                      filter space [(x + 1, y),
                                    (x - 1, y),
                                    (x, y + 1),
                                    (x, y - 1)]

main = do putStrLn "Length of Shortest Path:"
          print $ (length $ head $ paths [zero] [[zero]]) - 1
