import Data.List

data Item = G String | M String deriving (Eq, Show, Ord)
data Floor = Floor Int Item deriving (Eq, Show, Ord)

type State = (Int, [Floor])
type Path = [State]


-- 9 + 8 + 8 + 8 + 3 + (6 + 6 + 6 + 3)

-- E GA    GB    GC    GD    GE ME
--
--
--      MA    MB    MC    MD

-- starting = (1, sortOn item [Floor 1 (M "A"),
                            -- Floor 1 (M "B"),
                            -- Floor 2 (G "A"),
                            -- Floor 3 (G "B")])

starting = (1, sortOn item [Floor 1 (G "A"),
                            Floor 1 (M "A"),
                            Floor 1 (G "X"),
                            Floor 1 (M "X"),
                            Floor 1 (G "Y"),
                            Floor 1 (M "Y"),
                            Floor 2 (G "B"),
                            Floor 2 (G "C"),
                            Floor 2 (G "D"),
                            Floor 2 (G "E"),
                            Floor 3 (M "B"),
                            Floor 3 (M "C"),
                            Floor 3 (M "D"),
                            Floor 3 (M "E")])

without :: Eq a => [a] -> [a] -> [a]
without bs as = filter (not . (`elem` bs)) as

distinctBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
distinctBy f = map head . groupOn f . sortOn f

up :: Floor -> Floor
up (Floor x y) = (Floor (x + 1) y)

down :: Floor -> Floor
down (Floor x y) = (Floor (x - 1) y)

isGenerator :: Item -> Bool
isGenerator (G _) = True
isGenerator _             = False

isMicrochip :: Item -> Bool
isMicrochip (M _) = True
isMicrochip _             = False

name :: Item -> String
name (M n) = n
name (G n) = n

item :: Floor -> Item
item (Floor _ item) = item

num :: Floor -> Int
num (Floor n _) = n

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> f a == f b)

nothingFried :: [Floor] -> Bool
nothingFried state = all chipsWithGenerator $ map (map item) $ groupOn num $ sortOn num state
  where chipsWithGenerator f = let (chips, generators) = partition isMicrochip f
                                   names = map name
                               in null generators || (all (`elem` (names generators)) $ names chips)

valid :: State -> Bool
valid (e, fs) = (e > 0) &&
                (e <= 4) &&
                (any ((== e) . num) fs) &&
                (nothingFried fs)

-- score :: State -> Double
-- score = 

goal :: State -> Bool
goal = all ((== 4) . num) . snd

distinct :: (Ord a, Eq a) => [a] -> [a]
distinct = map head . group . sort

combine :: Eq a => a -> a -> [a]
combine a b
  | a == b    = [a]
  | otherwise = [a, b]

combos :: (Eq a, Ord a) => [a] -> [[a]]
combos as = distinct $ map sort $ concatMap (\i -> map (\j -> combine i j) as) as

nextsForAll :: [State] -> [Path] -> [Path]
nextsForAll visited [] = []
nextsForAll visited (h:t) = distinctBy head $ paths ++ nextsForAll newVisited t
  where paths = nexts visited h
        newVisited = map head paths

nexts :: [State] -> Path -> [Path]
nexts visited path = map (: path) pruned
  where (elevator, state) = head path
        (onFloor, _) = partition ((== elevator) . num) state
        combinations = combos onFloor
        remaining new = without new state
        upFloor new = (elevator + 1, sortOn item $ map up new ++ remaining new)
        downFloor new = (elevator - 1, sortOn item $ map down new ++ remaining new)
        upMoves = map upFloor combinations
        downMoves = map downFloor combinations
        calculated = filter valid $ upMoves ++ downMoves
        newNodes = without visited calculated
        pruned = pruneState visited newNodes

pruneState :: [State] -> [State] -> [State]
pruneState visited [] = []
pruneState visited (h:t) = if shouldPrune then pruneState visited t
                                          else h : pruneState visited t
  where shouldPrune = any (equivalent h) visited ||
                      any (equivalent h) t

nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty _  = True

paths :: Int -> ([String], [[State]])
paths i = pathsFrom [starting] [[starting]] i

pathsFrom :: [State] -> [Path] -> Int -> ([String], [[State]])
pathsFrom _ pathsSoFar 0 = (["** RAN OUT OF ITERATIONS !!!"], pathsSoFar)
pathsFrom visited pathsSoFar i =
  if nonEmpty reached
    then (["** REACHED !!!"], reached)
    else (message : messages, final)
      where (reached, rest) = partition (goal . head) pathsSoFar
            newPathsSoFar = nextsForAll visited pathsSoFar
            len = show $ (length $ head newPathsSoFar) - 1
            pruned = prune visited newPathsSoFar
            newlyVisited = visited ++ (without visited $ map head pruned)
            message = " -- length = " ++ len ++ " --> " ++
                                         show (length newPathsSoFar) ++ " paths; " ++
                                         show (length pruned) ++ " pruned; " ++
                                         show (length newlyVisited) ++ " visited"
            (messages, final) = pathsFrom newlyVisited pruned (i - 1)

prune :: [State] -> [Path] -> [Path]
prune visited [] = []
prune visited (h:t) = if shouldPrune then prune visited t
                                           else h : prune visited t
  where shouldPrune = any (equivalent $ head h) visited ||
                      any (equivalent $ head h) (map head t)

equivalent :: State -> State -> Bool
equivalent (ea, fsa) (eb, fsb) = ea == eb && floorsSimilar
  where floorsSimilar = all similar (ga `zip` gb)
        ga = orderedByOrientations $ groupedByElement fsa
        gb = orderedByOrientations $ groupedByElement fsb
        similar = tuplize $ (==) `on` orientation

tuplize :: (x -> y -> z) -> (x, y) -> z
tuplize f (a, b) = f a b

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on r f a b = r (f a) (f b)

orderedByOrientations :: [[Floor]] -> [[Floor]]
orderedByOrientations = sortOn orientation

orientation :: [Floor] -> (Int, Int)
orientation (Floor mf (M _) : Floor gf (G _) : []) = (mf, gf)
orientation (Floor gf (G _) : Floor mf (M _) : []) = (mf, gf)
orientation floors = error $ "Unexpected floor array " ++ show floors

groupedByElement :: [Floor] -> [[Floor]]
groupedByElement = groupOn element . sortOn element

element :: Floor -> String
element (Floor _ (M n)) = n
element (Floor _ (G n)) = n

display :: State -> String
display (e, s) = "\n\n" ++ floorToString 4 ++ "\n" ++
                           floorToString 3 ++ "\n" ++
                           floorToString 2 ++ "\n" ++
                           floorToString 1 ++ "\n"
  where floorToString f = show f ++ " ---> " ++ elevatorToString f e ++ concatMap (itemToString f) forDisplay
        elevatorToString f e | f == e    = "E   "
                             | otherwise = "    "
        itemToString f (Floor i (M n)) | f == i    = "M" ++ n ++ "  "
                                       | otherwise = "    "
        itemToString f (Floor i (G n)) | f == i    = "G" ++ n ++ "  "
                                       | otherwise = "    "
        forDisplay = sortOn displaySort s

displaySort (Floor _ (M n)) = show n ++ "M"
displaySort (Floor _ (G n)) = show n ++ "G"

main = do let (msgs, p) = paths 100
          mapM_ putStrLn $ msgs
          putStrLn ""
          putStrLn "Minimum Moves to Goal:"
          print $ (length $ head $ p) - 1
