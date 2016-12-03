import Data.Maybe

data Heading = North | South | East | West deriving (Show)
data Coordinate = Coordinate Int Int deriving (Show, Eq)

data Position = Position Heading Coordinate deriving (Show)

coordinate :: Position -> Coordinate
coordinate (Position _ coord) = coord

data Direction = R | L deriving (Show)

initial = Position North (Coordinate 0 0)

inputs = ["R3", "L5", "R2", "L1", "L2", "R5", "L2", "R2", "L2", "L2", "L1", "R2", "L2", "R4"] ++
         ["R4", "R1", "L2", "L3", "R3", "L1", "R2", "L2", "L4", "R4", "R5", "L3", "R3", "L3"] ++
         ["L3", "R4", "R5", "L3", "R3", "L5", "L1", "L2", "R2", "L1", "R3", "R1", "L1", "R187"] ++
         ["L1", "R2", "R47", "L5", "L1", "L2", "R4", "R3", "L3", "R3", "R4", "R1", "R3", "L1"] ++
         ["L4", "L1", "R2", "L1", "R4", "R5", "L1", "R77", "L5", "L4", "R3", "L2", "R4", "R5"] ++
         ["R5", "L2", "L2", "R2", "R5", "L2", "R194", "R5", "L2", "R4", "L5", "L4", "L2", "R5"] ++
         ["L3", "L2", "L5", "R5", "R2", "L3", "R3", "R1", "L4", "R2", "L1", "R5", "L1", "R5"] ++
         ["L1", "L1", "R3", "L1", "R5", "R2", "R5", "R5", "L4", "L5", "L5", "L5", "R3", "L2"] ++
         ["L5", "L4", "R3", "R1", "R1", "R4", "L2", "L4", "R5", "R5", "R4", "L2", "L2", "R5"] ++
         ["R5", "L5", "L2", "R4", "R4", "L4", "R1", "L3", "R1", "L1", "L1", "L1", "L4", "R5"] ++
         ["R4", "L4", "L4", "R5", "R3", "L2", "L2", "R3", "R1", "R4", "L3", "R1", "L4", "R3"] ++
         ["L3", "L2", "R2", "R2", "R2", "L1", "L4", "R3", "R2", "R2", "L3", "R2", "L3", "L2"] ++
         ["R4", "L2", "R3", "L4", "R5", "R4", "R1", "R5", "R3"]

findFinalDestination :: Position -> [[Char]] -> Position
findFinalDestination pos [] = pos
findFinalDestination (Position heading location) (head : tail) =
  findFinalDestination (Position newHeading newLocation) tail
    where instruction = parseInput head
          (direction, distance) = instruction
          newHeading = turn direction heading
          newLocation = move distance newHeading location

turn :: Direction -> Heading -> Heading
turn L = left
turn R = right

right :: Heading -> Heading
right North = East
right East = South
right South = West
right West = North

left :: Heading -> Heading
left South = East
left West = South
left North = West
left East = North

move :: Int -> Heading -> Coordinate -> Coordinate
move d heading coord = iterate (step heading) coord !! d

step :: Heading -> Coordinate -> Coordinate
step East (Coordinate x y) = Coordinate (x + 1) y
step West (Coordinate x y) = Coordinate (x - 1) y
step North (Coordinate x y) = Coordinate x (y + 1)
step South (Coordinate x y) = Coordinate x (y - 1)

parseInput :: [Char] -> (Direction, Int)
parseInput input = (direction, distance)
  where direction = asDirection $ take 1 input
        distance = asInt $ drop 1 input

asInt :: [Char] -> Int
asInt s = read s :: Int

asDirection :: [Char] -> Direction
asDirection "R" = R
asDirection "L" = L
asDirection d = error $ "Invalid Direction: `" ++ d ++ "`"

distance :: Coordinate -> Int
distance (Coordinate x y) = abs x + abs y

main = do putStrLn "Final Destination:"
          print $ distance $ coordinate $ findFinalDestination initial inputs
