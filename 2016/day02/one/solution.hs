data Heading = North | South | East | West deriving (Show)
data Coordinate = Coordinate Int Int deriving (Show, Eq)

input = ["LLLUDRDLLULDUDLLRLUDURULDURRRRLLURLDLDDDULLDDUDLRDLRDUURRDUUDLLRUUDDLULURDLRDUUDUDRURULLLLDRULDDLRDDRDLRDDLURLDDUDLLUUDLRDDDDLULUUURRDLUUDDLULLURRRDULLUDUDRDRDDRLDLLRLRDDDRDLULLUULDLLLRRDDUURUURDLLDRRDDLRULDLLDRLLUDRRDLUUULDLURLLDDURRUULLLLLRLUDLDDLLLURRRDUDULRULULDDLLDLRDDDUULRLRDUURLURRUDDURRUUDUDLDDLDULDDDDDULRULRDLRLDLRDLDDLUDRDUUDLDUDUDLLLRLUUDRUDDDRDRURRLLLDLUULDUULRDLULLUURLDRRRLDRLUDRRURLDULULDRUDDULLLDRDLLULUDDDDRDRULDULRRRRDDRULDLRDU"] ++
        ["DLDDRRDLLDUURRLULLLLRDRLUDURLDRRDURRRRUUDDUDRRDDDRRLDDLDDLURDLDRLUDULDUDUUDDDLLULRRLLUDULLLUULDDRDDUDUUDULURULULLDRLRUURDRDDLRRUDRUULLLLURRUDRDULDDRURRURLLLLLRLLLRLLUDUDLRDLULDUDLULLLUUDLLDDDUDUDLLRRDLRDLLLRRLRUDRDUDRURLUUURULLDDDDLLURDULURRLLLRLRRULLRRRLUUULLLLRLRDUURRDUURLLLDRDURRRULDDUDRRDLRLDLLRUDDLLUDRLLUUDRLLLLLLUDLURLLRUDRUUUULDLUDULLDDDDRLURUURDDDURRRLRLUDUUDURDDDDRRLRDLURDDLLULDRDDURLLURULUUUUURDUUULDRRLLURRRRRLDDUULLRULLDLDLDLRRRDRLDRUUD"] ++
        ["RLDRRRURULDLUDLDUDLLDUUURRDUDDURULLRRDDULUUDRRRULRUURRRLUUULRDRUDRRLLRLURDLDRDRDLLUDRUULRUDRUDDRURLRLURRDDRRURUUDRRDDRURURUDUUUDUDRRLRDRUUURLLUUUDLRUUDDRDDDDLDRLRDUDDULDDLRLLRURLLURLDDLDLDDULLDDUUURDLRUDUUDLDURDDRUULDRDDRDDDUUUDRDRDDRRDRRDLRDRURDUDDLUUUDULLUULULULRDRUUDDURURDRRRRLUDLDUDURLDRURDLLUUUDLRRDRRURDDULULURLDUDDLUDLDDLLRLDULLULULURUURLDULUDLLUUDLDDULDRRDDUULLUDLDLLRDRDURDDURDDURLDDURUURLLRURURUDDURRDRLRLDDUUDUULRDLLURRRRULURULDUDUDDUDDRLLLDLURDUURUURLUULRRLDLULDDRLDDUURULURUDRD"] ++
        ["URLDDRLLRRLDRLLRRURURURDDLRRRUUUURULRRUUDLUDRULLDLRUDDLULRUULDULURLLRLLUDDUDLURDRRRRLURULRURRURRULRRRULDLLDDLRLUDULUUUDDUDDRRDDDDUULRRLDRRULULRDUURRLDDRDULDURUDUDDLDLLURDDLDDRUDUUUDUUURDLDUDUUULLDLRDULRRRDLLURLDLLULRDDULULURLRLUULRLLLDDDUDLLDLURRRULRDUDDLULUDRUDDURULRLRUDDURLLURULLURDRULDUDLDULRRDLDURLUURRDDUDDUDRURUDDURRUUDURUULLLLDDRDDDDDULUUDDURRULLDRRLRRRRRDDRUUDDDURDRDRUDDUULDUDRRDRULUURLURLUDUDULDDRDULDLRUUDLLLRRLRDRDDUUULRDUDLUDLURRDUURDULDRLLDRDULDUDUULRLLDLRLDLUUDLRUULDUUULDLRLRLUULLLLRLRDUDRUUDURLDUDRRURLRUDRRLRDDLRDDLDDUDDDRLRLLRLUUURLURRRLULRLLDRLRDDRRDRL"] ++
        ["DLLLLLLRLRDRUDLRLLRLDLRURRUURLDLDDDDDUDUULLLLRRLRRDUUDUDLULLRRDULUDLLULURLRULURUULRLURDUDLUDULULUUURLRUDULURULRURULURLRLDRRRRLUDLLDULLDDLLULUURRULRDURDUUDDDURRUDLLLLRLDLUDDULLDUDDURURURRRRULDULULUDDUUDRLRLLLDLLLUUUURUDUUDLDLLRLRDDUULLUURLDDLRRDRLULDLULRULDLDURLULUURRRUDLLRDLUDDULRULULUDDURDLUUURDUUURDUDURLUUDRLUDRULUDDRRDLUUDLLLRDDDDDDLDURDDLDRDLUUDRULLUDRDLDULLULDDRUUDRRLRURRUULLRLRDUUURRDRRDULDDULUUDDURLULRLRURLLRRR"]

numpad = [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]]

lowerBound = 0
upperBound = 2

bound :: Int -> Int
bound x
  | x <= lowerBound = lowerBound
  | x >= upperBound = upperBound
  | otherwise       = x

correct :: Coordinate -> Coordinate
correct (Coordinate x y) =
  Coordinate (bound x) (bound y)

rawMove :: Heading -> Coordinate -> Coordinate
rawMove North (Coordinate x y) = Coordinate (x - 1) y
rawMove South (Coordinate x y) = Coordinate (x + 1) y
rawMove East (Coordinate x y) = Coordinate x (y + 1)
rawMove West (Coordinate x y) = Coordinate x (y - 1)

move :: Heading -> Coordinate -> Coordinate
move heading = correct . (rawMove heading)

initial = Coordinate 1 1

directionForLetter :: Char -> Heading
directionForLetter 'U' = North
directionForLetter 'D' = South
directionForLetter 'L' = West
directionForLetter 'R' = East

nextPosition :: Coordinate -> Char -> Coordinate
nextPosition coord char = move heading coord
  where heading = directionForLetter char

digit :: Coordinate -> [Char] -> Coordinate
digit coord chars = foldl nextPosition coord chars

digits :: Coordinate -> [[Char]] -> [Coordinate]
digits coord strings = tail $ scanl digit coord strings

toDigit :: Coordinate -> String
toDigit (Coordinate x y) = show $ numpad !! x !! y

code :: [Coordinate] -> String
code coords = foldr (++) "" $ map toDigit coords

main = do putStrLn "Digits:"
          print $ code $ digits initial input