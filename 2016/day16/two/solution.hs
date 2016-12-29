import Data.List (find)
import Data.Maybe (fromJust)

data DiskData = Raw Int String           |
                Expanded Int DiskData    |
                Taken Int Int DiskData   |
                Reduced Int DiskData     deriving (Show)

flipBit :: Char -> Char
flipBit '0' = '1'
flipBit '1' = '0'
flipBit x   = error $ "unexpected char '" ++ [x] ++ "'"

lengthOf :: DiskData -> Int
lengthOf (Raw       l _) = l
lengthOf (Expanded  l _) = l
lengthOf (Taken   _ l _) = l
lengthOf (Reduced   l _) = l

raw s = Raw (length s) s

taken i s = Taken i (min i $ lengthOf s) s

expanded s = Expanded (1 + 2 * lengthOf s) s

reduced s
  | even $ lengthOf s = Reduced (lengthOf s `quot` 2) s
  | otherwise         = error $ "didn't expect s: " ++ show s

fill size seed = taken size $ until ((>= size) . lengthOf) expanded (raw seed)

checksum = until (odd . lengthOf) reduced

render :: DiskData -> String
render d = map (char d) [0 .. lengthOf d - 1]

char :: DiskData -> Int -> Char
char d @ (Raw       l s) ix
  | ix < 0 && ix >= l = raiseIt ix l d
  | otherwise         = s !! ix

char d @ (Expanded  l s) ix
  | ix < 0 && ix >= l = raiseIt ix l d
  | ix == len         = '0'
  | ix <  len         = char s ix
  | ix >  len         = flipBit $ char s (len - (ix - len))
    where len = lengthOf s

char d @ (Taken   i l s) ix
  | ix < 0 && ix >= l = raiseIt ix l d
  | otherwise         = char s ix

char d @ (Reduced   l s) ix
  | ix < 0 && ix >= l = raiseIt ix l d
  | otherwise         = reduceChars (char s (ix * 2))
                                    (char s (ix * 2 + 1))

reduceChars a b
  | a == b    = '1'
  | otherwise = '0'

raiseIt ix l d = error $ "Index " ++ show ix ++ " is out of bounds [0, " ++ show l ++ ") for " ++ show d

main = do putStrLn "Checksum of Expanded Seed:"
          print $ render $ checksum $ fill 35651584 "11011110011011101"
